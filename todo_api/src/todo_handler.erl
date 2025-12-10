-module(todo_handler).
-export([handle/2, handle_event/3]).

%%%===================================================================
%%% Elli Handler Callbacks
%%%===================================================================

%% @doc Main Elli handler callback
handle(Req, _Args) ->
    Method = elli_request:method(Req),
    Path = elli_request:path(Req),
    
    handle_request(Method, Path, Req).

%% @doc Handle elli events (optional)
handle_event(_Event, _Data, _Args) ->
    ok.

%%%===================================================================
%%% Request Routing
%%%===================================================================

%% List all todos: GET /todos with optional query parameters
%% Supports: ?completed=true/false, ?page=1, ?limit=10, ?search=keyword
handle_request('GET', [<<"todos">>], Req) ->
    CompletedFilter = elli_request:get_arg(<<"completed">>, Req, undefined),
    PageParam = elli_request:get_arg(<<"page">>, Req, undefined),
    LimitParam = elli_request:get_arg(<<"limit">>, Req, undefined),
    SearchParam = elli_request:get_arg(<<"search">>, Req, undefined),
    
    % Parse pagination parameters
    {UsePagination, Page, Limit} = case {PageParam, LimitParam} of
        {undefined, undefined} -> {false, 1, 10};
        {P, L} when P /= undefined; L /= undefined ->
            ParsedPage = parse_int(P, 1),
            ParsedLimit = parse_int(L, 10),
            {true, ParsedPage, ParsedLimit};
        _ -> {false, 1, 10}
    end,
    
    % Get todos based on filters and pagination
    Result = case {CompletedFilter, SearchParam, UsePagination} of
        {undefined, undefined, false} ->
            % No filters, no pagination
            todo_db:read_all();
        {undefined, undefined, true} ->
            % Only pagination
            todo_db:read_all_paginated(Page, Limit);
        {<<"true">>, undefined, _} ->
            % Filter by completed
            todo_db:read_by_status(true);
        {<<"false">>, undefined, _} ->
            % Filter by pending
            todo_db:read_by_status(false);
        {undefined, Search, _} when Search /= undefined ->
            % Search (will implement next)
            {ok, search_todos(Search)};
        {_, _, _} ->
            % Invalid combination or completed with invalid value
            case CompletedFilter of
                undefined -> todo_db:read_all();
                <<"true">> -> todo_db:read_by_status(true);
                <<"false">> -> todo_db:read_by_status(false);
                _ -> {error, invalid_completed}
            end
    end,
    
    % Build response
    case Result of
        {ok, Todos} when UsePagination ->
            {ok, TotalCount} = todo_db:count_all(),
            TotalPages = ceil(TotalCount / Limit),
            json_response(200, #{
                <<"todos">> => Todos,
                <<"pagination">> => #{
                    <<"page">> => Page,
                    <<"limit">> => Limit,
                    <<"total">> => TotalCount,
                    <<"total_pages">> => TotalPages
                }
            });
        {ok, Todos} ->
            json_response(200, #{<<"todos">> => Todos});
        {error, invalid_completed} ->
            json_response(400, #{<<"error">> => <<"Invalid 'completed' parameter. Use 'true' or 'false'">>});
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)})
    end;

%% Get specific todo: GET /todos/:id
handle_request('GET', [<<"todos">>, IdBin], _Req) ->
    try binary_to_integer(IdBin) of
        Id ->
            case todo_db:read(Id) of
                {ok, Todo} ->
                    json_response(200, Todo);
                {error, not_found} ->
                    json_response(404, #{<<"error">> => <<"Todo not found">>})
            end
    catch
        _:_ ->
            json_response(400, #{<<"error">> => <<"Invalid todo ID">>})
    end;

%% Create new todo: POST /todos
handle_request('POST', [<<"todos">>], Req) ->
    Body = elli_request:body(Req),
    
    case parse_json(Body) of
        {ok, TodoData} ->
            case validate_create(TodoData) of
                ok ->
                    case todo_db:create(TodoData) of
                        {ok, Id} ->
                            case todo_db:read(Id) of
                                {ok, Todo} ->
                                    json_response(201, Todo);
                                _ ->
                                    json_response(500, #{<<"error">> => <<"Failed to retrieve created todo">>})
                            end;
                        {error, Reason} ->
                            json_response(500, #{<<"error">> => format_error(Reason)})
                    end;
                {error, ValidationError} ->
                    json_response(400, #{<<"error">> => ValidationError})
            end;
        {error, _ParseError} ->
            json_response(400, #{<<"error">> => <<"Invalid JSON">>})
    end;

%% Update todo: PUT /todos/:id
handle_request('PUT', [<<"todos">>, IdBin], Req) ->
    try binary_to_integer(IdBin) of
        Id ->
            Body = elli_request:body(Req),
            
            case parse_json(Body) of
                {ok, UpdateData} ->
                    case todo_db:update(Id, UpdateData) of
                        {ok, UpdatedTodo} ->
                            json_response(200, UpdatedTodo);
                        {error, not_found} ->
                            json_response(404, #{<<"error">> => <<"Todo not found">>});
                        {error, Reason} ->
                            json_response(500, #{<<"error">> => format_error(Reason)})
                    end;
                {error, _ParseError} ->
                    json_response(400, #{<<"error">> => <<"Invalid JSON">>})
            end
    catch
        _:_ ->
            json_response(400, #{<<"error">> => <<"Invalid todo ID">>})
    end;

%% Delete todo: DELETE /todos/:id
handle_request('DELETE', [<<"todos">>, IdBin], _Req) ->
    try binary_to_integer(IdBin) of
        Id ->
            case todo_db:delete(Id) of
                ok ->
                    json_response(204, #{});
                {error, not_found} ->
                    json_response(404, #{<<"error">> => <<"Todo not found">>});
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)})
            end
    catch
        _:_ ->
            json_response(400, #{<<"error">> => <<"Invalid todo ID">>})
    end;

%% Health check endpoint: GET /health
handle_request('GET', [<<"health">>], _Req) ->
    json_response(200, #{<<"status">> => <<"ok">>});

%% CORS preflight: OPTIONS
handle_request('OPTIONS', _Path, _Req) ->
    json_response(200, #{<<"message">> => <<"CORS preflight">>});

%% Not found for all other routes
handle_request(_Method, _Path, _Req) ->
    json_response(404, #{<<"error">> => <<"Route not found">>}).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Parse JSON body
parse_json(Body) ->
    try
        {ok, jsone:decode(Body)}
    catch
        _:_ ->
            {error, invalid_json}
    end.

%% @doc Validate create request data
validate_create(Data) when is_map(Data) ->
    case maps:get(<<"title">>, Data, undefined) of
        undefined ->
            {error, <<"Title is required">>};
        <<>> ->
            {error, <<"Title cannot be empty">>};
        _ ->
            ok
    end;
validate_create(_) ->
    {error, <<"Invalid data format">>}.

%% @doc Format error for response
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc Parse integer from binary, return default if invalid
parse_int(undefined, Default) ->
    Default;
parse_int(Binary, Default) when is_binary(Binary) ->
    try
        binary_to_integer(Binary)
    catch
        _:_ -> Default
    end;
parse_int(_, Default) ->
    Default.

%% @doc Search todos by keyword in title or description
search_todos(Keyword) ->
    {ok, AllTodos} = todo_db:read_all(),
    LowerKeyword = string:lowercase(Keyword),
    lists:filter(fun(Todo) ->
        Title = string:lowercase(maps:get(<<"title">>, Todo, <<>>)),
        Description = string:lowercase(maps:get(<<"description">>, Todo, <<>>)),
        string:find(Title, LowerKeyword) =/= nomatch orelse
        string:find(Description, LowerKeyword) =/= nomatch
    end, AllTodos).

%% @doc Create JSON response with CORS headers
json_response(StatusCode, Data) ->
    Body = case StatusCode of
        204 -> <<>>;  % No content for 204
        _ -> jsone:encode(Data)
    end,
    
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"Access-Control-Allow-Methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>},
        {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}
    ],
    
    {StatusCode, Headers, Body}.

