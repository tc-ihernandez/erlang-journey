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

%% List all todos: GET /todos or GET /todos?completed=true/false
handle_request('GET', [<<"todos">>], Req) ->
    % Check for completed query parameter
    case elli_request:get_arg(<<"completed">>, Req, undefined) of
        undefined ->
            % No filter, return all
            case todo_db:read_all() of
                {ok, Todos} ->
                    json_response(200, #{<<"todos">> => Todos});
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)})
            end;
        <<"true">> ->
            % Filter by completed = true
            case todo_db:read_by_status(true) of
                {ok, Todos} ->
                    json_response(200, #{<<"todos">> => Todos});
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)})
            end;
        <<"false">> ->
            % Filter by completed = false
            case todo_db:read_by_status(false) of
                {ok, Todos} ->
                    json_response(200, #{<<"todos">> => Todos});
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)})
            end;
        _ ->
            % Invalid value for completed parameter
            json_response(400, #{<<"error">> => <<"Invalid 'completed' parameter. Use 'true' or 'false'">>})
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

