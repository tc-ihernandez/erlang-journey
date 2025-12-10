-module(todo_handler).
-export([handle/2, handle_event/3]).

%%%===================================================================
%%% Elli Handler Callbacks
%%%===================================================================

%% @doc Main Elli handler callback
handle(Req, _Args) ->
    % Get client IP for rate limiting
    IP = get_client_ip(Req),
    
    % Check rate limit
    case rate_limiter:check_rate_limit(IP) of
        {ok, Remaining} ->
            Method = elli_request:method(Req),
            Path = elli_request:path(Req),
            Response = handle_request(Method, Path, Req),
            add_rate_limit_headers(Response, IP, Remaining);
        {error, rate_limit_exceeded} ->
            {Count, Limit, ResetTime} = rate_limiter:get_limit_info(IP),
            RetryAfter = max(0, (ResetTime - erlang:system_time(millisecond)) div 1000),
            json_response_with_rate_limit(429, 
                #{<<"error">> => <<"Rate limit exceeded">>, 
                  <<"retry_after">> => RetryAfter},
                Count, Limit, ResetTime)
    end.

%% @doc Handle elli events (optional)
handle_event(_Event, _Data, _Args) ->
    ok.

%%%===================================================================
%%% Request Routing
%%%===================================================================

%% List all todos: GET /todos with optional query parameters
%% Supports: ?completed=true/false, ?page=1, ?limit=10, ?search=keyword, ?tags=tag1,tag2
handle_request('GET', [<<"todos">>], Req) ->
    CompletedFilter = elli_request:get_arg(<<"completed">>, Req, undefined),
    PageParam = elli_request:get_arg(<<"page">>, Req, undefined),
    LimitParam = elli_request:get_arg(<<"limit">>, Req, undefined),
    SearchParam = elli_request:get_arg(<<"search">>, Req, undefined),
    TagsParam = elli_request:get_arg(<<"tags">>, Req, undefined),
    
    % Parse pagination parameters
    {UsePagination, Page, Limit} = case {PageParam, LimitParam} of
        {undefined, undefined} -> {false, 1, 10};
        {P, L} when P /= undefined; L /= undefined ->
            ParsedPage = parse_int(P, 1),
            ParsedLimit = parse_int(L, 10),
            {true, ParsedPage, ParsedLimit};
        _ -> {false, 1, 10}
    end,
    
    % Parse tags parameter
    Tags = case TagsParam of
        undefined -> undefined;
        TagsBin -> binary:split(TagsBin, <<",">>, [global])
    end,
    
    % Get todos based on filters and pagination
    Result = case {CompletedFilter, SearchParam, Tags, UsePagination} of
        {undefined, undefined, undefined, false} ->
            % No filters, no pagination
            todo_db:read_all();
        {undefined, undefined, undefined, true} ->
            % Only pagination
            todo_db:read_all_paginated(Page, Limit);
        {<<"true">>, undefined, undefined, _} ->
            % Filter by completed
            todo_db:read_by_status(true);
        {<<"false">>, undefined, undefined, _} ->
            % Filter by pending
            todo_db:read_by_status(false);
        {undefined, Search, undefined, _} when Search /= undefined ->
            % Search
            {ok, search_todos(Search)};
        {undefined, undefined, TagList, _} when TagList /= undefined ->
            % Filter by tags
            todo_db:read_by_tags(TagList);
        {_, _, _, _} ->
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

%% Delete todo: DELETE /todos/:id (soft delete) or DELETE /todos/:id?permanent=true
handle_request('DELETE', [<<"todos">>, IdBin], Req) ->
    Permanent = elli_request:get_arg(<<"permanent">>, Req, <<"false">>),
    
    try binary_to_integer(IdBin) of
        Id ->
            Result = case Permanent of
                <<"true">> -> todo_db:delete_permanent(Id);
                _ -> todo_db:delete(Id)
            end,
            
            case Result of
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

%% Restore soft-deleted todo: POST /todos/:id/restore
handle_request('POST', [<<"todos">>, IdBin, <<"restore">>], _Req) ->
    try binary_to_integer(IdBin) of
        Id ->
            case todo_db:restore(Id) of
                {ok, RestoredTodo} ->
                    json_response(200, RestoredTodo);
                {error, not_found} ->
                    json_response(404, #{<<"error">> => <<"Todo not found">>});
                {error, not_deleted} ->
                    json_response(400, #{<<"error">> => <<"Todo is not deleted">>});
                {error, Reason} ->
                    json_response(500, #{<<"error">> => format_error(Reason)})
            end
    catch
        _:_ ->
            json_response(400, #{<<"error">> => <<"Invalid todo ID">>})
    end;

%% Bulk complete: POST /todos/bulk-complete
handle_request('POST', [<<"todos">>, <<"bulk-complete">>], Req) ->
    Body = elli_request:body(Req),
    
    case parse_json(Body) of
        {ok, Data} ->
            case maps:get(<<"ids">>, Data, undefined) of
                undefined ->
                    json_response(400, #{<<"error">> => <<"Missing 'ids' array">>});
                Ids when is_list(Ids) ->
                    case todo_db:bulk_complete(Ids) of
                        {ok, SuccessCount} ->
                            json_response(200, #{
                                <<"message">> => <<"Bulk complete successful">>,
                                <<"updated_count">> => SuccessCount
                            });
                        {error, Reason} ->
                            json_response(500, #{<<"error">> => format_error(Reason)})
                    end;
                _ ->
                    json_response(400, #{<<"error">> => <<"Invalid 'ids' format. Expected array of integers">>})
            end;
        {error, _ParseError} ->
            json_response(400, #{<<"error">> => <<"Invalid JSON">>})
    end;

%% Bulk delete: POST /todos/bulk-delete
handle_request('POST', [<<"todos">>, <<"bulk-delete">>], Req) ->
    Body = elli_request:body(Req),
    
    case parse_json(Body) of
        {ok, Data} ->
            case maps:get(<<"ids">>, Data, undefined) of
                undefined ->
                    json_response(400, #{<<"error">> => <<"Missing 'ids' array">>});
                Ids when is_list(Ids) ->
                    case todo_db:bulk_delete(Ids) of
                        {ok, SuccessCount} ->
                            json_response(200, #{
                                <<"message">> => <<"Bulk delete successful">>,
                                <<"deleted_count">> => SuccessCount
                            });
                        {error, Reason} ->
                            json_response(500, #{<<"error">> => format_error(Reason)})
                    end;
                _ ->
                    json_response(400, #{<<"error">> => <<"Invalid 'ids' format. Expected array of integers">>})
            end;
        {error, _ParseError} ->
            json_response(400, #{<<"error">> => <<"Invalid JSON">>})
    end;

%% Health check endpoint: GET /health
handle_request('GET', [<<"health">>], _Req) ->
    json_response(200, #{<<"status">> => <<"ok">>});

%% Statistics endpoint: GET /api/stats
handle_request('GET', [<<"api">>, <<"stats">>], _Req) ->
    case todo_db:get_statistics() of
        {ok, Stats} ->
            json_response(200, Stats);
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)})
    end;

%% Tags endpoint: GET /tags
handle_request('GET', [<<"tags">>], _Req) ->
    case todo_db:get_all_tags() of
        {ok, Tags} ->
            json_response(200, #{<<"tags">> => Tags});
        {error, Reason} ->
            json_response(500, #{<<"error">> => format_error(Reason)})
    end;

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

%% @doc Get client IP from request
get_client_ip(Req) ->
    case elli_request:get_header(<<"X-Forwarded-For">>, Req) of
        undefined ->
            try
                {Peer, _Port} = elli_request:peer(Req),
                list_to_binary(inet:ntoa(Peer))
            catch
                _:_ -> <<"127.0.0.1">>
            end;
        ForwardedFor ->
            % Take first IP from X-Forwarded-For header
            hd(binary:split(ForwardedFor, <<",">>))
    end.

%% @doc Add rate limit headers to response
add_rate_limit_headers({StatusCode, Headers, Body}, IP, _Remaining) ->
    {Count, Limit, ResetTime} = rate_limiter:get_limit_info(IP),
    Remaining = Limit - Count,
    ResetTimeSeconds = ResetTime div 1000,
    
    RateLimitHeaders = [
        {<<"X-RateLimit-Limit">>, integer_to_binary(Limit)},
        {<<"X-RateLimit-Remaining">>, integer_to_binary(Remaining)},
        {<<"X-RateLimit-Reset">>, integer_to_binary(ResetTimeSeconds)}
    ],
    
    {StatusCode, Headers ++ RateLimitHeaders, Body}.

%% @doc Create JSON response with rate limit headers
json_response_with_rate_limit(StatusCode, Data, Count, Limit, ResetTime) ->
    Body = jsone:encode(Data),
    Remaining = Limit - Count,
    ResetTimeSeconds = ResetTime div 1000,
    RetryAfter = max(0, (ResetTime - erlang:system_time(millisecond)) div 1000),
    
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"Access-Control-Allow-Methods">>, <<"GET, POST, PUT, DELETE, OPTIONS">>},
        {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>},
        {<<"X-RateLimit-Limit">>, integer_to_binary(Limit)},
        {<<"X-RateLimit-Remaining">>, integer_to_binary(Remaining)},
        {<<"X-RateLimit-Reset">>, integer_to_binary(ResetTimeSeconds)},
        {<<"Retry-After">>, integer_to_binary(RetryAfter)}
    ],
    
    {StatusCode, Headers, Body}.

