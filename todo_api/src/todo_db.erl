-module(todo_db).
-behaviour(gen_server).

%% API exports
-export([start_link/0, create/1, read/1, read_all/0, read_all_paginated/2, read_all_sorted/2, read_by_status/1, read_by_tags/1, update/2, delete/1, delete_permanent/1, restore/1, bulk_complete/1, bulk_delete/1, count_all/0, get_statistics/0, get_all_tags/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(todo, {
    id :: integer(),
    title :: binary(),
    description :: binary(),
    completed = false :: boolean(),
    tags = [] :: list(binary()),
    priority = <<"medium">> :: binary(), % low, medium, high, urgent
    due_date = null :: integer() | null,
    deleted = false :: boolean(),
    created_at :: integer(),
    updated_at :: integer()
}).

-define(SERVER, ?MODULE).
-define(TABLE, todo).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the database server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create a new todo item
%% Input: #{title => binary(), description => binary()}
%% Output: {ok, TodoId} | {error, Reason}
create(TodoData) ->
    gen_server:call(?SERVER, {create, TodoData}).

%% @doc Read a specific todo by ID
%% Output: {ok, Todo} | {error, not_found}
read(Id) ->
    gen_server:call(?SERVER, {read, Id}).

%% @doc Read all todos
%% Output: {ok, [Todo]}
read_all() ->
    gen_server:call(?SERVER, read_all).

%% @doc Read all todos with pagination
%% Input: Page (starting from 1), Limit (items per page)
%% Output: {ok, [Todo]}
read_all_paginated(Page, Limit) when is_integer(Page), is_integer(Limit), Page > 0, Limit > 0 ->
    gen_server:call(?SERVER, {read_all_paginated, Page, Limit}).

%% @doc Read all todos with sorting
%% Input: SortBy (binary), Order (binary)
%% Output: {ok, [Todo]}
read_all_sorted(SortBy, Order) ->
    gen_server:call(?SERVER, {read_all_sorted, SortBy, Order}).

%% @doc Count total number of todos
%% Output: {ok, Count}
count_all() ->
    gen_server:call(?SERVER, count_all).

%% @doc Get statistics about todos
%% Output: {ok, Statistics}
get_statistics() ->
    gen_server:call(?SERVER, get_statistics).

%% @doc Read todos filtered by completion status
%% Input: true | false
%% Output: {ok, [Todo]}
read_by_status(Completed) when is_boolean(Completed) ->
    gen_server:call(?SERVER, {read_by_status, Completed}).

%% @doc Read todos filtered by tags (any of the provided tags)
%% Input: [binary()]
%% Output: {ok, [Todo]}
read_by_tags(Tags) when is_list(Tags) ->
    gen_server:call(?SERVER, {read_by_tags, Tags}).

%% @doc Get all unique tags with usage count
%% Output: {ok, [{Tag, Count}]}
get_all_tags() ->
    gen_server:call(?SERVER, get_all_tags).

%% @doc Update a todo item
%% Input: Id, #{title => binary(), description => binary(), completed => boolean()}
%% Output: {ok, UpdatedTodo} | {error, Reason}
update(Id, UpdateData) ->
    gen_server:call(?SERVER, {update, Id, UpdateData}).

%% @doc Delete a todo item (soft delete)
%% Output: ok | {error, not_found}
delete(Id) ->
    gen_server:call(?SERVER, {delete, Id}).

%% @doc Permanently delete a todo item
%% Output: ok | {error, not_found}
delete_permanent(Id) ->
    gen_server:call(?SERVER, {delete_permanent, Id}).

%% @doc Restore a soft-deleted todo item
%% Output: {ok, Todo} | {error, not_found | not_deleted}
restore(Id) ->
    gen_server:call(?SERVER, {restore, Id}).

%% @doc Bulk complete multiple todos
%% Input: [Id]
%% Output: {ok, SuccessCount} | {error, Reason}
bulk_complete(Ids) when is_list(Ids) ->
    gen_server:call(?SERVER, {bulk_complete, Ids}).

%% @doc Bulk delete multiple todos (soft delete)
%% Input: [Id]
%% Output: {ok, SuccessCount} | {error, Reason}
bulk_delete(Ids) when is_list(Ids) ->
    gen_server:call(?SERVER, {bulk_delete, Ids}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Initialize Mnesia
    case init_mnesia() of
        ok ->
            {ok, #{}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({create, TodoData}, _From, State) ->
    Title = maps:get(<<"title">>, TodoData, <<>>),
    Description = maps:get(<<"description">>, TodoData, <<>>),
    Tags = maps:get(<<"tags">>, TodoData, []),
    Priority = maps:get(<<"priority">>, TodoData, <<"medium">>),
    DueDateInput = maps:get(<<"due_date">>, TodoData, null),
    
    % Parse due_date (accepts both timestamp and DD/MM/YYYY format)
    DueDate = parse_date_input(DueDateInput),
    
    case Title of
        <<>> ->
            {reply, {error, title_required}, State};
        _ ->
            Id = generate_id(),
            Timestamp = erlang:system_time(second),
            Todo = #todo{
                id = Id,
                title = Title,
                description = Description,
                completed = false,
                tags = Tags,
                priority = Priority,
                due_date = DueDate,
                created_at = Timestamp,
                updated_at = Timestamp
            },
            
            Result = mnesia:transaction(fun() ->
                mnesia:write(?TABLE, Todo, write),
                Todo
            end),
            
            case Result of
                {atomic, CreatedTodo} ->
                    % Log audit event (async)
                    spawn(fun() ->
                        audit_log:log_event(Id, <<"created">>, #{
                            <<"title">> => Title,
                            <<"description">> => Description,
                            <<"tags">> => Tags,
                            <<"priority">> => Priority,
                            <<"due_date">> => DueDate
                        }, <<"system">>)
                    end),
                    {reply, {ok, todo_to_map(CreatedTodo)}, State};
                {aborted, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({read, Id}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        mnesia:read(?TABLE, Id)
    end),
    
    case Result of
        {atomic, [Todo]} ->
            {reply, {ok, todo_to_map(Todo)}, State};
        {atomic, []} ->
            {reply, {error, not_found}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(read_all, _From, State) ->
    Result = mnesia:transaction(fun() ->
        mnesia:match_object(?TABLE, #todo{_ = '_'}, read)
    end),
    
    case Result of
        {atomic, Todos} ->
            TodoMaps = [todo_to_map(T) || T <- Todos],
            {reply, {ok, TodoMaps}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({read_by_status, Completed}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        mnesia:match_object(?TABLE, #todo{completed = Completed, _ = '_'}, read)
    end),
    
    case Result of
        {atomic, Todos} ->
            TodoMaps = [todo_to_map(T) || T <- Todos],
            {reply, {ok, TodoMaps}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({read_by_tags, Tags}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllTodos = mnesia:match_object(?TABLE, #todo{_ = '_'}, read),
        % Filter todos that have at least one of the requested tags
        lists:filter(fun(Todo) ->
            TodoTags = Todo#todo.tags,
            lists:any(fun(Tag) -> lists:member(Tag, TodoTags) end, Tags)
        end, AllTodos)
    end),
    
    case Result of
        {atomic, Todos} ->
            TodoMaps = [todo_to_map(T) || T <- Todos],
            {reply, {ok, TodoMaps}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_all_tags, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllTodos = mnesia:match_object(?TABLE, #todo{_ = '_'}, read),
        % Extract all tags and count occurrences
        AllTags = lists:flatten([T#todo.tags || T <- AllTodos]),
        % Count occurrences
        TagCounts = lists:foldl(fun(Tag, Acc) ->
            Count = maps:get(Tag, Acc, 0),
            maps:put(Tag, Count + 1, Acc)
        end, #{}, AllTags),
        % Convert to list of maps
        [#{<<"tag">> => Tag, <<"count">> => Count} || {Tag, Count} <- maps:to_list(TagCounts)]
    end),
    
    case Result of
        {atomic, Tags} ->
            % Sort by count descending
            SortedTags = lists:sort(fun(#{<<"count">> := C1}, #{<<"count">> := C2}) ->
                C1 >= C2
            end, Tags),
            {reply, {ok, SortedTags}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({read_all_paginated, Page, Limit}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllTodos = mnesia:match_object(?TABLE, #todo{_ = '_'}, read),
        % Filter out deleted
        ActiveTodos = [T || T <- AllTodos, T#todo.deleted =:= false],
        % Sort by ID (could be by created_at or other field)
        SortedTodos = lists:sort(fun(A, B) -> A#todo.id =< B#todo.id end, ActiveTodos),
        % Calculate offset
        Offset = (Page - 1) * Limit,
        % Slice the list
        paginate_list(SortedTodos, Offset, Limit)
    end),
    
    case Result of
        {atomic, Todos} ->
            TodoMaps = [todo_to_map(T) || T <- Todos],
            {reply, {ok, TodoMaps}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({read_all_sorted, SortBy, Order}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllTodos = mnesia:match_object(?TABLE, #todo{_ = '_'}, read),
        % Filter out deleted
        ActiveTodos = [T || T <- AllTodos, T#todo.deleted =:= false],
        % Sort by requested field
        Sorted = sort_todos(ActiveTodos, SortBy, Order),
        Sorted
    end),
    
    case Result of
        {atomic, Todos} ->
            TodoMaps = [todo_to_map(T) || T <- Todos],
            {reply, {ok, TodoMaps}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(count_all, _From, State) ->
    Result = mnesia:transaction(fun() ->
        mnesia:table_info(?TABLE, size)
    end),
    
    case Result of
        {atomic, Count} ->
            {reply, {ok, Count}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_statistics, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllTodos = mnesia:match_object(?TABLE, #todo{_ = '_'}, read),
        Now = erlang:system_time(second),
        TodayStart = Now - (Now rem 86400), % Start of today in UTC
        
        % Calculate statistics
        Total = length(AllTodos),
        Completed = length([T || T <- AllTodos, T#todo.completed =:= true]),
        Pending = Total - Completed,
        CompletionRate = case Total of
            0 -> 0.0;
            _ -> (Completed / Total) * 100
        end,
        
        CreatedToday = length([T || T <- AllTodos, T#todo.created_at >= TodayStart]),
        UpdatedToday = length([T || T <- AllTodos, T#todo.updated_at >= TodayStart]),
        
        % Find oldest and newest
        {Oldest, Newest} = case AllTodos of
            [] -> {null, null};
            _ ->
                Sorted = lists:sort(fun(A, B) -> A#todo.created_at =< B#todo.created_at end, AllTodos),
                OldestTodo = hd(Sorted),
                NewestTodo = lists:last(Sorted),
                {OldestTodo#todo.created_at, NewestTodo#todo.created_at}
        end,
        
        #{
            <<"total">> => Total,
            <<"completed">> => Completed,
            <<"pending">> => Pending,
            <<"completion_rate">> => round(CompletionRate * 10) / 10,
            <<"created_today">> => CreatedToday,
            <<"updated_today">> => UpdatedToday,
            <<"oldest_todo">> => Oldest,
            <<"newest_todo">> => Newest
        }
    end),
    
    case Result of
        {atomic, Stats} ->
            {reply, {ok, Stats}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update, Id, UpdateData}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        case mnesia:read(?TABLE, Id) of
            [Todo] ->
                UpdatedTodo = update_todo(Todo, UpdateData),
                mnesia:write(?TABLE, UpdatedTodo, write),
                {ok, Todo, UpdatedTodo};
            [] ->
                {error, not_found}
        end
    end),
    
    case Result of
        {atomic, {ok, _OldTodo, UpdatedTodo}} ->
            % Log audit event (async)
            spawn(fun() ->
                audit_log:log_event(Id, <<"updated">>, UpdateData, <<"system">>)
            end),
            {reply, {ok, todo_to_map(UpdatedTodo)}, State};
        {atomic, {error, Reason}} ->
            {reply, {error, Reason}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete, Id}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        case mnesia:read(?TABLE, Id) of
            [_Todo] ->
                mnesia:delete(?TABLE, Id, write),
                ok;
            [] ->
                {error, not_found}
        end
    end),
    
    case Result of
        {atomic, ok} ->
            {reply, ok, State};
        {atomic, {error, Reason}} ->
            {reply, {error, Reason}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({restore, Id}, _From, State) ->
    % Restore a soft-deleted todo
    Result = mnesia:transaction(fun() ->
        case mnesia:read(?TABLE, Id) of
            [Todo] when Todo#todo.deleted =:= true ->
                UpdatedAt = erlang:system_time(second),
                RestoredTodo = Todo#todo{deleted = false, updated_at = UpdatedAt},
                mnesia:write(?TABLE, RestoredTodo, write),
                {ok, RestoredTodo};
            [_Todo] ->
                {error, not_deleted};
            [] ->
                {error, not_found}
        end
    end),
    
    case Result of
        {atomic, {ok, RestoredTodo}} ->
            % Log audit event (async)
            spawn(fun() ->
                audit_log:log_event(Id, <<"restored">>, #{}, <<"system">>)
            end),
            {reply, {ok, todo_to_map(RestoredTodo)}, State};
        {atomic, {error, Reason}} ->
            {reply, {error, Reason}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({bulk_complete, Ids}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        UpdatedAt = erlang:system_time(second),
        lists:foldl(fun(Id, SuccessCount) ->
            case mnesia:read(?TABLE, Id) of
                [Todo] when Todo#todo.deleted =:= false ->
                    UpdatedTodo = Todo#todo{completed = true, updated_at = UpdatedAt},
                    mnesia:write(?TABLE, UpdatedTodo, write),
                    SuccessCount + 1;
                _ ->
                    SuccessCount
            end
        end, 0, Ids)
    end),
    
    case Result of
        {atomic, SuccessCount} ->
            {reply, {ok, SuccessCount}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({bulk_delete, Ids}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        UpdatedAt = erlang:system_time(second),
        lists:foldl(fun(Id, SuccessCount) ->
            case mnesia:read(?TABLE, Id) of
                [Todo] when Todo#todo.deleted =:= false ->
                    DeletedTodo = Todo#todo{deleted = true, updated_at = UpdatedAt},
                    mnesia:write(?TABLE, DeletedTodo, write),
                    SuccessCount + 1;
                _ ->
                    SuccessCount
            end
        end, 0, Ids)
    end),
    
    case Result of
        {atomic, SuccessCount} ->
            {reply, {ok, SuccessCount}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Initialize Mnesia database and create table if needed
init_mnesia() ->
    % Create schema if it doesn't exist
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        Error -> Error
    end,
    
    % Start Mnesia
    case mnesia:start() of
        ok -> ok;
        {error, {already_started, mnesia}} -> ok;
        Error2 -> Error2
    end,
    
    % Wait for tables to be available
    mnesia:wait_for_tables([schema], 5000),
    
    % Create table if it doesn't exist
    case mnesia:create_table(?TABLE, [
        {attributes, record_info(fields, todo)},
        {ram_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> 
            io:format("✓ Mnesia table created~n"),
            ok;
        {aborted, {already_exists, ?TABLE}} -> 
            io:format("✓ Mnesia table exists~n"),
            ok;
        {aborted, Reason} -> 
            io:format("✗ Mnesia error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Generate a unique ID for a new todo
generate_id() ->
    erlang:unique_integer([positive, monotonic]).

%% @doc Convert todo record to map
todo_to_map(#todo{id = Id, title = Title, description = Desc, 
                  completed = Completed, tags = Tags, priority = Priority,
                  due_date = DueDate, deleted = Deleted,
                  created_at = CreatedAt, updated_at = UpdatedAt}) ->
    #{
        <<"id">> => Id,
        <<"title">> => Title,
        <<"description">> => Desc,
        <<"completed">> => Completed,
        <<"tags">> => Tags,
        <<"priority">> => Priority,
        <<"due_date">> => format_date(DueDate),
        <<"due_date_timestamp">> => DueDate,
        <<"deleted">> => Deleted,
        <<"created_at">> => format_date(CreatedAt),
        <<"created_at_timestamp">> => CreatedAt,
        <<"updated_at">> => format_date(UpdatedAt),
        <<"updated_at_timestamp">> => UpdatedAt
    }.

%% @doc Format timestamp to DD/MM/YYYY HH:MM:SS
format_date(null) -> null;
format_date(Timestamp) when is_integer(Timestamp) ->
    DateTime = calendar:system_time_to_universal_time(Timestamp, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    iolist_to_binary(io_lib:format("~2..0B/~2..0B/~4..0B ~2..0B:~2..0B:~2..0B", 
                                    [Day, Month, Year, Hour, Minute, Second]));
format_date(_) -> null.

%% @doc Parse date input - accepts timestamp (integer) or DD/MM/YYYY string
parse_date_input(null) -> null;
parse_date_input(Timestamp) when is_integer(Timestamp) -> Timestamp;
parse_date_input(DateString) when is_binary(DateString) ->
    try
        % Try to parse DD/MM/YYYY or DD/MM/YYYY HH:MM:SS
        case binary:split(DateString, <<" ">>, [global]) of
            [DatePart, TimePart] ->
                % Has time part
                [Day, Month, Year] = binary:split(DatePart, <<"/">>, [global]),
                [Hour, Minute, Second] = binary:split(TimePart, <<":">>, [global]),
                DateTime = {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
                           {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)}},
                calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200; % Unix epoch adjustment
            [DatePart] ->
                % Only date part, assume 00:00:00
                [Day, Month, Year] = binary:split(DatePart, <<"/">>, [global]),
                DateTime = {{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
                           {0, 0, 0}},
                calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200 % Unix epoch adjustment
        end
    catch
        _:_ -> null % If parsing fails, return null
    end;
parse_date_input(_) -> null.

%% @doc Update todo record with new data
update_todo(Todo, UpdateData) ->
    Title = maps:get(<<"title">>, UpdateData, Todo#todo.title),
    Description = maps:get(<<"description">>, UpdateData, Todo#todo.description),
    Completed = maps:get(<<"completed">>, UpdateData, Todo#todo.completed),
    Tags = maps:get(<<"tags">>, UpdateData, Todo#todo.tags),
    Priority = maps:get(<<"priority">>, UpdateData, Todo#todo.priority),
    DueDateInput = maps:get(<<"due_date">>, UpdateData, Todo#todo.due_date),
    
    % Parse due_date if provided
    DueDate = case maps:is_key(<<"due_date">>, UpdateData) of
        true -> parse_date_input(DueDateInput);
        false -> Todo#todo.due_date
    end,
    
    UpdatedAt = erlang:system_time(second),
    
    Todo#todo{
        title = Title,
        description = Description,
        completed = Completed,
        tags = Tags,
        priority = Priority,
        due_date = DueDate,
        updated_at = UpdatedAt
    }.

%% @doc Paginate a list
paginate_list(List, Offset, Limit) ->
    case Offset >= length(List) of
        true -> [];
        false ->
            Sublist = lists:nthtail(Offset, List),
            case length(Sublist) > Limit of
                true -> lists:sublist(Sublist, Limit);
                false -> Sublist
            end
    end.

%% @doc Sort todos by field and order
sort_todos(Todos, SortBy, Order) ->
    CompareFun = case {SortBy, Order} of
        {<<"created_at">>, <<"asc">>} -> fun(A, B) -> A#todo.created_at =< B#todo.created_at end;
        {<<"created_at">>, <<"desc">>} -> fun(A, B) -> A#todo.created_at >= B#todo.created_at end;
        {<<"updated_at">>, <<"asc">>} -> fun(A, B) -> A#todo.updated_at =< B#todo.updated_at end;
        {<<"updated_at">>, <<"desc">>} -> fun(A, B) -> A#todo.updated_at >= B#todo.updated_at end;
        {<<"title">>, <<"asc">>} -> fun(A, B) -> A#todo.title =< B#todo.title end;
        {<<"title">>, <<"desc">>} -> fun(A, B) -> A#todo.title >= B#todo.title end;
        {<<"priority">>, <<"asc">>} -> fun(A, B) -> priority_value(A#todo.priority) =< priority_value(B#todo.priority) end;
        {<<"priority">>, <<"desc">>} -> fun(A, B) -> priority_value(A#todo.priority) >= priority_value(B#todo.priority) end;
        {<<"due_date">>, <<"asc">>} -> fun(A, B) -> due_date_value(A#todo.due_date) =< due_date_value(B#todo.due_date) end;
        {<<"due_date">>, <<"desc">>} -> fun(A, B) -> due_date_value(A#todo.due_date) >= due_date_value(B#todo.due_date) end;
        _ -> fun(A, B) -> A#todo.id =< B#todo.id end % Default
    end,
    lists:sort(CompareFun, Todos).

%% @doc Convert priority to numeric value for sorting
priority_value(<<"urgent">>) -> 4;
priority_value(<<"high">>) -> 3;
priority_value(<<"medium">>) -> 2;
priority_value(<<"low">>) -> 1;
priority_value(_) -> 0.

%% @doc Convert due_date to sortable value (null becomes max value)
due_date_value(null) -> 9999999999;
due_date_value(DueDate) when is_integer(DueDate) -> DueDate;
due_date_value(_) -> 9999999999.

