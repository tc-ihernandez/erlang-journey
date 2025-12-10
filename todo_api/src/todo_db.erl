-module(todo_db).
-behaviour(gen_server).

%% API exports
-export([start_link/0, create/1, read/1, read_all/0, read_all_paginated/2, read_by_status/1, update/2, delete/1, count_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(todo, {
    id :: integer(),
    title :: binary(),
    description :: binary(),
    completed = false :: boolean(),
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

%% @doc Count total number of todos
%% Output: {ok, Count}
count_all() ->
    gen_server:call(?SERVER, count_all).

%% @doc Read todos filtered by completion status
%% Input: true | false
%% Output: {ok, [Todo]}
read_by_status(Completed) when is_boolean(Completed) ->
    gen_server:call(?SERVER, {read_by_status, Completed}).

%% @doc Update a todo item
%% Input: Id, #{title => binary(), description => binary(), completed => boolean()}
%% Output: {ok, UpdatedTodo} | {error, Reason}
update(Id, UpdateData) ->
    gen_server:call(?SERVER, {update, Id, UpdateData}).

%% @doc Delete a todo item
%% Output: ok | {error, not_found}
delete(Id) ->
    gen_server:call(?SERVER, {delete, Id}).

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
                created_at = Timestamp,
                updated_at = Timestamp
            },
            
            Result = mnesia:transaction(fun() ->
                mnesia:write(?TABLE, Todo, write)
            end),
            
            case Result of
                {atomic, ok} ->
                    {reply, {ok, Id}, State};
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

handle_call({read_all_paginated, Page, Limit}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllTodos = mnesia:match_object(?TABLE, #todo{_ = '_'}, read),
        % Sort by ID (could be by created_at or other field)
        SortedTodos = lists:sort(fun(A, B) -> A#todo.id =< B#todo.id end, AllTodos),
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

handle_call({update, Id, UpdateData}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        case mnesia:read(?TABLE, Id) of
            [Todo] ->
                UpdatedTodo = update_todo(Todo, UpdateData),
                mnesia:write(?TABLE, UpdatedTodo, write),
                {ok, UpdatedTodo};
            [] ->
                {error, not_found}
        end
    end),
    
    case Result of
        {atomic, {ok, UpdatedTodo}} ->
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
                  completed = Completed, created_at = CreatedAt, updated_at = UpdatedAt}) ->
    #{
        <<"id">> => Id,
        <<"title">> => Title,
        <<"description">> => Desc,
        <<"completed">> => Completed,
        <<"created_at">> => CreatedAt,
        <<"updated_at">> => UpdatedAt
    }.

%% @doc Update todo record with new data
update_todo(Todo, UpdateData) ->
    Title = maps:get(<<"title">>, UpdateData, Todo#todo.title),
    Description = maps:get(<<"description">>, UpdateData, Todo#todo.description),
    Completed = maps:get(<<"completed">>, UpdateData, Todo#todo.completed),
    UpdatedAt = erlang:system_time(second),
    
    Todo#todo{
        title = Title,
        description = Description,
        completed = Completed,
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

