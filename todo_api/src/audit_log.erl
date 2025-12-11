-module(audit_log).
-behaviour(gen_server).

%% API
-export([start_link/0, log_event/4, get_history/1, get_all_history/0, clear_old_logs/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, audit_log).

-record(audit_entry, {
    id :: integer(),
    todo_id :: integer(),
    action :: binary(), % created, updated, deleted, restored
    changes :: map(),
    timestamp :: integer(),
    user_ip :: binary()
}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the audit log server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Log an event
%% Action: created | updated | deleted | restored | bulk_complete | bulk_delete
log_event(TodoId, Action, Changes, UserIP) ->
    gen_server:cast(?SERVER, {log_event, TodoId, Action, Changes, UserIP}).

%% @doc Get history for a specific TODO
get_history(TodoId) ->
    gen_server:call(?SERVER, {get_history, TodoId}).

%% @doc Get all audit history
get_all_history() ->
    gen_server:call(?SERVER, get_all_history).

%% @doc Clear logs older than N days
clear_old_logs(Days) ->
    gen_server:call(?SERVER, {clear_old_logs, Days}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    init_mnesia(),
    {ok, #state{}}.

handle_call({get_history, TodoId}, _From, State) ->
    Result = mnesia:transaction(fun() ->
        Entries = mnesia:match_object(?TABLE, #audit_entry{todo_id = TodoId, _ = '_'}, read),
        Sorted = lists:sort(fun(A, B) -> 
            A#audit_entry.timestamp >= B#audit_entry.timestamp 
        end, Entries),
        [entry_to_map(E) || E <- Sorted]
    end),
    
    case Result of
        {atomic, History} ->
            {reply, {ok, History}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_all_history, _From, State) ->
    Result = mnesia:transaction(fun() ->
        AllEntries = mnesia:match_object(?TABLE, #audit_entry{_ = '_'}, read),
        Sorted = lists:sort(fun(A, B) -> 
            A#audit_entry.timestamp >= B#audit_entry.timestamp 
        end, AllEntries),
        [entry_to_map(E) || E <- Sorted]
    end),
    
    case Result of
        {atomic, History} ->
            {reply, {ok, History}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({clear_old_logs, Days}, _From, State) ->
    CutoffTime = erlang:system_time(second) - (Days * 86400),
    
    Result = mnesia:transaction(fun() ->
        AllEntries = mnesia:match_object(?TABLE, #audit_entry{_ = '_'}, read),
        OldEntries = [E || E <- AllEntries, E#audit_entry.timestamp < CutoffTime],
        lists:foreach(fun(Entry) ->
            mnesia:delete_object(?TABLE, Entry, write)
        end, OldEntries),
        length(OldEntries)
    end),
    
    case Result of
        {atomic, Count} ->
            {reply, {ok, Count}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast({log_event, TodoId, Action, Changes, UserIP}, State) ->
    spawn(fun() ->
        Id = generate_id(),
        Timestamp = erlang:system_time(second),
        
        Entry = #audit_entry{
            id = Id,
            todo_id = TodoId,
            action = Action,
            changes = Changes,
            timestamp = Timestamp,
            user_ip = UserIP
        },
        
        mnesia:transaction(fun() ->
            mnesia:write(?TABLE, Entry, write)
        end)
    end),
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

%% @doc Initialize Mnesia table
init_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    case mnesia:create_table(?TABLE, [
        {attributes, record_info(fields, audit_entry)},
        {ram_copies, [node()]},
        {type, set}
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?TABLE}} -> ok;
        {aborted, Reason} -> 
            io:format("Failed to create audit_log table: ~p~n", [Reason]),
            error(Reason)
    end,
    
    mnesia:wait_for_tables([?TABLE], 5000).

%% @doc Generate unique ID
generate_id() ->
    erlang:unique_integer([positive, monotonic]).

%% @doc Convert audit entry to map
entry_to_map(#audit_entry{id = Id, todo_id = TodoId, action = Action, 
                          changes = Changes, timestamp = Timestamp, user_ip = UserIP}) ->
    #{
        <<"id">> => Id,
        <<"todo_id">> => TodoId,
        <<"action">> => Action,
        <<"changes">> => Changes,
        <<"timestamp">> => Timestamp,
        <<"user_ip">> => UserIP
    }.

