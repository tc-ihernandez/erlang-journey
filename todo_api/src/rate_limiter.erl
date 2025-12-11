-module(rate_limiter).
-behaviour(gen_server).

%% API
-export([start_link/0, check_rate_limit/1, get_limit_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(RATE_LIMIT, 100). % requests per window
-define(WINDOW_SIZE, 60000). % 60 seconds in milliseconds
-define(CLEANUP_INTERVAL, 120000). % Clean up old entries every 2 minutes

-record(state, {
    requests = #{} :: map() % IP => {Count, WindowStart}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the rate limiter server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if IP has exceeded rate limit
%% Returns: {ok, Remaining} | {error, rate_limit_exceeded}
check_rate_limit(IP) ->
    gen_server:call(?SERVER, {check_rate_limit, IP}).

%% @doc Get rate limit info for IP
%% Returns: {Count, Limit, ResetTime}
get_limit_info(IP) ->
    gen_server:call(?SERVER, {get_limit_info, IP}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Schedule periodic cleanup
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    {ok, #state{}}.

handle_call({check_rate_limit, IP}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Requests = State#state.requests,
    
    case maps:get(IP, Requests, undefined) of
        undefined ->
            % First request from this IP
            NewRequests = maps:put(IP, {1, Now}, Requests),
            {reply, {ok, ?RATE_LIMIT - 1}, State#state{requests = NewRequests}};
        {Count, WindowStart} ->
            case Now - WindowStart >= ?WINDOW_SIZE of
                true ->
                    % Window expired, reset counter
                    NewRequests = maps:put(IP, {1, Now}, Requests),
                    {reply, {ok, ?RATE_LIMIT - 1}, State#state{requests = NewRequests}};
                false ->
                    % Still in same window
                    case Count >= ?RATE_LIMIT of
                        true ->
                            % Rate limit exceeded
                            {reply, {error, rate_limit_exceeded}, State};
                        false ->
                            % Increment counter
                            NewRequests = maps:put(IP, {Count + 1, WindowStart}, Requests),
                            {reply, {ok, ?RATE_LIMIT - Count - 1}, State#state{requests = NewRequests}}
                    end
            end
    end;

handle_call({get_limit_info, IP}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Requests = State#state.requests,
    
    case maps:get(IP, Requests, undefined) of
        undefined ->
            {reply, {0, ?RATE_LIMIT, Now + ?WINDOW_SIZE}, State};
        {Count, WindowStart} ->
            ResetTime = WindowStart + ?WINDOW_SIZE,
            case Now >= ResetTime of
                true ->
                    {reply, {0, ?RATE_LIMIT, Now + ?WINDOW_SIZE}, State};
                false ->
                    {reply, {Count, ?RATE_LIMIT, ResetTime}, State}
            end
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    Now = erlang:system_time(millisecond),
    Requests = State#state.requests,
    
    % Remove expired entries
    CleanedRequests = maps:filter(fun(_IP, {_Count, WindowStart}) ->
        Now - WindowStart < ?WINDOW_SIZE * 2 % Keep entries for 2 windows
    end, Requests),
    
    % Schedule next cleanup
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),
    
    {noreply, State#state{requests = CleanedRequests}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

