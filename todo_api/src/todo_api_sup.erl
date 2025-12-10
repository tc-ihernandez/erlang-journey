-module(todo_api_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc Initialize the supervisor with child specifications
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },
    
    % Define child specifications
    ChildSpecs = [
        % Database server
        #{
            id => todo_db,
            start => {todo_db, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [todo_db]
        },
        % HTTP server (Elli)
        #{
            id => elli_http,
            start => {elli, start_link, [[
                {callback, todo_handler},
                {port, 8000}
            ]]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [elli]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

