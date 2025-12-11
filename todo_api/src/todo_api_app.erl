-module(todo_api_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @doc Start the application
start(_StartType, _StartArgs) ->
    io:format("~n=================================================~n"),
    io:format("Starting TODO API...~n"),
    io:format("=================================================~n~n"),
    
    case todo_api_sup:start_link() of
        {ok, Pid} ->
            io:format("✓ TODO API started successfully~n"),
            io:format("✓ HTTP Server listening on http://localhost:8000~n"),
            io:format("✓ Mnesia database initialized~n~n"),
            io:format("Available endpoints:~n"),
            io:format("  GET    /health          - Health check~n"),
            io:format("  GET    /todos           - List all todos~n"),
            io:format("  GET    /todos/:id       - Get a specific todo~n"),
            io:format("  POST   /todos           - Create a new todo~n"),
            io:format("  PUT    /todos/:id       - Update a todo~n"),
            io:format("  DELETE /todos/:id       - Delete a todo~n~n"),
            io:format("=================================================~n~n"),
            {ok, Pid};
        Error ->
            io:format("✗ Failed to start TODO API: ~p~n", [Error]),
            Error
    end.

%% @doc Stop the application
stop(_State) ->
    io:format("~nStopping TODO API...~n"),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

