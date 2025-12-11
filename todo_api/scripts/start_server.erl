#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    io:format("Starting TODO API server...~n"),
    
    % Add paths
    code:add_pathz("_build/default/lib/elli/ebin"),
    code:add_pathz("_build/default/lib/jsone/ebin"),
    code:add_pathz("_build/default/lib/todo_api/ebin"),
    
    % Start applications
    application:ensure_all_started(mnesia),
    application:ensure_all_started(elli),
    application:ensure_all_started(jsone),
    application:ensure_all_started(todo_api),
    
    io:format("~n=================================================~n"),
    io:format("TODO API is running on http://localhost:8080~n"),
    io:format("Press Ctrl+C to stop~n"),
    io:format("=================================================~n~n"),
    
    % Keep alive
    timer:sleep(infinity).

