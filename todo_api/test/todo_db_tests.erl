-module(todo_db_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Start required applications
    application:ensure_all_started(todo_api),
    ok.

teardown(_) ->
    % Cleanup
    application:stop(todo_api),
    ok.

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

crud_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Create TODO", fun test_create_todo/0},
      {"Read TODO", fun test_read_todo/0},
      {"Update TODO", fun test_update_todo/0},
      {"Delete TODO", fun test_delete_todo/0}
     ]}.

advanced_features_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Tags System", fun test_tags/0},
      {"Priority", fun test_priority/0},
      {"Due Date", fun test_due_date/0},
      {"Soft Delete and Restore", fun test_soft_delete_restore/0},
      {"Bulk Operations", fun test_bulk_operations/0}
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_create_todo() ->
    % Create a TODO
    TodoData = #{
        <<"title">> => <<"Test TODO">>,
        <<"description">> => <<"Test Description">>,
        <<"tags">> => [<<"test">>, <<"eunit">>],
        <<"priority">> => <<"high">>
    },
    {ok, Todo} = todo_db:create(TodoData),
    
    % Verify
    ?assert(maps:is_key(<<"id">>, Todo)),
    ?assertEqual(<<"Test TODO">>, maps:get(<<"title">>, Todo)),
    ?assertEqual(<<"high">>, maps:get(<<"priority">>, Todo)),
    ?assertEqual([<<"test">>, <<"eunit">>], maps:get(<<"tags">>, Todo)).

test_read_todo() ->
    % Create a TODO
    {ok, Todo} = todo_db:create(#{<<"title">> => <<"Read Test">>}),
    Id = maps:get(<<"id">>, Todo),
    
    % Read it
    {ok, ReadTodo} = todo_db:read(Id),
    
    % Verify
    ?assertEqual(Id, maps:get(<<"id">>, ReadTodo)),
    ?assertEqual(<<"Read Test">>, maps:get(<<"title">>, ReadTodo)).

test_update_todo() ->
    % Create a TODO
    {ok, Todo} = todo_db:create(#{<<"title">> => <<"Original Title">>}),
    Id = maps:get(<<"id">>, Todo),
    
    % Update it
    {ok, Updated} = todo_db:update(Id, #{
        <<"title">> => <<"Updated Title">>,
        <<"completed">> => true
    }),
    
    % Verify
    ?assertEqual(<<"Updated Title">>, maps:get(<<"title">>, Updated)),
    ?assertEqual(true, maps:get(<<"completed">>, Updated)).

test_delete_todo() ->
    % Create a TODO
    {ok, Todo} = todo_db:create(#{<<"title">> => <<"To Delete">>}),
    Id = maps:get(<<"id">>, Todo),
    
    % Delete it (soft delete)
    ok = todo_db:delete(Id),
    
    % Should not appear in list
    {ok, Todos} = todo_db:read_all(),
    ?assertEqual(false, lists:any(fun(T) -> maps:get(<<"id">>, T) =:= Id end, Todos)).

test_tags() ->
    % Create TODO with tags
    {ok, Todo} = todo_db:create(#{
        <<"title">> => <<"Tagged TODO">>,
        <<"tags">> => [<<"erlang">>, <<"testing">>]
    }),
    
    % Verify tags
    ?assertEqual([<<"erlang">>, <<"testing">>], maps:get(<<"tags">>, Todo)),
    
    % Get all tags
    {ok, AllTags} = todo_db:get_all_tags(),
    ?assert(length(AllTags) > 0).

test_priority() ->
    % Create TODOs with different priorities
    {ok, Low} = todo_db:create(#{<<"title">> => <<"Low">>, <<"priority">> => <<"low">>}),
    {ok, High} = todo_db:create(#{<<"title">> => <<"High">>, <<"priority">> => <<"high">>}),
    
    ?assertEqual(<<"low">>, maps:get(<<"priority">>, Low)),
    ?assertEqual(<<"high">>, maps:get(<<"priority">>, High)).

test_due_date() ->
    % Create TODO with due date
    DueDate = erlang:system_time(second) + 86400, % Tomorrow
    {ok, Todo} = todo_db:create(#{
        <<"title">> => <<"Due Tomorrow">>,
        <<"due_date">> => DueDate
    }),
    
    ?assertEqual(DueDate, maps:get(<<"due_date">>, Todo)).

test_soft_delete_restore() ->
    % Create TODO
    {ok, Todo} = todo_db:create(#{<<"title">> => <<"To Restore">>}),
    Id = maps:get(<<"id">>, Todo),
    
    % Soft delete
    ok = todo_db:delete(Id),
    
    % Restore
    {ok, Restored} = todo_db:restore(Id),
    
    % Verify
    ?assertEqual(false, maps:get(<<"deleted">>, Restored)),
    ?assertEqual(Id, maps:get(<<"id">>, Restored)).

test_bulk_operations() ->
    % Create multiple TODOs
    {ok, T1} = todo_db:create(#{<<"title">> => <<"Bulk 1">>}),
    {ok, T2} = todo_db:create(#{<<"title">> => <<"Bulk 2">>}),
    {ok, T3} = todo_db:create(#{<<"title">> => <<"Bulk 3">>}),
    
    Ids = [
        maps:get(<<"id">>, T1),
        maps:get(<<"id">>, T2),
        maps:get(<<"id">>, T3)
    ],
    
    % Bulk complete
    {ok, Count} = todo_db:bulk_complete(Ids),
    ?assert(Count >= 2), % At least 2 should succeed
    
    % Bulk delete
    {ok, DeleteCount} = todo_db:bulk_delete(Ids),
    ?assert(DeleteCount >= 2).

