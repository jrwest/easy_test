%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(easy_test).

-export([parse_transform/2]).

-define(EASY_TEST_PREFIX, "test_").
-define(EASY_TEST_DEFAULT_GRP_OPTS, [shuffle]).

-define(EASY_TESTS_ETS, easy_exports).

parse_transform(Forms, _) ->
    ets:new(?EASY_TESTS_ETS, [ordered_set, protected, named_table]),
    scan_forms(Forms),
    Result = rewrite(Forms),
    ets:delete(?EASY_TESTS_ETS),
    Result.

scan_forms(Forms) ->
    TestPrefix = ?EASY_TEST_PREFIX,
    F = fun(Form) ->
		form(Form, TestPrefix)	       
	end,
    lists:foreach(F, Forms).

form({attribute, _L, easy_test, Data},  _) ->
    Name = proplists:get_value(test, Data),
    HasConfig = proplists:get_value(has_config, Data),
    store_export(Name,1),
    case HasConfig of
	true ->
	    store_export(Name, 0);
	_ ->
	    ok
    end;
form({function, _L, Name, 1, _Cs}, TestPrefix) ->
    NameAsList = atom_to_list(Name),
    case lists:prefix(TestPrefix, NameAsList) of
	true ->
	    store_export(Name, 1);
	false ->
	    skipped
    end;
form(_, _) ->
    skipped.

store_export(Name,Arity) ->
    ets:insert(?EASY_TESTS_ETS, {make_ref(), Name, Arity}).

rewrite([{attribute, _, module, _Name}=M | Fs]) ->
    module_decl(M, Fs);
rewrite([F | Fs]) -> % skip anything before the module declaration in the forms list
    [F | rewrite(Fs)];
rewrite([]) -> 
    []. % missing module delcaration failsafe

rewrite([{function, _, all, 0, _}=F | Fs], As, {_ExportAllFun, Tests}) ->    
    rewrite(Fs, [F | As], {false, Tests});
rewrite([F | Fs], As, ExportData) -> 
    rewrite(Fs, [F | As], ExportData);
rewrite([], As, {ExportAllFun, Tests}) -> 
    {if ExportAllFun ->
	     write_all(As, Tests);
	true -> As end, ExportAllFun}.

module_decl(M, Fs) ->
    {AllExports, Tests} = fetch_data(ets:first(?EASY_TESTS_ETS), [], []),
    {Fs1, ExportAllFun} = rewrite(Fs, [], {true, Tests}),
    Es = if ExportAllFun -> [{all, 0} | AllExports];
	    true -> AllExports end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].

fetch_data('$end_of_table', AllAcc, TestsAcc) ->
    {lists:reverse(AllAcc), lists:reverse(TestsAcc)};
fetch_data(Key, AllAcc, TestsAcc) ->
    [{_, Name, Arity} | _] = ets:lookup(?EASY_TESTS_ETS, Key),
    Data = {Name, Arity},
    case Arity of
	0 ->
	    fetch_data(ets:next(?EASY_TESTS_ETS, Key), [Data | AllAcc], TestsAcc);
	1 ->
	    fetch_data(ets:next(?EASY_TESTS_ETS, Key), [Data | AllAcc], [Data | TestsAcc])
    end.

write_all(As, Tests) ->
    [{function,0,all,0,
      [{clause, 0, [], [], [write_all_tests(Tests)]}]} | As].

write_all_tests([]) ->
    {nil,0};
write_all_tests([{_, 0} | Tests]) -> % Skip test case config functions
    write_all_tests(Tests);
write_all_tests([{Test, 1} | Tests]) ->
    {cons,0,
     {atom,0,Test},
     write_all_tests(Tests)}.
