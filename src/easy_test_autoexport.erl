%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(easy_test_autoexport).

-export([parse_transform/2]).

-define(EASY_TEST_PREFIX, "test_").

parse_transform(Forms, _) ->
    TestPrefix = ?EASY_TEST_PREFIX,
    F = fun(Form) ->
		form(Form, TestPrefix)	       
	end,
    ets:new(easy_test, [ordered_set, protected, named_table]),
    lists:foreach(F, Forms),
    Result = rewrite(Forms),
    ets:delete(easy_test),
    Result.

form({attribute, _L, easy_test, Data},  _) ->
    Name = proplists:get_value(test, Data),
    HasInit = proplists:get_value(init, Data),
    ets:insert(easy_test, {make_ref(), Name, 1}),
    case HasInit of
	true ->
	    ets:insert(easy_test, {make_ref(), Name, 0});
	_ ->
	    ok
    end;
form({function, _L, Name, 1, _Cs}, TestPrefix) ->
    NameAsList = atom_to_list(Name),
    case lists:prefix(TestPrefix, NameAsList) of
	true ->
	    ets:insert(easy_test, {make_ref(), Name, 1});
	false ->
	    skipped
    end;
form(_, _) ->
    skipped.

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
    {AllExports, Tests} = fetch_data(ets:first(easy_test), [], []),
    {Fs1, ExportAllFun} = rewrite(Fs, [], {true, Tests}),
    Es = if ExportAllFun -> [{all, 0} | AllExports];
	    true -> AllExports end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].

fetch_data('$end_of_table', AllAcc, TestsAcc) ->
    {lists:reverse(AllAcc), lists:reverse(TestsAcc)};
fetch_data(Key, AllAcc, TestsAcc) ->
    [{_, Name, Arity} | _] = ets:lookup(easy_test, Key),
    Data = {Name, Arity},
    case Arity of
	0 ->
	    fetch_data(ets:next(easy_test, Key), [Data | AllAcc], TestsAcc);
	1 ->
	    fetch_data(ets:next(easy_test, Key), [Data | AllAcc], [Data | TestsAcc])
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
