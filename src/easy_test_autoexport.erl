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
    F = fun(Form, Set) ->
		form(Form, Set, TestPrefix)	       
	end,
    Tests = sets:to_list(lists:foldl(F, sets:new(), Forms)),
    rewrite(Forms, Tests).

form({attribute, _L, easy_test, Data}, Tests, _) ->
    Name = proplists:get_value(test, Data),
    HasInit = proplists:get_value(init, Data),
    Tests1 = sets:add_element({Name, 1}, Tests),
    case HasInit of
	true ->
	    sets:add_element({Name, 0}, Tests1);
	_ ->
	    Tests1
    end;
form({function, _L, Name, 1, _Cs}, Tests, TestPrefix) ->
    NameAsList = atom_to_list(Name),
    case lists:prefix(TestPrefix, NameAsList) of
	true ->
	    sets:add_element({Name, 1}, Tests);
	false ->
	    Tests
    end;
form(_, Tests, _) ->
    Tests.

rewrite([{attribute, _, module, _Name}=M | Fs], Exports) ->
    module_decl(M, Fs, Exports);
rewrite([F | Fs], Exports) -> % skip anything before the module declaration in the forms list
    [F | rewrite(Fs, Exports)];
rewrite([], _) -> 
    []. % missing module delcaration failsafe

rewrite([{function, _, all, 0, _}=F | Fs], As, {_ExportAllFun, Tests}) ->    
    rewrite(Fs, [F | As], {false, Tests});
rewrite([F | Fs], As, ExportData) -> 
    rewrite(Fs, [F | As], ExportData);
rewrite([], As, {ExportAllFun, Tests}) -> 
    {if ExportAllFun ->
	     write_all(As, Tests);
	true -> As end, ExportAllFun}.

module_decl(M, Fs, Exports) ->
    {Fs1, ExportAllFun} = rewrite(Fs, [], {true, lists:reverse(Exports)}),
    Es = if ExportAllFun -> [{all, 0} | Exports];
	    true -> Exports end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].

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
