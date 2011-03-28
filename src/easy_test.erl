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
-define(EASY_GROUPS_ETS, easy_groups).

parse_transform(Forms, _) ->
    ets:new(?EASY_TESTS_ETS, [ordered_set, protected, named_table]),
    ets:new(?EASY_GROUPS_ETS, [ordered_set, protected, named_table]),
    ets:new(group_set(all), [ordered_set, protected, named_table]),
    scan_forms(Forms),
    Result = rewrite(Forms),
    ets:delete(?EASY_TESTS_ETS),
    ets:delete(?EASY_GROUPS_ETS),
    ets:delete(group_set(all)),
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
    Group = proplists:get_value(group, Data, all),
    store_export(Name,1),
    store_test(Group, Name),
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
	    store_export(Name, 1),
	    store_test(all, Name);
	false ->
	    skipped
    end;
form(_, _) ->
    skipped.

store_export(Name,Arity) ->
    ets:insert(?EASY_TESTS_ETS, {make_ref(), Name, Arity}).

store_test(GroupName, Test) ->
    GroupSetName = group_set(GroupName),
    case lists:member(GroupSetName, ets:all()) of
	true ->	    
	    ets:insert(GroupSetName, {make_ref(), test, Test});
	false ->
	    Parent = all,
	    store_group(GroupName, Parent),
	    store_test(GroupName, Test)
    end.

store_group(GroupName, ParentName) ->
    GroupSetName = group_set(GroupName),
    ParentSetName = group_set(ParentName),
    ets:new(GroupSetName, [ordered_set, protected, named_table]),
    ets:insert(?EASY_GROUPS_ETS, {GroupName, [], ParentName}),
    ets:insert(ParentSetName, {make_ref(), group, GroupName}).	   
		     

group_set(Group) ->
    list_to_atom("easy_group_" ++ atom_to_list(Group)).

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
	     write_all0(As, Tests);
	true -> As end, ExportAllFun}.

module_decl(M, Fs) ->
    AllExports = prepare_exports(fetch_table_data(?EASY_TESTS_ETS), []),
    Tests = fetch_table_data(group_set(all)),
    {Fs1, ExportAllFun} = rewrite(Fs, [], {true, Tests}),
    Es = if ExportAllFun -> [{all, 0} | AllExports];
	    true -> AllExports end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].

fetch_table_data(Table) ->
    fetch_data(Table, ets:first(Table), []).

fetch_data(_T, '$end_of_table', Acc) ->
    lists:reverse(Acc);
fetch_data(Table, Key, Acc) ->
    [Data | _] = ets:lookup(Table, Key),
    fetch_data(Table, ets:next(Table, Key), [Data | Acc]).

prepare_exports([], Acc) ->
    lists:reverse(Acc);
prepare_exports([H | T], Acc) ->
    {_, Name, Arity} = H,
    prepare_exports(T, [{Name, Arity} | Acc]).

write_all0(As, Tests) ->
    [{function,0,all,0,
      [{clause, 0, [], [], [write_all_data(Tests)]}]} | As].

write_all_data([]) ->
    {nil, 0};
write_all_data([{_, group, Group} | Tests]) ->
    {cons,0,
     {tuple,0,[{atom,0,group},{atom,0,Group}]},
     write_all_data(Tests)};
write_all_data([{_, test, Test} | Tests]) ->
    {cons,0,
     {atom,0,Test},
     write_all_data(Tests)}.


%% Used for debugging purposes only
dump_ets_table(Name) ->
    io:format("ETS TABLE -- ~p -- ~n", [Name]),
    dump_ets_table(Name, ets:first(Name)).

dump_ets_table(_, '$end_of_table') ->
    io:format("Done.~n");
dump_ets_table(Name, Key) ->
    io:format("~p~n", [ets:lookup(Name, Key)]),
    dump_ets_table(Name, ets:next(Name, Key)).
