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

-define(EASY_GROUP_DEFAULT_OPTS, [shuffle]).

parse_transform(Forms, _) ->
    create_tables([?EASY_TESTS_ETS, ?EASY_GROUPS_ETS, group_table_name(all)]),
    scan_forms(Forms),
    Result = rewrite(Forms),
    cleanup_group_tables(),
    delete_tables([?EASY_TESTS_ETS, ?EASY_GROUPS_ETS, group_table_name(all)]),
    Result.

scan_forms(Forms) ->
    TestPrefix = ?EASY_TEST_PREFIX,
    F = fun(Form) ->
		form(Form, TestPrefix)	       
	end,
    lists:foreach(F, Forms).

form({attribute, _L, easy_group, Data}, _) ->
    Name = get_required_value(Data,group),
    Context = get_optional_value(Data, context, all),
    Opts = get_optional_value(Data, opts, ?EASY_GROUP_DEFAULT_OPTS),
    Tests = get_optional_value(Data, tests, []),
    store_or_update_group(Name, Context, Opts, Tests);
form({attribute, _L, easy_test, Data},  _) ->
    Name = get_required_value(Data, test),
    HasConfig = get_optional_value(Data, has_config, false),
    Group = get_optional_value(Data, group, all),
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
    store_group_if_dne(GroupName),
    GroupSetName = group_table_name(GroupName),
    ets:insert(GroupSetName, {make_ref(), test, Test}).

store_or_update_group(Name, Context, Opts, Tests) ->
    case ets:lookup(?EASY_GROUPS_ETS, Name) of
	[] ->
	    store_group(Name, Context, Opts);
	[{N, _O, OldContext} | _] ->
	    move_group(OldContext, Context, Name),
	    ets:insert(?EASY_GROUPS_ETS, {N, Opts, Context})
    end,
    store_group_attr_tests(Name, Tests).

store_group_if_dne(GroupName) ->
    GroupTableName = group_table_name(GroupName),
    case lists:member(GroupTableName, ets:all()) of
	false ->
	    store_group(GroupName, all),
	    created;
	true ->
	    existed
    end.

store_group(GroupName, ParentName) ->
    store_group(GroupName, ParentName, ?EASY_GROUP_DEFAULT_OPTS).

store_group(GroupName, ParentName, Opts) ->
    store_group_if_dne(ParentName),
    ParentSetName = group_table_name(ParentName),
    GroupSetName = group_table_name(GroupName),
    create_table(GroupSetName),
    ets:insert(?EASY_GROUPS_ETS, {GroupName, Opts, ParentName}),
    ets:insert(ParentSetName, {make_ref(), group, GroupName}).
		     
move_group(OldContext, NewContext, GroupName) ->
    OldContextName = group_table_name(OldContext),
    NewContextName = group_table_name(NewContext),
    ets:match_delete(OldContextName, {'_', group, GroupName}),
    ets:insert(NewContextName, {make_ref(), group, GroupName}).

store_group_attr_tests(_, []) ->
    ok;
store_group_attr_tests(Name, [T | Ts]) ->
    store_test(Name, T),
    store_export(T, 1),
    store_group_attr_tests(Name, Ts).

group_table_name(Group) ->
    list_to_atom("easy_group_" ++ atom_to_list(Group)).

cleanup_group_tables() ->
    cleanup_group_tables(ets:first(?EASY_GROUPS_ETS)).

cleanup_group_tables('$end_of_table') ->
    ok;
cleanup_group_tables(Key) ->
    delete_table(group_table_name(Key)),
    cleanup_group_tables(ets:next(?EASY_GROUPS_ETS, Key)).

rewrite([{attribute, _, module, _Name}=M | Fs]) ->
    module_decl(M, Fs);
rewrite([F | Fs]) -> % skip anything before the module declaration in the forms list
    [F | rewrite(Fs)];
rewrite([]) -> 
    []. % missing module delcaration failsafe

rewrite([{function, _, all, 0, _}=F | Fs], As, {_, ExportGroups}) ->    
    rewrite(Fs, [F | As], {false, ExportGroups});
rewrite([{function, _, groups, 0, _}=F | Fs], As, {ExportAll, _}) ->
    rewrite(Fs, [F | As], {ExportAll, false});
rewrite([F | Fs], As, ExportData) -> 
    rewrite(Fs, [F | As], ExportData);
rewrite([], As, {ExportAllFun, ExportGroupsFun}) ->
    Final = write_all(write_groups(As, ExportGroupsFun), ExportAllFun),
    {Final, {ExportAllFun, ExportGroupsFun}}.
	

module_decl(M, Fs) ->
    AllExports = prepare_exports(fetch_table_data(?EASY_TESTS_ETS), []),
    {Fs1, {ExportAllFun, ExportGroupsFun}} = rewrite(Fs, [], {true, true}),
    Es = if ExportAllFun -> [{all, 0} | AllExports];
	    true -> AllExports end,
    Es1 = if ExportGroupsFun -> [{groups, 0} | Es];
	     true -> Es end,
    [M, {attribute,0,export,Es1} | lists:reverse(Fs1)].

create_tables([]) ->
    ok;
create_tables([T | Ts]) ->
    create_table(T),
    create_tables(Ts).

create_table(TableName) ->
    ets:new(TableName, [ordered_set, protected, named_table]).


delete_tables([]) ->
    ok;
delete_tables([T | Ts]) ->
    delete_table(T),
    delete_tables(Ts).

delete_table(TableName) -> ets:delete(TableName).

fetch_table_data(Table) ->
    fetch_data(Table, ets:first(Table), []).

fetch_data(_T, '$end_of_table', Acc) ->
    lists:reverse(Acc);
fetch_data(Table, Key, Acc) ->
    [Data | _] = ets:lookup(Table, Key),
    fetch_data(Table, ets:next(Table, Key), [Data | Acc]).

get_required_value(Data,Key) ->
    case proplists:get_value(Key, Data) of
	undefined ->
	    throw({easy_test, {missing_required_field, Key}});
	Val ->
	    Val
    end.

get_optional_value(Data,Key,Default) ->
    proplists:get_value(Key, Data, Default).

prepare_exports([], Acc) ->
    lists:reverse(Acc);
prepare_exports([H | T], Acc) ->
    {_, Name, Arity} = H,
    prepare_exports(T, [{Name, Arity} | Acc]).

write_groups(As, false) ->
    As;
write_groups(As, true) ->
    Groups = fetch_table_data(?EASY_GROUPS_ETS),
    write_groups0(As, Groups).

write_groups0(As, Groups) ->
    [{function,0,groups,0,
      [{clause,0,[],[],[write_groups_data(Groups)]}]} | As].

write_groups_data([]) ->
    {nil, 0};
write_groups_data([{Group, Opts, _} | Groups]) ->
    Tests = fetch_table_data(group_table_name(Group)),
    {cons,0,
     {tuple,0,[{atom,0,Group},
	       write_group_opts(Opts),
	       write_test_list(Tests)]},
     write_groups_data(Groups)}.

write_group_opts([]) ->
    {nil, 0};
write_group_opts([Opt | Opts]) when is_atom(Opt) ->
    {cons,0,
     {atom,0,Opt},
     write_group_opts(Opts)}.

write_all(As, false) ->
    As;
write_all(As, true) ->
    Tests = fetch_table_data(group_table_name(all)),
    write_all0(As, Tests).

write_all0(As, Tests) ->
    [{function,0,all,0,
      [{clause, 0, [], [], [write_test_list(Tests)]}]} | As].

write_test_list([]) ->
    {nil, 0};
write_test_list([{_, group, Group} | Tests]) ->
    {cons,0,
     {tuple,0,[{atom,0,group},{atom,0,Group}]},
     write_test_list(Tests)};
write_test_list([{_, test, Test} | Tests]) ->
    {cons,0,
     {atom,0,Test},
     write_test_list(Tests)}.


%% Used for debugging purposes only
dump_ets_table(Name) ->
    io:format("ETS TABLE -- ~p -- ~n", [Name]),
    dump_ets_table(Name, ets:first(Name)).

dump_ets_table(_, '$end_of_table') ->
    io:format("Done.~n");
dump_ets_table(Name, Key) ->
    io:format("~p~n", [ets:lookup(Name, Key)]),
    dump_ets_table(Name, ets:next(Name, Key)).
