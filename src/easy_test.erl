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

-define(EASY_TESTS_ETS, easy_exports).
-define(EASY_GROUPS_ETS, easy_groups).

-define(EASY_TEST_PREFIX, "test_").
-define(EASY_GROUP_INIT_PREFIX, "init_group_").
-define(EASY_GROUP_DEFAULT_OPTS, [shuffle]).
-define(EASY_GROUP_NO_INIT_FUN, nil).

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
    apply_if_true(HasConfig, fun store_export/2, [Name, 0]);
form({function, _L, Name, 1, _Cs}, TestPrefix) ->
    NameAsList = atom_to_list(Name),
    apply_if_true(lists:prefix(TestPrefix, NameAsList),
	          [fun store_export/2,
		   fun store_test/2],
	          [[Name, 1],
		   [all, Name]]),
    apply_if_true(lists:prefix(?EASY_GROUP_INIT_PREFIX, NameAsList),
		 fun store_group_init/2, [Name, NameAsList]);
form(_, _) ->
    skipped.

store_group_init(FunName, AsList) ->
    {_, GroupAsList} = lists:split(length(?EASY_GROUP_INIT_PREFIX), AsList),
    GroupName = list_to_atom(GroupAsList),
    case does_group_exist(GroupName) of
	true ->
	    store_export(FunName, 1),
	    [{GroupName, Opts, Context, _} | _] = ets:lookup(?EASY_GROUPS_ETS, GroupName),
	    ets:insert(?EASY_GROUPS_ETS, {GroupName, Opts, Context, FunName});
	false ->
	    skipped % if 'group_' function detection is implemented the
		    % skipping here may need to be changed to group creation
    end.

store_export(Name,Arity) ->
    ets:insert(?EASY_TESTS_ETS, {make_ref(), Name, Arity}).


store_test(GroupName, Test) ->
    store_group_if_dne(GroupName),
    GroupSetName = group_table_name(GroupName),
    ets:insert(GroupSetName, {make_ref(), test, Test}).

store_or_update_group(Name, Context, Opts, Tests) ->
    store_or_update_group(Name, Context, Opts, Tests, nil).

store_or_update_group(Name, Context, Opts, Tests, NewInit) ->
    case ets:lookup(?EASY_GROUPS_ETS, Name) of
	[] ->
	    store_group(Name, Context, Opts);
	[{N, _O, OldContext, Init} | _] ->
	    FinalInit = case NewInit of 
			    nil -> Init;
			    NewInit when is_function(NewInit) -> 
				NewInit
			end,
	    move_group(OldContext, Context, Name),
	    ets:insert(?EASY_GROUPS_ETS, {N, Opts, Context, FinalInit})
    end,
    store_group_attr_tests(Name, Tests).

store_group_if_dne(GroupName) ->
    case does_group_exist(GroupName) of
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
    ets:insert(?EASY_GROUPS_ETS, {GroupName, Opts, ParentName, ?EASY_GROUP_NO_INIT_FUN}),
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

does_group_exist(GroupName) when is_atom(GroupName) ->
    lists:member(group_table_name(GroupName), ets:all()).

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

% TODO refactor third arg to be a record
rewrite([{function, _, all, 0, _}=F | Fs], As, {_, ExportGroups, ExportGroupInits}) ->    
    rewrite(Fs, [F | As], {false, ExportGroups, ExportGroupInits});
rewrite([{function, _, groups, 0, _}=F | Fs], As, {ExportAll, _, ExportGroupInits}) ->
    rewrite(Fs, [F | As], {ExportAll, false, ExportGroupInits});
rewrite([{function, _, init_per_group, 2, _}=F | Fs], As, {ExportAll, ExportGroups, _}) ->
    rewrite(Fs, [F | As], {ExportAll, ExportGroups, false});    
rewrite([F | Fs], As, ExportData) -> 
    rewrite(Fs, [F | As], ExportData);
rewrite([], As, {ExportAllFun, ExportGroupsFun, ExportGroupInits}) ->
    Final = write_all(write_groups(As, ExportGroupsFun, ExportGroupInits), ExportAllFun),
    {Final, {ExportAllFun, ExportGroupsFun, ExportGroupInits}}.
	

module_decl(M, Fs) ->
    AllExports = prepare_exports(fetch_table_data(?EASY_TESTS_ETS), []),
    {Fs1, {ExportAllFun, ExportGroupsFun, ExportGroupInits}} = rewrite(Fs, [], {true, true, true}),
    Es = if ExportAllFun -> [{all, 0} | AllExports];
	    true -> AllExports end,
    Es1 = if ExportGroupsFun -> [{groups, 0} | Es];
	     true -> Es end,
    Es2 = if ExportGroupInits -> [{init_per_group, 2} | Es1];
	     true -> Es1 end,
    [M, {attribute,0,export,Es2} | lists:reverse(Fs1)].

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

write_groups(As, false, false) ->
    As;
write_groups(As, true, WriteGroupInit) ->
    Groups = fetch_table_data(?EASY_GROUPS_ETS),
    write_groups(write_groups0(As, Groups), false, WriteGroupInit);
write_groups(As, _, true) ->
    Groups = fetch_table_data(?EASY_GROUPS_ETS),
    Map = fun({Name, _, _, Fun}) -> {Name, Fun} end,
    Filter = fun({_, Fun}) -> Fun =/= ?EASY_GROUP_NO_INIT_FUN end,
    InitFuns = lists:filter(Filter, lists:map(Map, Groups)),
    write_group_init_fun(As, InitFuns).

write_groups0(As, Groups) ->
    [{function,0,groups,0,
      [{clause,0,[],[],[write_groups_data(Groups)]}]} | As].

write_groups_data([]) ->
    {nil, 0};
write_groups_data([{Group, Opts, _, _} | Groups]) ->
    Tests = fetch_table_data(group_table_name(Group)),
    {cons,0,
     {tuple,0,[{atom,0,Group},
	       write_group_opts(Opts),
	       write_test_list(Tests)]},
     write_groups_data(Groups)}.

write_group_opts([]) ->
    {nil, 0};
write_group_opts([{R, T} | Opts]) ->
    {cons,0,
     {tuple,0,[{atom,0,R},
	       {atom,0,T}]},
     write_group_opts(Opts)};
write_group_opts([Opt | Opts]) when is_atom(Opt) ->
    {cons,0,
     {atom,0,Opt},
     write_group_opts(Opts)}.

write_group_init_fun(As, []) -> % dont write init_per_group/2 if there are no init funs
    As;
write_group_init_fun(As, InitFuns) ->
    [{function,0,init_per_group,2,
      write_group_init_clauses(InitFuns)} | As].

write_group_init_clauses(InitFuns) ->
    write_group_init_clauses(InitFuns, []).

write_group_init_clauses([], Acc) ->
    Acc;
write_group_init_clauses([{GroupName, Fun} | InitFuns], Acc) ->
    Clause = {clause,0,
	      [{atom,0,GroupName},{var,0,'Config'}],
	      [],
	      [{call,0,{atom,0,Fun},[{var,0,'Config'}]}]},
    write_group_init_clauses(InitFuns, [Clause | Acc]).
	      
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

apply_if_true(_, [], _) ->
    ok;
apply_if_true(Bool, [F | Fs], [Args | ArgsList]) ->
    apply_if_true(Bool, F, Args),
    apply_if_true(Bool, Fs, ArgsList);
apply_if_true(Bool, F, Args) when is_function(F) andalso is_list(Args) ->
    case Bool of 
	true ->
	    apply(F, Args);
	_ ->
	    false
    end.

%% Used for debugging purposes only
dump_ets_table(Name) ->
    io:format("ETS TABLE -- ~p -- ~n", [Name]),
    dump_ets_table(Name, ets:first(Name)).

dump_ets_table(_, '$end_of_table') ->
    io:format("Done.~n");
dump_ets_table(Name, Key) ->
    io:format("~p~n", [ets:lookup(Name, Key)]),
    dump_ets_table(Name, ets:next(Name, Key)).
