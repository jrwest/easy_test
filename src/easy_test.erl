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
-define(EASY_GROUP_END_PREFIX, "end_group_").
-define(EASY_GROUP_DEFAULT_OPTS, [shuffle]).
-define(EASY_GROUP_NO_INIT_FUN, nil).
-define(EASY_GROUP_NO_END_FUN, nil).

-define(END_GRP_CATCH_ALL_CLAUSE,
	{clause, 0, [{var, 0, '_'}, {var, 0, '_'}], [],
	 [{atom, 0, ok}]}).
-define(END_GRP_CATCH_ALL,
        {function, 0, end_per_group, 2,
         [?END_GRP_CATCH_ALL_CLAUSE]}).

-define(INIT_GRP_CATCH_ALL_CLAUSE,
	 {clause,0,[{var,34,'_'},{var,34,'Config'}],[],[{var,35,'Config'}]}).
-define(INIT_GRP_CATCH_ALL,
	{function,0,init_per_group,2,
	 [?INIT_GRP_CATCH_ALL_CLAUSE]}).

-record(exp_funs, {all=true,
		   groups=true,
		   init_per_suite=false,
		   end_per_suite=false,
		   init_per_group=true,
		   end_per_group=true}).


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
		  fun store_group_init/2, [Name, NameAsList]),
    apply_if_true(lists:prefix(?EASY_GROUP_END_PREFIX, NameAsList),
		  fun store_group_end/2, [Name, NameAsList]);
form(_, _) ->
    skipped.

store_group_init(FunName, AsList) ->
    {_, GroupAsList} = lists:split(length(?EASY_GROUP_INIT_PREFIX), AsList),
    GroupName = list_to_atom(GroupAsList),
    case does_group_exist(GroupName) of
	true ->
	    store_export(FunName, 1),
	    [{GroupName, Opts, Context, _, EndFun} | _] = ets:lookup(?EASY_GROUPS_ETS, GroupName),
	    ets:insert(?EASY_GROUPS_ETS, {GroupName, Opts, Context, FunName, EndFun});
	false ->
	    skipped % if 'group_' function detection is implemented the
		    % skipping here may need to be changed to group creation
    end.

store_group_end(FunName, AsList) ->
    {_, GroupAsList} = lists:split(length(?EASY_GROUP_END_PREFIX), AsList),
    GroupName = list_to_atom(GroupAsList),
    case does_group_exist(GroupName) of
	true ->
	    store_export(FunName, 1),
	    [{GroupName, Opts, Context, InitFun, _} | _] = ets:lookup(?EASY_GROUPS_ETS, GroupName),
	    ets:insert(?EASY_GROUPS_ETS, {GroupName, Opts, Context, InitFun, FunName});
	false ->
	    skipped % see not in store_group_init
    end.

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
	[{N, _O, OldContext, Init, End} | _] ->
	    move_group(OldContext, Context, Name),
	    ets:insert(?EASY_GROUPS_ETS, {N, Opts, Context, Init, End})
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
    ets:insert(?EASY_GROUPS_ETS, {GroupName, Opts, ParentName, ?EASY_GROUP_NO_INIT_FUN, ?EASY_GROUP_NO_END_FUN}),
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

rewrite([{function, _, all, 0, _}=F | Fs], As, ExpFuns) ->    
    rewrite(Fs, [F | As], ExpFuns#exp_funs{all=false});
rewrite([{function, _, groups, 0, _}=F | Fs], As, ExpFuns) ->
    rewrite(Fs, [F | As], ExpFuns#exp_funs{groups=false});
rewrite([{function, _, init_per_group, 2, _}=F | Fs], As, ExpFuns) ->
    rewrite(Fs, [F | As], ExpFuns#exp_funs{init_per_group=false});
rewrite([{function, _, end_per_group, 2, _}=F | Fs], As, ExpFuns) ->
    rewrite(Fs, [F | As], ExpFuns#exp_funs{end_per_group=false});
rewrite([{function, _, init_per_suite, 1, _}=F | Fs], As, ExpFuns) ->
    rewrite(Fs, [F | As], ExpFuns#exp_funs{init_per_suite=true});
rewrite([{function, _, end_per_suite, 1, _}=F | Fs], As, ExpFuns) ->
    rewrite(Fs, [F | As], ExpFuns#exp_funs{end_per_suite=true});
rewrite([F | Fs], As, ExpFuns) ->
    rewrite(Fs, [F | As], ExpFuns);
rewrite([], As, ExpFuns = #exp_funs{all=ExpAll}) ->
    Final = write_all(write_groups(As, ExpFuns), ExpAll),
    {Final, ExpFuns}.
	
module_decl(M, Fs) ->
    AllExports = prepare_exports(fetch_table_data(?EASY_TESTS_ETS), []),
    {Fs1, EF} = rewrite(Fs, [], #exp_funs{}),
    Es = prepend_if_true(EF#exp_funs.groups,
			 {groups, 0},				   
			 prepend_if_true(EF#exp_funs.all, 
					 {all, 0}, 
					 AllExports)),
    Es1 = prepend_if_true(EF#exp_funs.end_per_group,
			  {end_per_group, 2},
			  prepend_if_true(EF#exp_funs.init_per_group,		    
					  {init_per_group, 2},
					  Es)),
    Es2 = prepend_if_true(EF#exp_funs.end_per_suite, 
			  {end_per_suite, 1},
			  prepend_if_true(EF#exp_funs.init_per_suite,
					  {init_per_suite, 1},
					  Es1)),
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

write_groups(As, #exp_funs{groups=false,init_per_group=false}) ->
    As;
write_groups(As, #exp_funs{groups=DoGrp,init_per_group=DoInit,end_per_group=DoEnd}) ->
    Groups = fetch_table_data(?EASY_GROUPS_ETS),
    InitFuns = [{Name, InitFun} || {Name, _, _, InitFun, _} <- Groups, InitFun =/= ?EASY_GROUP_NO_INIT_FUN],
    EndFuns = [{Name, EndFun} || {Name, _, _, _, EndFun} <- Groups, EndFun =/= ?EASY_GROUP_NO_END_FUN],
    prepend_if_true(DoEnd,
		    write_group_end_fun(EndFuns),		   
		    prepend_if_true(DoInit,
				    write_group_init_fun(InitFuns),
				    prepend_if_true(DoGrp, write_groups0(Groups), As))).
      
write_groups0(Groups) ->
    {function,0,groups,0,
     [{clause,0,[],[],[write_groups_data(Groups)]}]}.

write_groups_data([]) ->
    {nil, 0};
write_groups_data([{Group, Opts, _, _, _} | Groups]) ->
    Tests = fetch_table_data(group_table_name(Group)),
    {cons,0,
     {tuple,0,[{atom,0,Group},
	       write_group_opts(Opts),
	       write_test_list(Tests)]},
     write_groups_data(Groups)}.

write_group_opts(Opts) ->
    erl_parse:abstract(Opts).

write_group_init_fun([]) -> % write a catch-all if no init funs exist
    ?INIT_GRP_CATCH_ALL;
write_group_init_fun(InitFuns) ->
    {function,0,init_per_group,2,
     write_group_init_clauses(InitFuns)}.

write_group_end_fun([]) ->
    ?END_GRP_CATCH_ALL;
write_group_end_fun(EndFuns) ->
    {function,0,end_per_group,2,
     write_group_end_clauses(EndFuns)}.

%% TODO these could probably be refactored into one fun
write_group_init_clauses(InitFuns) ->
    write_group_init_clauses(InitFuns, []).

write_group_end_clauses(EndFuns) ->
    write_group_end_clauses(EndFuns, []).

write_group_init_clauses([], Acc) ->
    Acc ++ [?INIT_GRP_CATCH_ALL_CLAUSE];
write_group_init_clauses([{GroupName, Fun} | InitFuns], Acc) ->
    Clause = {clause,0,
	      [{atom,0,GroupName},{var,0,'Config'}],
	      [],
	      [{call,0,{atom,0,Fun},[{var,0,'Config'}]}]},
    write_group_init_clauses(InitFuns, [Clause | Acc]).

write_group_end_clauses([], Acc) ->
    Acc ++ [?END_GRP_CATCH_ALL_CLAUSE];
write_group_end_clauses([{GroupName, Fun} | EndFuns], Acc) ->
    Clause = {clause,0,
	      [{atom,0,GroupName},{var,0,'Config'}],
	      [],
	      [{call,0,{atom,0,Fun},[{var,0,'Config'}]}]},
    write_group_end_clauses(EndFuns, [Clause | Acc]).
	      
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

prepend_if_true(Exp, Elem, List) when is_boolean(Exp) andalso is_list(List)->
    if Exp -> [Elem | List];
       true -> List end.
		   	      
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
