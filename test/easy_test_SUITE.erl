%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(easy_test_SUITE).

-include_lib("easy_test/include/easy_test.hrl").

-export([init_per_group/2, end_per_group/2, init_per_suite/1, end_per_suite/1]).

-easy_group([{group, group_2}, {context, group_1}]).
-easy_group([{group, group_3}, {context, group_4}]).
-easy_test([{test, autoexport_attr_test_function}, {has_config, false}]).
-easy_test([{test, autoexport_attr_with_init}, {has_config, true}]).
-easy_test([{test, grouped_from_test_attr}, {group, group_1}, {has_config, false}]).
-easy_test([{test, explicit_all_group}, {group, all}]). % has config defaults to 'false'
-easy_test([{test, nested_test_1}, {group, group_2}]).
-easy_group([{group, group_4}, {context, group_1}]).
-easy_test([{test, double_nested_test}, {group, group_4}]).
-easy_group([{group, group_5}, {opts, [shuffle, sequence]}, {tests, [group_attr_test]}]).
-easy_group([{group, group_6}, {context, group_2}, {tests, [running_out_of_names_1, running_out_of_names_2]}]).

init_per_suite(Config) ->
    [{global, 0} | Config].

end_per_suite(_) ->
    ok.

init_per_group(group_1, Config) ->
    [{group_1, 1} | Config];
init_per_group(group_2, Config) ->
    [{group_2, 2} | Config];
init_per_group(group_3, Config) ->
    [{group_3, 3} | Config];
init_per_group(group_4, Config) ->
    [{group_4, 4} | Config];
init_per_group(group_5, Config) ->
    [{group_5, 5} | Config];
init_per_group(group_6, Config) ->
    [{group_6, 6} | Config].



end_per_group(_, _) ->
    ok.

%% This function should be autoexported and the test 
%% will pass if it is; if not it will fail
test_autoexport_prefixed_function(_) ->
    does_contain_export({test_autoexport_prefixed_function, 1}, ?MODULE).

test_no_autoexport_non_prefix_function(_) ->
    does_not_contain_export({do_not_export_me, 1}, ?MODULE),
    does_not_contain_export({does_not_contain_export, 2}, ?MODULE).

autoexport_attr_test_function(_) ->
    does_contain_export({autoexport_attr_test_function, 1}, ?MODULE).

    
autoexport_attr_with_init() ->
    [].

autoexport_attr_with_init(_) ->
    does_contain_export({autoexport_attr_with_init, 0}, ?MODULE).

grouped_from_test_attr(Config) ->
    does_contain_export({grouped_from_test_attr, 1}, ?MODULE),    
    1 = ?config(group_1, Config).

explicit_all_group(Config) ->
    does_contain_export({explicit_all_group, 1}, ?MODULE),
    0 = ?config(global, Config).

nested_test_1(Config) ->
    does_contain_export({nested_test_1, 1}, ?MODULE),
    0 = ?config(global, Config),
    1 = ?config(group_1, Config),
    2 = ?config(group_2, Config).

double_nested_test(Config) ->
    does_contain_export({double_nested_test, 1}, ?MODULE),
    0 = ?config(global, Config),
    1 = ?config(group_1, Config),
    4 = ?config(group_4, Config).

group_attr_test(Config) ->
    does_contain_export({group_attr_test, 1}, ?MODULE),
    0 = ?config(global, Config),
    5 = ?config(group_5, Config).    

running_out_of_names_1(Config) ->
    0 = ?config(global, Config),
    1 = ?config(group_1, Config),
    2 = ?config(group_2, Config),
    undefined = ?config(group_4, Config),
    6 = ?config(group_6, Config).

running_out_of_names_2(Config) ->
    0 = ?config(global, Config),
    1 = ?config(group_1, Config),
    2 = ?config(group_2, Config),
    undefined = ?config(group_4, Config),
    6 = ?config(group_6, Config).

does_contain_export(Export, Module) ->
    true = lists:member(Export, Module:module_info(exports)).

does_not_contain_export(Export, Module) ->
    false = lists:member(Export, Module:module_info(exports)).

%% Placeholder for 'test_autoexport_non_prefix_function
do_not_export_me(_) ->
    ok.

