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

-easy_test([{test, autoexport_attr_test_function}, {has_config, false}]).
-easy_test([{test, autoexport_attr_with_init}, {has_config, true}]).
-easy_test([{test, grouped_from_test_attr}, {group, group_1}, {has_config, false}]).
-easy_test([{test, explicit_all_group}, {group, all}, {has_config, false}]).

init_per_suite(Config) ->
    [{global, 0} | Config].

end_per_suite(_) ->
    ok.

init_per_group(group_1, Config) ->
    [{group_1, 1} | Config].

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


does_contain_export(Export, Module) ->
    true = lists:member(Export, Module:module_info(exports)).

does_not_contain_export(Export, Module) ->
    false = lists:member(Export, Module:module_info(exports)).

%% Placeholder for 'test_autoexport_non_prefix_function
do_not_export_me(_) ->
    ok.

