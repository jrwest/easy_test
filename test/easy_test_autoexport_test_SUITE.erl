%%%-------------------------------------------------------------------
%%% @author Jordan West <jordanrw@gmail.com>
%%% @copyright (C) 2011, Jordan West
%%% @doc
%%%
%%% @end
%%% Created : 26 Mar 2011 by Jordan West <jordanrw@gmail.com>
%%%-------------------------------------------------------------------
-module(easy_test_autoexport_test_SUITE).

-include_lib("easy_test/include/easy_test.hrl").

-easy_test([{test, autoexport_attr_test_function}, {init, false}]).
-easy_test([{test, autoexport_attr_with_init}, {init, true}]).


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

does_contain_export(Export, Module) ->
    true = lists:member(Export, Module:module_info(exports)).

does_not_contain_export(Export, Module) ->
    false = lists:member(Export, Module:module_info(exports)).

%% Placeholder for 'test_autoexport_non_prefix_function
do_not_export_me(_) ->
    ok.

