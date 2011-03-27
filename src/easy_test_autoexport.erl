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

%% Rewrite the forms list
%% so that the export attribute
%% follows the module declaration
rewrite([{attribute, _, module, _Name}=M | Fs], Exports) ->
    [M, {attribute,0,export,Exports} | Fs];
rewrite([F | Fs], Exports) -> % skip anything before the module declaration in the forms list
    [F | rewrite(Fs, Exports)];
rewrite([], _) -> 
    []. % missing module delcaration failsafe
    
				      
		
