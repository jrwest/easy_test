## easy_test

Be more expressive and DRY up (although not so much yet) your Erlang common test suites. See [Wiki](https://github.com/jrwest/easy_test/wiki) for more information.

## Currently Implemented

    * Any function prefixed with 'test_' will be exported
    * use module attribute '-easy_test([Opt]).' to automatically
      export function Name/1. if {init, true} is specified export Name/0 as well
    * automatically write and export the all/0 function from all autoexported tests
    * use module attribute '-easy_group[Opts]).' or '-easy_test([..., {group, GroupName::atom()}, ...]).' 
      to specifiy common_test groups
    * automatically write and export the groups/0 function from all groups 
      specified in '-easy_test' or '-easy_group'


## To Be Implemented
 
    * Support groups and group options through a function name prefix
    * Try to make suites more DRY (still need to write test names at least twice, but thats still better
      than atleast three times). 
    * Autoexport init_per_[suite | group | testcase] or provide some convention so init_per_* can be written like all/0
    * Provide a module attribute to define suite opts and use it to write suite/0
    * support '-easy_test(...)' Opts in -easy_group
    * refactor to use erl_parse:abstract instead of explicitly writing each update to the AST. 

## Example

Below is a common test suite and a possible conversion using easy_test. Because easy_test is flexible there are other ways to achieve the same result.

    # common_test suite
    -module(a_SUITE).
    -include_lib("common_test/include/ct.hrl").
    -export([all/0, groups/0]).
    -export([t1/0, t1/1, t2/1, t3/3]).
    
    groups() ->
      [{g1,
       [shuffle, sequence],
       [t2, t3]}].
       
    all() ->
      [{group, g1}, t1].
      
    t1() ->
      [{required, some_var}].
      
    t1(_) ->
      ...
      
    t2(_) ->
      ...
      
    t3(_) ->
      ...
      
    # equivalent easy_test suite
    -module(a_SUITE).
    -include_lib("easy_test/include/easy_test.hrl").
    
    -easy_group([{group, g1}, {opts, [shuffle, sequence]}, {tests, [t2, t3]}]).
    -easy_test([{test, t1}, {has_config, true}]).
      
    t1() ->
      [{required, some_var}].
      
    t1(_) ->
      ...
      
    t2(_) ->
      ...
      
    t3(_) ->
      ...

## Credits

Thanks to Richard Carlsson and Mickaël Rémond (authors of [EUnit](http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html)). 
Their code in the eunit_autoexport module forms the basis of this library's functionality.

## Contributing to easy_test

* Fork the project
* Start a feature/bugfix branch
* Commit and push until your contribution is prepared
* Please add test cases for your code and make sure all tests pass
* Unless necessary do not edit src/easy_test.app.src
  * If you do edit this file please explain why in your pull request
* Submit a pull request

If you submit a pull request (whether it is accepted or not) and would like to become a contributor on the project please note this the request or seperate message and you will be added.     

## Copyright

Copyright (c) 2011 Jordan West, Richard Carlsson. See LICENSE.txt for further detais.
