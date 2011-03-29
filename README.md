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

## Credits

Thanks to Richard Carlsson and Mickaël Rémond (authors of [EUnit](http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html)). 
Their code in the eunit_autoexport module forms the basis of this library's functionality.
