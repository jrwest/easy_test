## easy_test

Be more expressive and DRY up (although not so much yet) your Erlang common test suites. See [Wiki](https://github.com/jrwest/easy_test/wiki) for more information.

## Currently Implemented

    * Any function prefixed with 'test_' will be exported
    * use module attribute '-easy_test([{test, Name::atom()}, {init, bool()}}).' to automatically
      export function Name/1. if {init, true} is specified export Name/0 as well
    * automatically write and export the all function from all autoexported tests


## To Be Implemented
 
    * Support groups and group options through a function name prefix and module attribute
    * Try to make suites more DRY (still need to write test names at least twice, but thats still better
      than atleast three times). 
    * Autoexport init_per_[suite | group | testcase]
    * Provide a module attribute to define suite opts and use it to write suite/0


## Credits

Thanks to Richard Carlsson and Mickaël Rémond (authors of [EUnit](http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html)). 
Their code in the eunit_autoexport module forms the basis of this library's functionality.
