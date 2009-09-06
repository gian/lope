ifeq ($(VERBOSE), "")
     VERBOSE := 1
else

endif



all:
	echo "CM.make \"sources.cm\";\nLopeTests.run_all_tests();" | sml 

debug:
	echo "CM.make \"sources.cm\";\nDebug.set_verbosity 2; LopeTests.run_all_tests();" | sml 
