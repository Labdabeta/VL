.SILENT:
BUILD_DIR=../build

default: all
	true

clean :
	-@rm ../build/* ../bin/*
	-@rm -r html

viewcoverage : force_make
	gcov ../build/.gcno
	lcov --capture --directory ../build/ --output-file test.info
	lcov --remove test.info '*.ads' '*/build/*' '*/lib/*' '*/test/*' -o test.info
	genhtml test.info --output-directory html
	rm test.info
	firefox html/index.html

run% : force_make
	../bin/$*.out

% : force_make %.gpr
	gnatmake -P $*.gpr
	# Maybe remove -f? it forces recompilations.
	#cd $(BUILD_DIR); make $@

force_make:
	true
