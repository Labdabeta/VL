.SILENT:
BUILD_DIR=../build

default: all
	true

run% : force_make
	../bin/$*.out

% : force_make %.gpr
	gnatmake -P $*.gpr
	# Maybe remove -f? it forces recompilations.
	#cd $(BUILD_DIR); make $@

force_make:
	true
