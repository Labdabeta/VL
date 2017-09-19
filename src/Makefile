.SILENT:
BUILD_DIR=../build

default: all
	true

% : force_make
	cd $(BUILD_DIR); make $@

force_make:
	true

# TODO: use gprbuild
