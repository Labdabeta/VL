# Meta targets
.PHONY: all clean test runtest
.SILENT:

all: test

# Location to store coverage file
COVERAGE_FILE=../src/coverage.gcov

# Source directory names
SOURCE_DIRS=$(shell find -L . -type d)
CSOURCES=$(wildcard c/*.c)
COBJECTS=$(patsubst %.c,%.o,$(CSOURCES))

# Basic GNAT flag setup
GNATLINKFLAGS=`sdl2-config --libs` -lSDL2_image -lSDL2_ttf -lSDL2_mixer
GNATBINDFLAGS=
GNATMAKEFLAGS=

# Basic C flag setup
CFLAGS=`sdl2-config --cflags`

GNATMAKEFLAGS+=-d # Display progress
GNATMAKEFLAGS+=-eL # Follow symbolic links
GNATMAKEFLAGS+=-F # Use full pathnames (simplifies vim :make)
GNATMAKEFLAGS+=-gnatU # Tag errors with "error:"
GNATMAKEFLAGS+=-gnatVa # Enable validity checking
GNATMAKEFLAGS+=-gnatef # Use full pathnames in errors too
GNATMAKEFLAGS+=-gnatwe # Warnings are errors
GNATMAKEFLAGS+=-gnaty4aAbCdefhiklL12mM80noprStux # Mandate style guide
# Style flags are:
# 4 - Proper indentation with tabs set at 4 spaces
# a - Proper attribute casing
# A - Clear array attributes (no index on 1D, full index on nD)
# b - No blanks at the end of statements
# C - Comments don't suck (and need 1 space)
# d - No DOS EOLs
# e - Ends/Exits must have labels
# f - No form feeds or vtabs
# h - No TAB characters
# i - If then layout must be decent
# k - All lowercase keywords
# l - Proper layout
# L12 - Can't nest deeper than 12 levels of nesting
# m - Sufficiently short lines
# M80 - Maximum line length is 80 characters
# n - Standard types are appropriately cased
# o - Subprograms are in alphabetical order
# p - Proper pragma casing
# r - Identifiers must stay the same case
# S - No statements on the same line as then or else
# -s - Everything has a spec (currently omitted)
# t - Super picky token spacing
# u - No unnecessary blank lines
# x - No extra parenthesis (especially if (x) style)

INCLUDE_DIRS=$(patsubst %,-I%,$(SOURCE_DIRS))

GNATMAKE=gnatmake
GNATLINK=--GNATLINK="gnatlink $(GNATLINKFLAGS)"
GNATBIND=--GNATBIND="gnatbind $(GNATBINDFLAGS)"
GNATOPTS=$(GNATLINK) $(GNATBIND) $(GNATMAKEFLAGS)

################################################################################
# TARGETS                                                                      #
################################################################################

# Test Executable
TEST_SRC=test/main.adb
TEST_EXE=../bin/test.out
test : $(TEST_EXE)
test : GNATMAKEFLAGS+=-g

$(TEST_EXE) : $(COBJECTS) force_make
	$(GNATMAKE) $(TEST_SRC) -D. $(INCLUDE_DIRS) -o $@ $(GNATMAKEFLAGS)

runtest :
	$(TEST_EXE)


################################################################################
# ADDITIONAL TARGETS     												       #
################################################################################

%.o : %.c
	gcc $(CFLAGS) -c $< -o $@

COVERED_FILES=$(filter-out b~%,$(wildcard *.gcda))
COVERED_SOURCES=$(patsubst %.gcda,%.gcno,$(COVERED_FILES))
coverage :
	gcov $(COVERED_SOURCES)
	cat *.gcov > $(COVERAGE_FILE)


################################################################################
# METATARGETS                                                                  #
################################################################################

clean :
	-rm -f *.ali
	-rm -f *.o
	-rm -f *.gcno
	-rm -f *.gcda

force_make:
	true
