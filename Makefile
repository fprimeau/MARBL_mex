# This Makefile uses gfortran to build the MARBL library with the -fPIC flag
# The mex target then uses these object files when building the marbl interface
# for use with mex / Matlab
#
# Assumes that MARBL is available in the directory marbl parallel to
# marbl-interface/. If this is not the case, run
# $ make MARBL_ROOT=[marbl directory]


#MARBL_ROOT=../marbl
MARBL_ROOT=../MARBL-stable
MARBL_ROOT_FULL=$(realpath $(MARBL_ROOT))
LIB_DIR=$(realpath .)/marbl_lib
MARBL_LIB=marbl_lib/libmarbl.a
SO_INTERFACE=marbl_lib/marbl_interface.so
INTERFACE_SRC=marbl_interface_wrapper_mod.F90
INTERFACE_OBJ=$(INTERFACE_SRC:.F90=.o)
MEX_INTERFACE=$(INTERFACE_SRC:.F90=.mexa64)
MEX_DRIVER_SRC=mex_marbl_driver.F90
MEX_DRIVER=$(MEX_DRIVER_SRC:.F90=.mexa64)

#############
#  TARGETS  #
#############

# By default, build the mex driver included as an example
all: mex

# The mex driver should be rebuilt if the mex interface changes or if the
# driver source code changes
$(MEX_DRIVER): $(MEX_INTERFACE) $(MEX_DRIVER_SRC)
	mex -Imarbl_include $(MEX_DRIVER_SRC) $(INTERFACE_SRC) marbl_include/*.o 

# Shortcut for building the mex driver: "$ make mex"
.PHONY: mex
mex: $(MEX_DRIVER)

# The mex interface should be rebuilt if the MARBL library changes or if
# the interface source code changes
$(MEX_INTERFACE): $(MARBL_LIB) $(INTERFACE_SRC)
	mex -Imarbl_include $(INTERFACE_SRC) -Lmarbl_lib -lmarbl -c

$(SO_INTERFACE): $(MARBL_LIB) $(INTERFACE_SRC)
	gfortran -Imarbl_include -fPIC -c $(INTERFACE_SRC)
	gfortran -Lmarbl_lib -shared -o $(SO_INTERFACE) $(INTERFACE_OBJ) -lmarbl

# Short target for building the .so target
libso: $(SO_INTERFACE)

# The marbl library should be rebuilt if any of the MARBL fortran files change
$(MARBL_LIB): $(wildcard $(MARBL_ROOT_FULL)/src/*.F90)
	cd marbl_include ; pwd ; ls ../marbl_lib ; $(MAKE) -f $(MARBL_ROOT_FULL)/src/Makefile FC=gfortran FCFLAGS="-fPIC" USE_DEPS=TRUE OBJ_DIR=. INC_DIR=. LIB_DIR=../marbl_lib ../$(MARBL_LIB) ; cd ..

# Here's an easy way to build just the MARBL library: "$ make lib"
# (Instead of "$ make marbl_lib/libmarbl.a")
.PHONY: lib
lib: $(MARBL_LIB)

# Clean up just the interface files with "$ make clean"
.PHONY: clean
clean:
	rm -f *.mod *.mexa64 *.o *.pyc $(SO_INTERFACE)

# Or use "$ make allclean" to clean up the interface files and the MARBL library
.PHONY: allclean
allclean: clean
	rm -f marbl_include/*.mod marbl_include/*.o marbl_include/*.d $(MARBL_LIB)

