MAKE_ARGS=VERBOSE=1
FCC=gfortran
FFLAGS= -ffree-line-length-none -cpp -g -fcheck=all,no-array-temps -pedantic -fbacktrace 

NWTC_SRC= ../modules/version/src/VersionInfo.f90 ../modules/nwtc-library/src/NWTC_Library_Types.f90 ../modules/nwtc-library/src/SingPrec.f90 ../modules/nwtc-library/src/NWTC_Base.f90 ../modules/nwtc-library/src/SysGnuLinux.f90 ../modules/nwtc-library/src/NWTC_IO.f90

MAIN= test_input.f90

all: compile test


compile: prog $(BUILD_DIR)

prog: $(NWTC_SRC) $(MAIN)
	$(FCC) $(FFLAGS) $(MAIN)  $(NWTC_SRC) -o prog

	
clean:
	rm *.o

test:
	@echo "------------------------------------------------------------"
	@./prog 
	@echo "------------------------------------------------------------"
	

