VAR := $(shell ./bin/arch_finder)
FC=mpif90
FCFLAGS= -cpp -funroll-loops -D arch=$(VAR)
BUILD_DIR ?= ./build
SCR_DIR ?= ./scr



mandelbrot.mpi: Mandelbrot.o Header.o fractal.o
	$(FC) $(FCFLAGS) -o mandelbrot.mpi Mandelbrot.o Header.o fractal.o -cpp

Header.o: Header.f90
	$(FC) $(FCFLAGS) -c Header.f90 -cpp

fractal.o: fractal.f90
	$(FC) $(FCFLAGS) -c fractal.f90 -cpp


Mandelbrot.o: Mandelbrot.f90 Header.o fractal.o
	$(FC) $(FCFLAGS) -c Mandelbrot.f90 -cpp