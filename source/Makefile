
VAR := $(shell . ./../bin/arch_finder)
ifeq ($(strip $(VAR)),)
VAR="'Unknown'"
endif
#FC=mpif90
FCFLAGS= -cpp -funroll-loops -D arch=$(VAR)
BUILD_DIR ?= ./../build





#Edit this line to Toggle between "mpi" and "serial" 
#COMMS_ARCH:=mpi



ifeq ($(COMMS_ARCH),mpi)


mandelbrot.mpi: Mandelbrot.o Header.o fractal.o
	$(MPI_FC) $(FCFLAGS) -o mandelbrot.mpi Mandelbrot.o Header.o fractal.o -cpp

Header.o: Header.f90
	$(MPI_FC) $(FCFLAGS) -c Header.f90 -cpp

fractal.o: fractal.f90
	$(MPI_FC) $(FCFLAGS) -c fractal.f90 -cpp


Mandelbrot.o: Mandelbrot.f90 Header.o fractal.o
	$(MPI_FC) $(FCFLAGS) -c Mandelbrot.f90 -cpp



endif


ifeq ($(COMMS_ARCH),serial)


mandelbrot.serial: Serial_Mandelbrot.o Header.o fractal.o
	$(SERIAL_FC) $(FCFLAGS) -o mandelbrot.serial Serial_Mandelbrot.o Header.o fractal.o -cpp

Header.o: Header.f90
	$(SERIAL_FC) $(FCFLAGS) -c Header.f90 -cpp

fractal.o: fractal.f90
	$(SERIAL_FC) $(FCFLAGS) -c fractal.f90 -cpp


Serial_Mandelbrot.o: Mandelbrot.f90 Header.o fractal.o
	$(SERIAL_FC) $(FCFLAGS) -c Serial_Mandelbrot.f90 -cpp



endif