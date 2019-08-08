VAR := $(shell . ./../bin/arch_finder)
DIR := $(shell . ./../bin/dir_finder)

OS := $(shell uname -s)

ifeq ($(strip $(OS)),Darwin)
CPU := $(shell . ./../bin/CPU_DARWIN)
endif	

ifeq ($(strip $(OS)),Linux)
CPU := $(shell . ./../bin/CPU_LINUX)
endif

ifeq ($(strip $(VAR)),)
VAR="'Unknown'"
endif
#FC=mpif90
FCFLAGS= -cpp  -funroll-loops -D arch=$(VAR) -D direc=$(DIR) -D cpu=$(CPU) 
BUILD_DIR ?= ./../build





#Edit this line to Toggle between "mpi" and "serial" 
#COMMS_ARCH:=mpi



ifeq ($(COMMS_ARCH),mpi)


mandelbrot.mpi: Mandelbrot.o  fractal.o IO.o COMMS_MPI.o
	$(MPI_FC) $(FCFLAGS) -o mandelbrot.mpi Mandelbrot.o COMMS_MPI.o fractal.o IO.o  -cpp

COMMS_MPI.o: COMMS_MPI.f90
	$(MPI_FC) $(FCFLAGS) -c COMMS_MPI.f90 -cpp


fractal.o: fractal.f90
	$(MPI_FC) $(FCFLAGS) -c fractal.f90 -cpp

IO.o: IO.f90
	$(MPI_FC) $(FCFLAGS) -c IO.f90 -cpp


Mandelbrot.o: Mandelbrot.f90 fractal.o IO.o COMMS_MPI.o
	$(MPI_FC) $(FCFLAGS) -c Mandelbrot.f90 -cpp



endif


ifeq ($(COMMS_ARCH),serial)

mandelbrot.serial: Mandelbrot.o  fractal.o IO.o COMMS_SERIAL.o
	$(SERIAL_FC) $(FCFLAGS) -o mandelbrot.serial Mandelbrot.o COMMS_SERIAL.o  fractal.o IO.o  -cpp

fractal.o: fractal.f90
	$(SERIAL_FC) $(FCFLAGS) -c fractal.f90 -cpp

IO.o: IO.f90
	$(SERIAL_FC) $(FCFLAGS) -c IO.f90 -cpp

COMMS_SERIAL.o: COMMS_SERIAL.f90
	$(SERIAL_FC) $(FCFLAGS) -c COMMS_SERIAL.f90 -cpp


Mandelbrot.o: Mandelbrot.f90  fractal.o IO.o COMMS_SERIAL.o
	$(SERIAL_FC) $(FCFLAGS) -c Mandelbrot.f90 -cpp




endif