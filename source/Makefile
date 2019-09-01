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
FCFLAGSCPP= -cpp -O1  -funroll-loops -D arch=$(VAR) -D direc=$(DIR) -D cpu=$(CPU) -xHost -p #-ipo  -g
FCFLAGS=  -O1  -funroll-loops -xHost -p #-ipo -g
BUILD_DIR ?= ./../build





#Edit this line to Toggle between "mpi" and "serial" 
#COMMS_ARCH:=mpi



ifeq ($(COMMS_ARCH),mpi)
mandelbrot.mpi: Mandelbrot.o fractal.o colour.o IO.o COMMS_MPI.o trace.o
	 $(MPI_FC) -o mandelbrot.mpi  Mandelbrot.o fractal.o colour.o IO.o COMMS_MPI.o trace.o  $(FCFLAGS)
# Here are the compile steps

Mandelbrot.o:Mandelbrot.f90 fractal.o IO.o COMMS_MPI.o trace.o
	 $(MPI_FC) -c  Mandelbrot.f90 $(FCFLAGS)

fractal.o:./fractal.f90 colour.o IO.o trace.o
	 $(MPI_FC) -c  ./fractal.f90 $(FCFLAGSCPP)

colour.o:./colour.f90 IO.o trace.o
	 $(MPI_FC) -c  ./colour.f90 $(FCFLAGS)


IO.o:./IO.f90 COMMS_MPI.o trace.o
	 $(MPI_FC) -c  ./IO.f90 $(FCFLAGSCPP)

COMMS_MPI.o:./COMMS_MPI.f90 trace.o
	 $(MPI_FC) -c  ./COMMS_MPI.f90 $(FCFLAGS)

trace.o:./trace.f90
	$(MPI_FC) -c ./trace.f90 $(FCFLAGS)



endif


ifeq ($(COMMS_ARCH),serial)

mandelbrot.serial: Mandelbrot.o fractal.o colour.o IO.o COMMS_SERIAL.o
	 $(SERIAL_FC) -o mandelbrot.serial  Mandelbrot.o fractal.o colour.o IO.o COMMS_SERIAL.o 
# Here are the compile steps

Mandelbrot.o:Mandelbrot.f90 fractal.o IO.o COMMS_SERIAL.o
	 $(SERIAL_FC) -c  Mandelbrot.f90 fractal.o IO.o COMMS_SERIAL.o $(FCFLAGS)

fractal.o:./fractal.f90 colour.o IO.o
	 $(SERIAL_FC) -c  ./fractal.f90 $(FCFLAGSCPP)

colour.o:./colour.f90 IO.o
	 $(SERIAL_FC) -c  ./colour.f90 $(FCFLAGS)


IO.o:./IO.f90 COMMS_SERIAL.o
	 $(SERIAL_FC) -c  ./IO.f90 $(FCFLAGSCPP)

COMMS_SERIAL.o:./COMMS_SERIAL.f90
	 $(SERIAL_FC) -c  ./COMMS_SERIAL.f90 $(FCFLAGS)





endif
