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
FCFLAGSCPP= -cpp  -D arch=$(VAR) -D direc=$(DIR) -D cpu=$(CPU)  -p -O3 -funroll-loops -g -fcheck=all -Wall
FCFLAGS= -O3  -funroll-loops -p -g -fcheck=all -Wall
BUILD_DIR ?= ./../build



ifeq ($(BUILD),fast)
OPT= '"FAST"'
ifeq ($(F90),gfortran)
FCFLAGS= -fconvert=big-endian -fno-realloc-lhs -fopenmp -fPIC -O3 -funroll-loops -fno-signed-zeros -g -fbacktrace -march=native -mno-avx -fbackslash
endif
ifeq ($(F90),ifort)
FCFLAGS=-convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -O3 -debug minimal -traceback -xHost -fbackslash
endif

endif

ifeq ($(BUILD),debug)
OPT= '"DEBUG"'
ifeq ($(F90),gfortran)
FCFLAGS= -static-libgfortran -Ddebug -O0 -g -fbounds-check -fbacktrace -Wall -Waliasing -Wsurprising -Wline-truncation -Wno-tabs -Wno-uninitialized -Wno-unused-dummy-argument -Wno-unused -Wno-character-truncation -Wl,-no_pie -fbackslash
endif
ifeq ($(F90),ifort)
FCFLAGS= -convert big_endian -assume norealloc_lhs -diag-disable 8290 -diag-disable 8291 -qopenmp -fPIC -Ddebug -O0 -g -debug extended -traceback -C -stand f03 -warn all -diag-disable 7025 -diag-disable 7712 -diag-disable 5268 -diag-disable 7847 -diag-disable 7893 -diag-disable 6439 -fbackslash
endif
endif




#Edit this line to Toggle between "mpi" and "serial" 
#COMMS_ARCH:=mpi



ifeq ($(COMMS_ARCH),mpi)
mandelbrot.mpi: Mandelbrot.o fractal.o colour.o IO.o comms_mpi.o trace.o
	 $(MPI_FC) -o mandelbrot.mpi  Mandelbrot.o fractal.o colour.o IO.o comms_mpi.o trace.o  $(FCFLAGS)
# Here are the compile steps

Mandelbrot.o:Mandelbrot.f90 fractal.o IO.o comms_mpi.o trace.o
	 $(MPI_FC) -c  Mandelbrot.f90 $(FCFLAGS)

fractal.o:./fractal.f90 colour.o IO.o trace.o
	 $(MPI_FC) -c  ./fractal.f90 $(FCFLAGSCPP)

colour.o:./colour.f90 IO.o trace.o
	 $(MPI_FC) -c  ./colour.f90 $(FCFLAGS)


IO.o:./IO.f90 comms_mpi.o trace.o
	 $(MPI_FC) -c  ./IO.f90 $(FCFLAGSCPP) -Dcompiler_kind=$(PRECISION)

comms_mpi.o:./comms_mpi.f90 trace.o
	 $(MPI_FC) -c  ./comms_mpi.f90 $(FCFLAGS)

trace.o:./trace.f90
	$(MPI_FC) -c ./trace.f90 $(FCFLAGS) 



endif


ifeq ($(COMMS_ARCH),serial)
mandelbrot.serial: Mandelbrot.o fractal.o colour.o IO.o comms_serial.o trace.o
	 $(F90) -o mandelbrot.serial  Mandelbrot.o fractal.o colour.o IO.o comms_serial.o trace.o  $(FCFLAGS)
# Here are the compile steps

Mandelbrot.o:Mandelbrot.f90 fractal.o IO.o comms_serial.o trace.o
	 $(F90) -c  Mandelbrot.f90 $(FCFLAGS)

fractal.o:./fractal.f90 colour.o IO.o trace.o
	 $(F90) -c  ./fractal.f90 $(FCFLAGSCPP)

colour.o:./colour.f90 IO.o trace.o
	 $(F90) -c  ./colour.f90 $(FCFLAGS)


IO.o:./IO.f90 comms_serial.o trace.o
	 $(F90) -c  ./IO.f90 $(FCFLAGSCPP) -Dcompiler_kind=$(PRECISION)

comms_serial.o:./comms_serial.f90 trace.o
	 $(F90) -c  ./comms_serial.f90 $(FCFLAGS)

trace.o:./trace.f90
	$(F90) -c ./trace.f90 $(FCFLAGS)



endif

