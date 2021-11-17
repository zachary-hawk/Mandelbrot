#########################################
#   TOP LEVEL MAKEFILE                  #
#########################################

SOURCE = ./source
BUILD_DIR = ./build

#########################################
#User editable options

#Communications architectiure: mpi,serial
COMMS_ARCH:=mpi

MPI_FC:=mpif90

F90:=gfortran

PRECISION:=real32

BUILD= fast

########################################

export COMMS_ARCH
export MPI_FC
export F90
export PRECISION
export BUILD
ifeq ($(COMMS_ARCH),mpi)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mandelbrot.mpi $(BUILD_DIR)
	rm -f $(SOURCE)/mandelbrot.mpi

.phony: install

clean:
	rm -f $(SOURCE)/*.o $(objects)  $(BUILD_DIR)/mandelbrot.mpi $(SOURCE)/*.mod



endif 


ifeq ($(COMMS_ARCH),serial)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mandelbrot.serial $(BUILD_DIR)
	rm -f $(SOURCE)/mandelbrot.serial
.phony: install



clean:
	rm -f $(SOURCE)/*.o $(objects)  $(SOURCE)/*.mod


endif
clean_all:
	rm -f $(BUILD_DIR)/mandelbrot.* $(SOURCE)/*.o $(objects)  $(SOURCE)/*.mod

dist:
	tar  --exclude="./.git" --exclude="./test" --exclude="./*/*.mpi" --exclude="./*/*.serial" --exclude="./source/*.o" --exclude="./source/*.mod" -cvf MANDELBROT.tar .
#tar -cvf MANDELBROT.tar source/*.f90 bin build/plot_mand.py examples Makefile README.md Mandelbrot
