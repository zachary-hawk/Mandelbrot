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

SERIAL_FC:=ifort


########################################

export COMMS_ARCH
export MPI_FC
export SERIAL_FC

ifeq ($(COMMS_ARCH),mpi)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mandelbrot.mpi $(BUILD_DIR)

.phony: install

clean:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.* $(BUILD_DIR)/mandelbrot.* $(SOURCE)/*.mod



endif 


ifeq ($(COMMS_ARCH),serial)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mandelbrot.serial $(BUILD_DIR)

.phony: install



clean:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.*  $(SOURCE)/*.mod

clean_all:
	rm -f $(BUILD_DIR)/mandelbrot.*
endif


dist:
	tar  --exclude="./.git" --exclude="./test" --exclude="./*/*.mpi" --exclude="./*/*.serial" --exclude="./source/*.o" --exclude="./source/*.mod" -cvf MANDELBROT.tar .
#tar -cvf MANDELBROT.tar source/*.f90 bin build/plot_mand.py examples Makefile README.md Mandelbrot
