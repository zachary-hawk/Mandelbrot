
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


.phony: install
install:
	install -m 557 $(SOURCE)/mandelbrot.mpi $(BUILD_DIR)

clean:
	rm $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.mpi

cleanall:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.mpi $(BUILD_DIR)/mandelbrot.mpi


endif 


ifeq ($(COMMS_ARCH),serial)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)


.phony: install
install:
	install -m 557 $(SOURCE)/mandelbrot.serial $(BUILD_DIR)

clean:
	rm $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.serial

cleanall:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.serial $(BUILD_DIR)/mandelbrot.serial


endif