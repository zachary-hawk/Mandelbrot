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
	rm $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.mpi

cleanall:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.* $(BUILD_DIR)/mandelbrot.* *.mod


endif 


ifeq ($(COMMS_ARCH),serial)


subsystem:
	export $(COMMS_ARCH)
	$(MAKE) -C $(SOURCE)
	install -m 557 $(SOURCE)/mandelbrot.serial $(BUILD_DIR)

.phony: install



clean:
	rm $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.serial

cleanall:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.* $(BUILD_DIR)/mandelbrot.* *.mod


endif


dist:
	tar  --exclude="./.git" --exclude="./test" --exclude="./*/*.mpi" --exclude="./*/*.serial" --exclude="./source/*.o" --exclude="./source/*.mod" -cvf MANDELBROT.tar .
#tar -cvf MANDELBROT.tar source/*.f90 bin build/plot_mand.py examples Makefile README.md Mandelbrot