#########################################
#   TOP LEVEL MAKEFILE                  #
#########################################

SOURCE = ./source
BUILD_DIR = ./build
subsystem:
	$(MAKE) -C $(SOURCE)


.phony: install
install:
	install -m 557 $(SOURCE)/mandelbrot.mpi $(BUILD_DIR)