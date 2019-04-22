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

clean:
	rm $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.mpi

cleanall:
	rm -f $(SOURCE)/*.o $(objects) $(SOURCE)/mandelbrot.mpi $(BUILD_DIR)/mandelbrot.mpi