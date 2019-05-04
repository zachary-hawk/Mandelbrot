AUTHOR
------
Z.Hawkhead


MANDELBROT
----------

Parallel code for producing the Mandelbrot set.


PREREQUISITS
------------

This project is written in FORTRAN90. A fortran compiler is required, either gfortran or ifort (version 18 or above). This project is a parallel code using MPI, so a fully compiled version of the MPI libraries is requires for successful install. For serial version, ```COMMS_ARCH:=serial``` in ```Makefile```.


INSTALL
-------

Edit the Makefile to specify the compiler and communications architecture.Compile using ```make```. To install to the '''build/''' directory, use ```make install``` Installation can be cleaned of object files using ```make clean```, to fully clean installation, use ```make cleanall```.


EXAMPLES
--------

Example output files can be found in the ```examples/``` directory.
