AUTHOR
------
Z.Hawkhead


MANDELBROT
----------

Fractal renderer, a selection of fractals are availible from the classic Mandelbrot set to Julia nova and the Newton-Raphson fractal. Fully  parallelised using MPI, but also can be compiled in serial.


PREREQUISITS
------------

This project is written in FORTRAN90. A fortran compiler is required, either gfortran or ifort (version 18 or above). This project is a parallel code using MPI, so a fully compiled version of the MPI libraries is requires for successful install. For serial version, ```COMMS_ARCH:=serial``` in ```Makefile```.


INSTALL
-------

Edit the Makefile to specify the compiler and communications architecture.Compile using ```make```. To install to the '''build/''' directory, use ```make install``` Installation can be cleaned of object files using ```make clean```, to fully clean installation, use ```make clean_all```.

IMAGING
-------

To visualise the calculations, use ```plot_mand.py```. Colours can be altered using commandline flag- either integer 0<i<79, or matplotlib.cm colormap string.

e.g. ```plot_mand.py jet```


EXAMPLES
--------

Example output files can be found in the ```examples/``` directory.
