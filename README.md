# ABOUT HYPOSAT / HYPOMOD

HYPOSAT/ HYPOMOD are file-in file-out utilities for localization of seismic events that have been around for many decades.
It has previously been shared to those interested via mail or ftp.
From version 6.2, it is available in GitHub

The program package consists mainly of Fortran code, and is put together by Johannes Schweitzer at NORSAR since 1997 and before at the Ruhr-Univerity Bochum, Germany. 

Details of the ideas behind HYPOSAT / HYPOMOD and how it developed can be found in the following publications:

Schweitzer, J. (1997). HYPOSAT – a new routine to locate seismic events. NORSAR Scientific Report, 1-97/98, 94-102, doi: 10.21348/p.1997.0014.

Schweitzer, J. (2001). HYPOSAT – an enhanced routine to locate seismic events. Pure and Applied Geophysics, 158, 277-289, doi: 10.1007/PL00001160.

Schweitzer, J. (2002). HYPOSAT/HYPOMOD, User manual for HYPOSAT (including HYPOMOD). PD 11.1 in NMSOP (2002, 2006, 2009) & NMSOP-2 (2012), 
doi: 10.2312/GFZ.NMSOP_r1_PD_11.1 and doi:10.2312/GFZ.NMSOP-2_PD_11.1.

Schweitzer, J. (2006). How can the ISC location procedures be improved? Phys. Earth Planet. Inter., 158, 19-26, doi: 10.1016/j.pepi.2006.03.017.

Schweitzer, J. (2018). User manual for HYPOSAT 6 and HYPOMOD 2. NMSOP-3, PD 11.1, 38 pp., doi: 10.2312/GFZ.NMSOP-3_PD_11.1D

Schweitzer, J. (2025). Travel-time corrections for seismic event locations. J. Geol. Soc. India (J-GSI), 101, 754-758. doi: 10.17491/jgsi/2025/0120040017.

Schweitzer, J. (2025). HYPOSAT 6.2 and HYPOMOD 2.2 - The User Manual. 60 pp., NORSAR, doi: 10.21348/p.2025.0001.


## Inherited source codes

Some program parts were copied from openly available sources, adapted and modified for the usage in HYPOSAT / HYPOMOD. It was used 

		for least squares fit code from the late Prof. Gerhard Müller, University of Frankfurt, Germany.
		for Flinn-Engdahl Regions code published with Young et al. (1996), The Flinn-Engdahl Regionalization Scheme: the 1995 Revision, PEPI, 96, 223-297.
		for 3rd level of seismotectonic units in Europe code from the late Günter Leydecker, BGR, Hannover, Germany.
		for SVD and a set of subroutines code from Press et al. (1992), Numerical Recipes in FORTRAN, Cambridge Univesity Press.
		for the libtau software code distributed with Kennett et al. (1995), Constrain on Seismic Velocities in the Earth from Travel Times, GJI, 122, 108-124.
		for the ellipticity corrections code distributed with Kennett & Gudmundsson (1996), Ellipticity Corrections for Seismic Phases, GJI, 127, 40-48.
		for ISF formatted file i/o code distributed by the ISC (http://www.isc.ac.uk/standards/isf/download/isf_fortran.tar).
		for handling of magnitude correction tables and JSON formatted output internal code from NORSAR.
		for ray tracing in local / regional models and crustal corections code from Schweitzer (2012), LAUFZE / LAUFPS, NMSOP-2, 14pp., doi: 10.2312/GFZ.NMSOP-2_PD_11.2.
		for epochal time handling code from the Center of Seismic Studies (CSS) in Arlington, USA.
		for all handling of Crust 1.0 data code from Gabi Laske (https://igppweb.ucsd.edu/~gabi/crust1.html).
		for an extension of the libtau software to invert ray parameters in epicentral distances code from Harley Benz, USGS, (pers. communication 1999).

For more details about the usage of this software see comments in the source codes in the directory ./msrc.


## Build requirements

To build HYPOSAT / HYPOMOD, you need:

- A working Fortran compiler (for example gfortran or Intel OneAPI)
- A working C/C++ compiler
- CMake. Required for Windows, optional for Linux. There is also a standard Makefile setup for Linux. Tested for CentOS 7. May require modifications for other Linux systems
- A build system generator: For example ninja (See https://ninja-build.org) or make

The Program has been tested on Windows 10 and Windows 11 with Visual Studio 2022 and Intel OneAPI, and on Linux RHEL 7 and RHEL 9 with gcc and gfortran compilers.

The Windows build expects Microsoft C/C++ compilers and Intel OneAPI Fortran compilers.
 
## Build instructions:

You may use cmake-gui to setup the build environment, 
or you can use the cmake CLI...

We recommend to build in a separate build directory to keep the code clean

## Example of building with default generator in directory hyposat-build:

mkdir hyposat-build
cd hyposat-build/

### Running cmake. Install directory: ../hyposat-install. Source in parent directory ..

cmake -D CMAKE_BUILD_TYPE:String="Release" -D CMAKE_INSTALL_PREFIX:String="../hyposat-install" ..

To force a specific generator, you can specify it on the command line:
For example:

cmake -G Ninja CMAKE_BUILD_TYPE:String="Release" -D CMAKE_INSTALL_PREFIX:String="../hyposat-install" ..

When generating an installiation, there are coveniance scripts under the install directory (hyposat.bat, hypomod.bat for windows, hyposat.sh, hypomod.sh for Linux) 
By calling these scripts from withn a Hyposat/Hypomod working directory, HYPOSAT_DATA will be automatically set, and the application executed.

### Running build without installing...

cmake --build . --target all

### Installing in ../hyposat-install

cmake --build . --target install

### Creating a zipped archive for installing anywhere

cmake --build . --target package

