# ABOUT HYPOSAT

HYPOSAT is a file-in file-out utility for localization of seismic events that has been around for many decades.
It has previously been shared to those interested via mail or ftp
From version 6.1e, it is available in GitHub

It consists mainly of Fortran code, and is put together by Johannes Schweitzer at NORSAR since 1997 and before at the Ruhr-Univerity Bochum, Germany. 
Details of the ideas behind HYPOSAT and how it developed can be found in the following publications:

Schweitzer, J. (1997). HYPOSAT – a new routine to locate seismic events. NORSAR Scientific Report, 1-97/98, 94-102, doi: 10.21348/p.1997.0014.

Schweitzer, J. (2001). HYPOSAT – an enhanced routine to locate seismic events. Pure and Applied Geophysics, 158, 277-289, doi: 10.1007/PL00001160.

Schweitzer, J. (2002). HYPOSAT/HYPOMOD, User manual for HYPOSAT (including HYPOMOD). PD 11.1 in NMSOP (2002, 2006, 2009) & NMSOP-2 (2012), doi: 10.2312/GFZ.NMSOP_r1_PD_11.1 and doi:10.2312/GFZ.NMSOP-2_PD_11.1.

Schweitzer, J. (2006). How can the ISC location procedures be improved? Phys. Earth Planet. Inter., 158, 19-26, doi: 10.1016/j.pepi.2006.03.017.

Schweitzer, J. (2018). User manual for HYPOSAT 6 and HYPOMOD 2. NMSOP-3, PD 11.1, doi: 10.2312/GFZ.NMSOP-3_PD_11.1, 38 pp.

Schweitzer, J. (2025). Travel-time corrections for seismic event locations. Accepted for publication by Journal of Geological Society of India (JGSI).

Schweitzer, J. (2025). HYPOSAT 6.2 and HYPOMOD 2.2 - The User Manual. NORSAR, doi: 10.21348/p.2025.0001.

## Build requirements

To build Hyposat, you need:

- A working FOrtran compiler (for example gfortran or intel OneAPI)
- A working C/C++ compiler
- CMake >= 3.1
- A build system generator: For example ninja or make

The Program has been tested on Windows 10 and Windows 11 with Visual Studio 2017 and later Intel OneAPI and on Linux with gcc and gfortran compilers.

The Windows build expects Microsoft C/C++ compilers and Intel Fortran compilers.
 
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

### Running build without installing...

cmake --build . --target all

### Installing in ../hyposat-install

cmake --build . --target install

### Creating install package for installing anywhere...

cmake --build . --target package

