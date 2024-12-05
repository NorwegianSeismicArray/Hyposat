# ABOUT HYPOSAT

Hyposat is a file-in file-out utility for localization of seismic events that has been around for many decades
It has previously been shared to those interested via mail or ftp
From version 6.1d, it is available in github

It consists mainly of Fortran code, and is put together by Johannes Shweizer at NORSAR

## Build requirements

To build Hyposat, you need:

- A working FOrtran compiler (for example gfortran or intel OneAPI)
- A working C/C++ compiler
- CMake >= 3.1
- A build system generator: For example ninja or make

Tested on windows with Visual Studio 2017 and later and Intel OneAPI.
Tested on Linux with gcc and gfortran

The windows build expects Microsoft C/C++ compilers and Intel Fortran compilers.
 


## Build instructions

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

