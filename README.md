ABOUT HYPOSAT
=============

Hyposat is a file-in file-out utility for localization of seismic events that has been around for many decades
It has previously been shared to those interested via mail or ftp
From version 6.1d, it is available in github

It consists mainly of Fortran code, and is put together by Johannes Shweizer at NORSAR

Build reqoirements
------------------
To build Hyposat, you need:

- A working FOrtran compiler (for example gfortran or intel OneAPI)
- A working C/C++ compiler
- CMake >= 3.1
- A build system generator: For example ninja or make

Tested on windows with Visual Studio 2017 and later and Intel OneAPI
Tested on Linux with gcc and gfortran

Build instructions
------------------

You may use cmake-gui to setup the build environment,
or you can use the cmake CLI...
We recommend to build in a separate build directory to keep the code clean

Example of building with ninja generator in directory hyposat-build:

mkdir hyposat-build
cd hyposat-build/

# Running cmake. Install directory: ../hyposat-install. Source in parent directory ..

cmake -D CMAKE_BUILD_TYPE:String="Release" -D CMAKE_INSTALL_PREFIX:String="../hyposat-install" ..

# Starting build...

cmake --build . --target all

# Installing in ../hyposat-install

ninja install

# Creating install package for installing anywhere...

ninja package
