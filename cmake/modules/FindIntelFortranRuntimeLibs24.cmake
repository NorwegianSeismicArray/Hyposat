# The Intel Fortran DLLs are not found by the install command for some reason... Find them here...
# Checks in alternative location, as having multiple installations might put libraries in other places...
# For Intel Fortran 2024

string(TOLOWER "${CMAKE_BUILD_TYPE}" _buildtype)
string(TOLOWER "${CMAKE_CONFIGURATION_TYPES}" _buildtype2)

set(FORTRAN_COMPILER_LIB_PATH $ENV{IFORT_COMPILER24})
get_filename_component(FORTRAN_COMPILER_LIB_PATH "${FORTRAN_COMPILER_LIB_PATH}" REALPATH)
#string(REGEX REPLACE "\\\\" "/" FORTRAN_COMPILER_LIB_PATH ${FORTRAN_COMPILER_LIB_PATH})
set(FORTRAN_COMPILER_LIB_PATH ${FORTRAN_COMPILER_LIB_PATH}/bin)

#set(FORTRAN_IFCONSOL_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/ifconsol.dll")

if (_buildtype STREQUAL "debug" OR _buildtype2 STREQUAL "debug")
    set(FORTRAN_IFCORE_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/libifcoremdd.dll")
else()
    set(FORTRAN_IFCORE_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/libifcoremd.dll")
endif()

set(FORTRAN_IFPORT_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/libifportmd.dll")

if (_buildtype STREQUAL "debug" OR _buildtype2 STREQUAL "debug")
    set(FORTRAN_M_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/libmmdd.dll")
else()
    set(FORTRAN_M_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/libmmd.dll")
endif()

#set(FORTRAN_IRC_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/libirc.dll")

set(FORTRAN_SVML_DISP_RT_LIB "${FORTRAN_COMPILER_LIB_PATH}/svml_dispmd.dll")

set(INTEL_FORTRAN_RUNTIME_LIBS ${FORTRAN_IFCONSOL_RT_LIB} ${FORTRAN_IFCORE_RT_LIB} ${FORTRAN_IFPORT_RT_LIB} ${FORTRAN_M_RT_LIB} ${FORTRAN_IRC_RT_LIB} ${FORTRAN_SVML_DISP_RT_LIB})
