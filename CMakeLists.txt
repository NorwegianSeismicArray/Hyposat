cmake_minimum_required (VERSION 3.26.0)
project(Hyposat)
enable_language(Fortran)


####################################################################################################################
# Configure build type
####################################################################################################################
if (WIN32)
    IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE Release
        CACHE STRING "Choose the type of build : None Debug Release RelWithDebInfo MinSizeRel Coverage."
        FORCE)
    ENDIF(NOT CMAKE_BUILD_TYPE)
else (WIN32)
    IF(NOT CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE Debug
        CACHE STRING "Choose the type of build : None Debug Release RelWithDebInfo MinSizeRel Coverage."
        FORCE)
    ENDIF(NOT CMAKE_BUILD_TYPE)
endif(WIN32)
message(STATUS "Current build type is: ${CMAKE_BUILD_TYPE}")


####################################################################################################################
# Set up C++ compiler/linker
####################################################################################################################
if (UNIX)
    message(STATUS "")
    message(STATUS "Configuring C++ compiler and linker for Linux using GCC ..")

    # Additional options
    SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3")

    # Warning option
    SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")

    # Hyposat specific linker flags
    SET( CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -s -lstdc++")
else(UNIX)
	message(STATUS "Configuring C++ compiler and linker for Windows using Microsoft C/C++ compiler ..")
    SET( CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ")
endif (UNIX)

message(STATUS "")
message(STATUS "CMAKE_C_FLAGS: ${CMAKE_C_FLAGS}")
message(STATUS "CMAKE_EXE_LINKER_FLAGS: ${CMAKE_EXE_LINKER_FLAGS}")
message(STATUS "")

set (HYPOSAT_VERSION_MAJOR 6)
set (HYPOSAT_VERSION_MINOR 2)
set (HYPOSAT_VERSION_PATCH 0)



####################################################################################################################
# Set up Fortran compiler/linker
####################################################################################################################
if (UNIX)
    message(STATUS "")
    message(STATUS "Configuring Fortran compiler and linker for Linux using GNU Fortran compiler ..")

    # Hyposat specific fortran flags
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-automatic -fno-backslash -std=legacy -std=gnu -fall-intrinsics ")

    # Additional options
    SET( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Og -ffixed-line-length-132")

    # Warning options
    SET( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -fbounds-check")
else (UNIX)
    message(STATUS "")
    message(STATUS "Configuring Intel Fortran compiler and linker for windows ..")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /threads /extend-source:132 /assume:underscore /names:lowercase")
endif (UNIX)

message(STATUS "")
message(STATUS "CMAKE_Fortran_COMPILER: ${CMAKE_Fortran_COMPILER}")
message(STATUS "CMAKE_Fortran_FLAGS: ${CMAKE_Fortran_FLAGS}")
message(STATUS "CMAKE_Fortran_LINK_EXECUTABLE: ${CMAKE_Fortran_LINK_EXECUTABLE}")
message(STATUS "")


set(ROOT_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/..)


####################################################################################################################
# Configure output build directory for standard
# executables and libraries
####################################################################################################################
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")



message(STATUS "--------------------------------------------------------------------------------------------------")
message(STATUS "Adding msrc subdirectory..")
add_subdirectory(msrc)
message(STATUS "")
message(STATUS "--------------------------------------------------------------------------------------------------")


install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/data/" DESTINATION data)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/examples/" DESTINATION examples)
install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE" DESTINATION . )

if(WIN32)
  install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/hyposat.bat" DESTINATION . )
  install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/hypomod.bat" DESTINATION . )
else()
  install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/hyposat.sh" DESTINATION . PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
  install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/hypomod.sh" DESTINATION . PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE )
endif()


if (WIN32)
    message("\n-- Install Intel Fortran libraries...")
    set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}\\cmake\\modules )
    message(STATUS "CMAKE_MODULE_PATH= ${CMAKE_MODULE_PATH} ")
    find_package(IntelFortranRuntimeLibs24)
    message(STATUS "INTEL_FORTRAN_RUNTIME_LIBS=${INTEL_FORTRAN_RUNTIME_LIBS}")
    install(FILES ${INTEL_FORTRAN_RUNTIME_LIBS} DESTINATION bin )
endif()

####################################################################################################################
# Configure CPack
####################################################################################################################
message(STATUS "--------------------------------------------------------------------------------------------------")
message(STATUS "Configuring CPack..")

set(CPACK_PACKAGE_NAME "Hyposat")
set(CPACK_PACKAGE_VENDOR "NORSAR")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "NORSAR Hyposat")
set(CPACK_PACKAGE_VERSION_MAJOR "${HYPOSAT_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${HYPOSAT_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${HYPOSAT_VERSION_PATCH}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")

set(HYPOSAT_EXTERNAL_VERSION "${HYPOSAT_VERSION_MAJOR}.${HYPOSAT_VERSION_MINOR}.${HYPOSAT_VERSION_PATCH}")

set(CPACK_PACKAGE_EXECUTABLES
    "hyposat" "Hyposat executable"
    "hypomod" "Hypomod executable"
)

if(WIN32)
    # There is a bug in NSI that does not handle full unix paths properly. Make
    # sure there is at least one set of four (4) backslashes.
    set(CPACK_PACKAGE_FILE_NAME "Hyposat")
    set(CPACK_GENERATOR "ZIP")
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME} ${HYPOSAT_EXTERNAL_VERSION}")
    set(CPACK_CREATE_DESKTOP_LINKS "${CPACK_PACKAGE_NAME} ${HYPOSAT_EXTERNAL_VERSION}")
    set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL "ON")
else()
    set(CPACK_GENERATOR STGZ)
    set(CPACK_PACKAGE_INSTALL_DIRECTORY "Hyposat")
    set(CPACK_PACKAGE_FILE_NAME "Hyposat")
endif()

include(CPack)
