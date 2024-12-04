set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

enable_language(Fortran)

include_directories(
)


SET( hyposat_SRCS
    hyposat.f

    gm2.h
    gmi.h
    isf_head.h
    lsq.h
    magpar.h
    modelc.h
    modelg.h
    model.h
    phlist.h
    ref.h
    ttimes.h
    ttlim.h
    crust_10.h

    hyposat_lib.f
    hyposat_geo.f
    hyposat_geotab.f
    hyposat_phase.f
    hyposat_clib.c
    hyposat_loc.f

    hyposat_crust.f
    hyposat_crust_mod.f
    hyposat_mag.f
    hyposat_numr.f
    isf_in.f
    isf_out.f
    libtau_h.f

    hyposat_cross.f
    hyposat_time.f
    hyposat_inv.f
    hyposat_file.f
    hyposat_plane.f
    hyposat_depth.f
    hyposat_out.f

    jsonstorage.cpp
)

add_executable( hyposat ${hyposat_SRCS} )
set_target_properties( hyposat PROPERTIES
    LINKER_LANGUAGE Fortran
)

if (WIN32)
    target_link_libraries( hyposat
        vcruntime
    )
endif (WIN32)



if (BUILD_PYTHON_EXTENSION)
    install(
        TARGETS hyposat
        RUNTIME DESTINATION hyposatpy/bin
    )
else()
    install(
        TARGETS hyposat
        RUNTIME DESTINATION bin
)

endif()
