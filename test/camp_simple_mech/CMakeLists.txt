CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
PROJECT(band_camp)
ENABLE_LANGUAGE(Fortran)

SET(CMAKE_MACOSX_RPATH 1)

######################################################################
# NetCDF

find_path(NETCDF_INCLUDE_DIR netcdf.mod NETCDF.mod
  DOC "NetCDF include directory (must contain netcdf.mod)"
  PATHS
  $ENV{NETCDF_HOME}/include
  /usr/lib/gfortran/modules
  /usr/lib64/gfortran/modules
  /opt/local/include)
find_library(NETCDF_C_LIB netcdf
  DOC "NetCDF C library"
  PATHS $ENV{NETCDF_HOME}/lib $ENV{NETCDF_HOME}/lib64 /opt/local/lib)
find_library(NETCDF_FORTRAN_LIB netcdff
  DOC "NetCDF Fortran library"
  PATHS $ENV{NETCDF_HOME}/lib $ENV{NETCDF_HOME}/lib64 /opt/local/lib)
set(NETCDF_LIBS ${NETCDF_C_LIB})
if(NETCDF_FORTRAN_LIB)
  set(NETCDF_LIBS ${NETCDF_LIBS} ${NETCDF_FORTRAN_LIB})
endif()
include_directories(${NETCDF_INCLUDE_DIR})

######################################################################
# GSL

find_path(GSL_INCLUDE_DIR gsl/gsl_math.h
  DOC "GSL include directory (must have gsl/ subdir)"
  PATHS $ENV{GSL_HOME}/include /opt/local/include)
find_library(GSL_LIB gsl
  DOC "GSL library"
  PATHS $ENV{GSL_HOME}/lib /opt/local/lib)
find_library(GSL_CBLAS_LIB gslcblas
  DOC "GSL CBLAS library"
  PATHS $ENV{GSL_HOME}/lib /opt/local/lib)
find_library(M_LIB m
  DOC "standard C math library")
set(GSL_SRC src/rand_gsl.c)
set(GSL_LIBS ${GSL_LIB} ${GSL_CBLAS_LIB} ${M_LIB})
include_directories(${GSL_INCLUDE_DIR})
add_definitions(-DPMC_USE_GSL)

######################################################################
# SUNDIALS

find_path(SUITE_SPARSE_INCLUDE_DIR klu.h
  DOC "SuiteSparse include directory (must have klu.h)"
  PATHS $ENV{SUITE_SPARSE_HOME}/include $ENV{SUNDIALS_HOME}/include
        /opt/local/include /usr/local/include)
find_library(SUITE_SPARSE_KLU_LIB klu
  DOC "SuiteSparse klu library"
  PATHS $ENV{SUITE_SPARSE_HOME}/lib $ENV{SUNDIALS_HOME}/lib
        /opt/local/lib /usr/local/lib)
find_path(SUNDIALS_INCLUDE_DIR cvode/cvode.h
  DOC "SUNDIALS include directory (must have cvode/, sundials/, nvector/ subdirs)"
  PATHS $ENV{SUNDIALS_HOME}/include /opt/local/include /usr/local/include)
find_library(SUNDIALS_NVECSERIAL_LIB sundials_nvecserial
  DOC "SUNDIALS serial vector library"
  PATHS $ENV{SUNDIALS_HOME}/lib /opt/local/lib /usr/local/lib)
find_library(SUNDIALS_CVODE_LIB sundials_cvode
  DOC "SUNDIALS CVODE library"
  PATHS $ENV{SUNDIALS_HOME}/lib /opt/local/lib /usr/local/lib)
find_library(SUNDIALS_KLU_LIB sundials_sunlinsolklu
  DOC "SUNDIALS KLU library"
  PATHS $ENV{SUITE_SPARSE_HOME}/lib $ENV{SUNDIALS_HOME}/lib
        /opt/local/lib /usr/local/lib)
find_library(SUNDIALS_SUNMATRIX_SPARSE_LIB sundials_sunmatrixsparse
  DOC "SUNDIALS SUNMatrixSparse library"
  PATHS $ENV{SUITE_SPARSE_HOME}/lib $ENV{SUNDIALS_HOME}/lib
        /opt/local/lib /usr/local/lib)
set(SUNDIALS_LIBS ${SUNDIALS_NVECSERIAL_LIB} ${SUNDIALS_CVODE_LIB}
  ${SUITE_SPARSE_KLU_LIB} ${SUNDIALS_KLU_LIB} ${SUNDIALS_SUNMATRIX_SPARSE_LIB})
set(SUNDIALS_SRC src/condense_solver.c)
include_directories(${SUNDIALS_INCLUDE_DIR} ${SUITE_SPARSE_INCLUDE_DIR})
add_definitions(-DPMC_USE_SUNDIALS)

######################################################################
# json-fortran

find_path(JSON_INCLUDE_DIR json_module.mod
  DOC "json-fortran include directory (must include json_*.mod files)"
  PATHS $ENV{JSON_FORTRAN_HOME}/lib /opt/local/lib /usr/local/lib
        /usr/local/jsonfortran-gnu-6.1.0/lib)
find_library(JSON_LIB jsonfortran
  DOC "json-fortran library"
  PATHS $ENV{JSON_FORTRAN_HOME}/lib /opt/local/lib /usr/local/lib
        /usr/local/jsonfortran-gnu-6.1.0/lib)
include_directories(${JSON_INCLUDE_DIR})
add_definitions(-DPMC_USE_JSON)

######################################################################
# Find the PartMC library

FIND_PATH(PARTMC_INCLUDE_DIR pmc_camp_core.mod
  DOC "PartMC include directory (must contain camp_core.mod)"
  PATHS $ENV{PARTMC_ROOT})
FIND_LIBRARY(PARTMC_LIB partmc
  DOC "PartMC library"
  PATHS $ENV{PARTMC_ROOT})
INCLUDE_DIRECTORIES(${PARTMC_INCLUDE_DIR})

######################################################################
# CCPP directories

FIND_PATH(CCPP_ROOT scripts/ccpp_capgen.py
          DOC "CCPP-framework root directory"
          PATH $ENV{CCPP_ROOT})
SET(CCPP_CAP_FILES ${CMAKE_BINARY_DIR}/ccpp CACHE
    STRING "Location of CCPP-generated cap files")
SET(CCPP_FRAMEWORK ${CCPP_ROOT}/scripts)

######################################################################
# BandCAMP

SET(HOST "band_camp")
ADD_EXECUTABLE(${HOST} ../band_camp_host.F90)
TARGET_SOURCES(${HOST} PUBLIC ../../src/band_camp_scheme.F90 band_camp_mod.F90)
TARGET_LINK_LIBRARIES(${HOST} ${PARTMC_LIB} ${NETCDF_LIBS} ${SUNDIALS_LIBS}
  ${GSL_LIBS} ${JSON_LIB})

if (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  ADD_COMPILE_OPTIONS(-fcheck=all)
  ADD_COMPILE_OPTIONS(-fbacktrace)
  ADD_COMPILE_OPTIONS(-ffpe-trap=zero)
  ADD_COMPILE_OPTIONS(-finit-real=nan)
  ADD_COMPILE_OPTIONS(-ggdb)
  ADD_COMPILE_OPTIONS(-ffree-line-length-none)
  ADD_COMPILE_OPTIONS(-cpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug extended)
  ADD_COMPILE_OPTIONS(-fpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "PGI")
  ADD_COMPILE_OPTIONS(-g)
  ADD_COMPILE_OPTIONS(-Mipa=noconst)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-Mfree)
  ADD_COMPILE_OPTIONS(-Mfptrap)
  ADD_COMPILE_OPTIONS(-Mpreprocess)
else (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  message (FATAL_ERROR "This program has only been compiled with gfortran, pgf90 and ifort. If another compiler is needed, the appropriate flags must be added in ${CMAKE_SOURCE_DIR}/CMakeLists.txt")
endif (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")

LIST(APPEND CMAKE_MODULE_PATH ${CCPP_FRAMEWORK}/cmake)

######################################################################
# CCPP code generation

SET(SCHEME_METADATA ../../src/band_camp_scheme.meta)
SET(HOST_METADATA ../band_camp_host.meta,band_camp_mod.meta)
SET(SUITE_XML ../band_camp_suite.xml)

# Run ccpp_capgen
SET(CAPGEN_CMD "${CCPP_FRAMEWORK}/ccpp_capgen.py")
LIST(APPEND CAPGEN_CMD "--host-files")
LIST(APPEND CAPGEN_CMD "${HOST_METADATA}")
LIST(APPEND CAPGEN_CMD "--scheme-files")
LIST(APPEND CAPGEN_CMD "${SCHEME_METADATA}")
LIST(APPEND CAPGEN_CMD "--suites")
LIST(APPEND CAPGEN_CMD "${SUITE_XML}")
LIST(APPEND CAPGEN_CMD "--output-root")
LIST(APPEND CAPGEN_CMD "${CCPP_CAP_FILES}")
LIST(APPEND CAPGEN_CMD "--generate-host-cap")
LIST(APPEND CAPGEN_CMD "--host-name")
LIST(APPEND CAPGEN_CMD "${HOST}")
LIST(APPEND CAPGEN_CMD "--verbose")

STRING(REPLACE ";" " " CAPGEN_STRING "${CAPGEN_CMD}")
MESSAGE(STATUS "Running: ${CAPGEN_STRING}")
EXECUTE_PROCESS(COMMAND ${CAPGEN_CMD} WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE CAPGEN_OUT ERROR_VARIABLE CAPGEN_OUT RESULT_VARIABLE RES)
MESSAGE(STATUS "${CAPGEN_OUT}")
IF(RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap generation completed")
ELSE(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
ENDIF(RES EQUAL 0)

# Add the cap files to the list of target sources
FILE(STRINGS ${CCPP_CAP_FILES}/capfiles.txt CCPP_CAPS)
TARGET_SOURCES(${HOST} PUBLIC ${CCPP_CAPS})
INCLUDE_DIRECTORIES(${CCPP_CAP_FILES})

SET_TARGET_PROPERTIES(${HOST} PROPERTIES
                              COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                              LINK_FLAGS "${CMAKE_Fortran_FLAGS}")

