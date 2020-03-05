CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
PROJECT(simple)
ENABLE_LANGUAGE(Fortran)

SET(CMAKE_MACOSX_RPATH 1)

# CCPP directories
FIND_PATH(CCPP_ROOT scripts/ccpp_capgen.py
          DOC "CCPP-framework root directory"
          PATH $ENV{CCPP_ROOT})
SET(CCPP_CAP_FILES ${CMAKE_BINARY_DIR}/ccpp CACHE
    STRING "Location of CCPP-generated cap files")
SET(CCPP_FRAMEWORK ${CCPP_ROOT}/scripts)

SET(HOST "simple")
ADD_EXECUTABLE(${HOST} simple_host.F90)
TARGET_SOURCES(${HOST} PUBLIC simple_scheme.F90 simple_mod.F90)

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

SET(SCHEME_METADATA simple_scheme.meta)
SET(HOST_METADATA simple_host.meta,simple_mod.meta)
SET(SUITE_XML simple_suite.xml)

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

