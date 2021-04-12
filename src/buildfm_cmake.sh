#!/bin/bash

. ../src/setenv.sh

# CMake
#echo "cmake ../src/cmake -G"Unix Makefiles" -DCONFIGURATION_TYPE=DFLOWFM_OPEN -DCOMMIT_VERSION=%build.vcs.number% -DCMAKE_FORTRAN_COMPILER=mpif90 -DCMAKE_CXX_COMPILER=mpicxx -DCMAKE_CC_COMPILER=mpicc -DCMAKE_BUILD_TYPE=Release"

echo "cmake ../src/cmake -G "Unix Makefiles" -B "." -D CONFIGURATION_TYPE="dflowfm_open" -D CMAKE_BUILD_TYPE=Release"
      cmake ../src/cmake -G "Unix Makefiles" -B "." -D CONFIGURATION_TYPE="dflowfm_open" -D CMAKE_BUILD_TYPE=Release

# Make
echo "make VERBOSE=1 install &>build_fm/make.log"
      make VERBOSE=1 install &>make.log

