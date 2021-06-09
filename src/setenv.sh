#!/bin/bash
###############################################
### load your build environment 	    ###
###############################################
echo "Module Load"
module load    intel/18.0.3
module display intel/18.0.3

module load    gcc/7.3.0
module display gcc/7.3.0

module load    libtool/2.4.6_gcc7.3.0
module display libtool/2.4.6_gcc7.3.0

module load    mpich/3.3.2_intel18.0.3
module display mpich/3.3.2_intel18.0.3

module load    netcdf/v4.7.4_v4.5.3_intel18.0.3
module display netcdf/v4.7.4_v4.5.3_intel18.0.3

module load    petsc/3.13.3_intel18.0.3_mpich3.3.2
module display petsc/3.13.3_intel18.0.3_mpich3.3.2

module load    metis/5.1.0_intel18.0.3
module display metis/5.1.0_intel18.0.3

# Shapelib is intertangled with the code in third_party_open
# loading the module is useless
#module load    shapelib/1.5.0_intel18.0.3
#module display shapelib/1.5.0_intel18.0.3

module load    proj/7.1.0_gcc7.3.0
module display proj/7.1.0_gcc7.3.0

module load    cmake/3.18.0_intel18.0.3 
module display cmake/3.18.0_intel18.0.3 

module load    gdal/3.1.2_gcc7.3.0
module display gdal/3.1.2_gcc7.3.0

# Do not load svn/1.x: "svnversion" is executed during CMake and must match with the SVN version used to checkout the source code
# module load    svn/1.9.12serf_gcc7.3.0
# module display svn/1.9.12serf_gcc7.3.0

echo "Export environment variables"
export FC=mpif90
echo "FC=$FC"

export CXX=mpicxx
echo "CXX=$CXX"

export CC=mpicc
echo "CC=$CC"

