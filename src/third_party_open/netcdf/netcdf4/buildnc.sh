#!/bin/sh
#
# Script to build HDF5 and NetCDf4 from scratch
#

echo Building HDF5 and NetCDF libraries
echo Note:
echo There may be build errors reported at various steps, but they seem not relevant
echo to getting the libraries
echo

which cmake 1>/dev/null 2>&1
if [ $? -ne 0 ]; then
   echo CMake not found - check the path!
   exit
fi

which icc 1>/dev/null 2>&1
if [ $? -eq 0 ]; then
    which ifort 1>/dev/null 2>&1
    if [ $? -eq 0 ]; then
        export CC=icc
        export FC=ifort
        export CFLAGS=-fPIC
        export FFLAGS=-fPIC
        echo Note: using Intel\'s C and Fortran compilers
    else
        echo Note: found Intel\'s C compiler, icc, but not ifort - please check
        exit
    fi
else
    echo Note: using GNU C and Fortran compilers
fi
echo

#
# Unpack the tar files
#
echo Unpacking ...
echo Pause - press ENTER
read dummy

gunzip CMake-hdf5-1.8.16.tar.gz
tar xvf CMake-hdf5-1.8.16.tar
gzip CMake-hdf5-1.8.16.tar

gunzip netcdf-4.3.2.tar.gz
tar xvf netcdf-4.3.2.tar
gzip netcdf-4.3.2.tar

gunzip netcdf-fortran-4.4.1.tar.gz
tar xvf netcdf-fortran-4.4.1.tar
gzip netcdf-fortran-4.4.1.tar

#
# HDF5, Step 1
#
echo Step 1: Building HDF5 - this will take a while ...
echo Pause - press ENTER
read dummy

cd CMake-hdf5-1.8.16
./build-unix.sh

#
# NetCDF-C, step 2
#
echo Step 2: Building NetCDF C libraries ...
echo Pause - press ENTER
read dummy

cd ..
cp netcdf-config.cmake.in.corrected netcdf-4.3.2/netcdf-config.cmake.in
mkdir build-netcdf
cd build-netcdf

HDF=`pwd`/../CMake-hdf5-1.8.16/build/_CPack_Packages/Linux/TGZ/HDF5-1.8.16-Linux/HDF_Group/HDF5/1.8.16
cmake ../netcdf-4.3.2 -G "Unix Makefiles" -DHDF5_LIB=$HDF/lib/libhdf5.a -DHDF5_HL_LIB=$HDF/lib/libhdf5_hl.a -DHDF5_INCLUDE_DIR=$HDF/include -DZLIB_LIBRARY=$HDF/lib/libz.a -DSZIP_LIBRARY=$HDF/lib/libszip.a -DZLIB_INCLUDE_DIR=$HDF/include -DSZIP_INCLUDE_DIR=$HDF/include -DENABLE_DAP=OFF -DENABLE_NETCDF_4=ON -DENABLE_PARALLEL=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_SHARED_LINKER_FLAGS=-ldl
make

#
# NetCDF-Fortran, step 3
#

echo Step 3: Building NetCDF Fortran libraries ...
echo Pause - press ENTER
read dummy

cd ..
mkdir build-netcdf-fortran
cd build-netcdf-fortran

cdir=../build-netcdf
pwd
ls $cdir/*.pc
libtype=`grep -i lib64 $cdir/netcdf.pc`
if [ -z $libtype ]
then
   mkdir $cdir/CMakeFiles/Export/lib
   cp $cdir/liblib/libnetcdf.so.4.3.2 $cdir/CMakeFiles/Export/lib
else
   mkdir $cdir/CMakeFiles/Export/lib64
   cp $cdir/liblib/libnetcdf.so.4.3.2 $cdir/CMakeFiles/Export/lib64
fi
NCSRC=`pwd`/../netcdf-4.3.2
NC=`pwd`/../build-netcdf
cmake ../netcdf-fortran-4.4.1 -G "Unix Makefiles" -DnetCDF_INCLUDE_DIR=$NCSRC/include -DnetCDF_DIR=$NC -DNETCDF_C_LIBRARY=$NC/liblib/libnetcdf.so.4.3.2 -DNETCDF_C_INCLUDE_DIR=$NCSRC/include -DENABLE_NETCDF_4=ON -DENABLE_DAP=OFF -DENABLE_NETCDF_V2=OFF -DCMAKE_INSTALL_PREFIX=$NC -DENABLE_NETCDF_4=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_STATIC_LINKER_FLAGS=-ldl
make

cd ..
echo Copying files to the subdirectory lib ...
if [ ! -d lib ]; then
   mkdir lib
   cd lib
else
   cd lib
   rm *
fi
cp $HDF/lib/*.a .
cp $NC/liblib/libnetcdf.so.4.3.2 .
cp ../build-netcdf-fortran/fortran/libnetcdff.so.6.0.1 .
cp ../build-netcdf-fortran/fortran/*.mod .
ln -s libnetcdff.so.6.0.1 libnetcdff.so.6
ln -s libnetcdff.so.6     libnetcdff.so
ln -s libnetcdf.so.4.3.2  libnetcdf.so.7.2.0
ln -s libnetcdf.so.7.2.0  libnetcdf.so
cd ..

echo Done
echo Contents of directory lib:
ls lib
