@echo off
rem
rem Script to build HDF5 and NetCDf4 from scratch
rem

goto netcdf
echo Building HDF5 and NetCDF libraries
echo Note:
echo There may be build errors reported at various steps, but they seem not relevant
echo to getting the libraries

set ARCH=64
if /%TARGET_ARCH%==/ia32 set ARCH=32
if /%TARGET_ARCH%==/intel64 set ARCH=64
if /%TARGET_VS%==/vs2012 set VS=VS2012
if /%TARGET_VS%==/vs2013 set VS=VS2013
if /%VS%==/ goto error

rem
rem HDF5, Step 1
rem
echo Step 1: Building HDF5 - this will take a while ...
pause

cd CMake-hdf5-1.8.16
call build-%VS%-%ARCH%.bat

rem
rem NetCDF-C, step 2
rem
echo Step 2: Building NetCDF C libraries ...
pause

cd ..

:netcdf

copy netcdf-config.cmake.in.corrected netcdf-4.3.2\netcdf-config.cmake.in
md build-netcdf
cd build-netcdf

set HDF=%cd%\..\CMake-hdf5-1.8.16\build\_CPack_Packages\win%ARCH%\ZIP\HDF5-1.8.16-win%ARCH%
cmake ..\netcdf-4.3.2 -G "NMake Makefiles" -DHDF5_LIB=%HDF%\lib\libhdf5.lib -DHDF5_HL_LIB=%HDF%\lib\libhdf5_hl.lib -DHDF5_INCLUDE_DIR=%HDF%\include -DZLIB_LIBRARY=%HDF%\lib\libzlib.lib -DSZIP_LIBRARY=%HDF%\lib\libszip.lib -DZLIB_INCLUDE_DIR=%$HDF%\include -DSZIP_INCLUDE_DIR=%HDF%\include -DENABLE_DAP=OFF -DENABLE_NETCDF_4=ON -DENABLE_PARALLEL=ON -DBUILD_SHARED_LIBS=OFF -DCMAKE_BUILD_TYPE=Release
nmake

rem
rem NetCDF-Fortran, step 3
rem
echo Step 3: Building NetCDF Fortran libraries ...
pause

cd ..
md build-netcdf-fortran
cd build-netcdf-fortran

set NCSRC=%cd%\..\netcdf-4.3.2
set NC=%cd%\..\build-netcdf

md %NC%\CMakeFiles\Export\lib
copy %NC%\liblib\netcdf.lib %NC%\CMakeFiles\Export\lib

cmake ..\netcdf-fortran-4.4.1 -G "NMake Makefiles" -DnetCDF_INCLUDE_DIR=%NCSRC%\include -DnetCDF_DIR=%NC% -DNETCDF_C_LIBRARY=%NC%\liblib\netcdf.lib -DNETCDF_C_INCLUDE_DIR=%NCSRC%\include -DENABLE_NETCDF_4=ON -DENABLE_DAP=OFF -DENABLE_NETCDF_V2=OFF -DCMAKE_INSTALL_PREFIX=..\build-netcdf -DENABLE_NETCDF_4=ON -DBUILD_SHARED_LIBS=OFF -DCMAKE_BUILD_TYPE=Release
nmake

cd ..
echo Copying files to the subdirectory lib ...
md lib
echo on
copy %HDF%\lib\*.lib lib
copy %NC%\liblib\netcdf.lib lib
copy build-netcdf-fortran\fortran\netcdff.lib lib
copy build-netcdf-fortran\fortran\*.mod lib

echo Done
goto end

rem Error handling - no compiler environment
:error
echo Could not detect the Visual Studio version. Should be either VS2012 or VS2013
echo Please correct
pause

rem End of the batch file
:end
