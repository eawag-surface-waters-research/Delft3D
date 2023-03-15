@echo off
set configuration=dwaq
set generator="Ninja"
set build_dir=build_%configuration%
set build_type=Release
set install_dir=install_dwaq_ninja 

rmdir /s /q %build_dir%
mkdir %build_dir%
cmake .\src\cmake -G %generator% -B %build_dir% -D CONFIGURATION_TYPE=%configuration% -D CMAKE_BUILD_TYPE=%build_type% -DCMAKE_INSTALL_PREFIX=%install_dir% -DCMAKE_Fortran_COMPILER=ifx
