@echo off
cmd.exe /K ""C:\Program Files (x86)\Intel\oneAPI\setvars.bat" && pwsh"


set configuration=dwaq
set generator="Ninja"
set build_dir=build_%configuration%
set build_type=Release
set install_dir=install_dwaq_ninja 

rmdir /s /q %build_dir%
mkdir %build_dir%
rem cmake .\src\cmake -G "Visual Studio 16 2019" -A x64 -B "build_all" -D CONFIGURATION_TYPE="all" -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icx -DCMAKE_Fortran_COMPILER=ifx
cmake .\src\cmake -G %generator% -B %build_dir% -D CONFIGURATION_TYPE=%configuration% -D CMAKE_BUILD_TYPE=%build_type% -DCMAKE_INSTALL_PREFIX=%install_dir% -DCMAKE_Fortran_COMPILER=ifx
