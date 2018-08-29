set SAVEDIR=%CD%
cd ..\src
python ..\scripts\template\generate.py partition.F90 modules.f90 ..\utils_lgpl\gridgeom\packages\gridgeom\src\network_data.f90 d_flooding.f90 monitoring.f90 ..\utils_lgpl\gridgeom\packages\gridgeom\src\generalmodules.f90 --template-dir=.\templates --verbose
cd %SAVEDIR%
