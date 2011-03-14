@ echo off
rem
rem the config file to be used:
rem
set argfile=config.ini
rem
rem
rem path to your Delft3D installation:
rem
set D3D_HOME=D:\src\ds_trunk_src
rem
rem
rem path to your Delft3D executables:
rem
rem set exedir=%D3D_HOME%\engines_gpl\deltares_hydro\bin\Debug
set exedir=%D3D_HOME%\engines_gpl\deltares_hydro\bin\Release
rem
rem
rem set mpiexec in your path:
rem
set PATH=%D3D_HOME%\third_party_open\mpich2\bin;%PATH%
rem
rem
rem delete old com-file and tri-diag-files:
rem
del /f com*.d??
del /f tri-diag.*
rem
rem
rem start computation on all your local cores (2 for dual core; 4 for quad core etc.):
rem
mpiexec -n %NUMBER_OF_PROCESSORS% %exedir%\deltares_hydro.exe %argfile%
rem
pause