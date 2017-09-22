set SAVEDIR=%CD%
cd ..\src
python ..\scripts\template\generate.py partition.F90 modules.f90 network_data.f90 d_flooding.f90 monitoring.f90 --template-dir=.\templates --verbose
cd %SAVEDIR%
