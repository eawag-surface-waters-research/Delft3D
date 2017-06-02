@echo off

set swanexec=%D3D_HOME%\%ARCH%\swan\bin\swan_4072ABCDE_del_w64_i11_omp.exe

rem
set OMP_NUM_THREADS_BACKUP=%OMP_NUM_THREADS%
rem swan40.72AB and newer runs parallel, using the total number of cores on the machine by default
rem Two ways to force the number of parallel processes:
rem 1. Define environment parameter OMP_NUM_THREADS_SWAN with the correct number of processes
rem 2. Put a number behind the =-sign on the line "set OMP_NUM_THREADS=" below
if "%OMP_NUM_THREADS_SWAN%" == "" (
    set OMP_NUM_THREADS=
) else (
    set OMP_NUM_THREADS=%OMP_NUM_THREADS_SWAN%
)

@echo SWAN batchfile executed for Delft3D
@echo Using swan.bat in directory %~dp0
@echo Using %swanexec%
@echo Performing wave computation for: %1.swn

if exist PRINT del PRINT
if exist INPUT del INPUT
if exist swaninit del swaninit
if exist Errfile del Errfile
if exist errpts del errpts
if exist %1.erf del %1.erf
if exist %1.erp del %1.erp
if not exist %1.swn goto error1
if not exist "%swanexec%" goto error2
copy %1.swn INPUT

"%swanexec%"

copy PRINT %1.prt
if exist errfile copy errfile %1.erf
if exist errpts copy errpts %1.erp
if exist swaninit del swaninit
goto finish
:error1
@echo
@echo     **************************************************************
@echo                SWAN input file %1.swn does not exist
@echo     **************************************************************
rem No user interaction!
rem pause
goto finish
:error2
@echo
@echo     **************************************************************
@echo                SWAN executable does not exist
@echo                (%swanexec%)
@echo     **************************************************************
rem No user interaction!
rem pause
goto finish
:finish
set OMP_NUM_THREADS=%OMP_NUM_THREADS_BACKUP%

@echo on
rem exit
