@ echo off
title %CD%
    rem
    rem This script is an example for running DFlowFM
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 19 Dec 2013
    rem
    rem
    rem This script starts a DFlowFM calculation on Windows
    rem


    rem
    rem Set the home directory
    rem
set ARCH=win64
set SCRIPT_DIR=%~dp0
set D3D_HOME=%SCRIPT_DIR%..\..\..

    rem release:
    rem set dflowfmexedir=%SCRIPT_DIR%..\bin
    rem set mpidir=%D3D_HOME%\%ARCH%\mpiexec

set dflowfmexedir=%SCRIPT_DIR%..\bin
set mpidir=%D3D_HOME%\%ARCH%\mpiexec

    rem
    rem No adaptions needed below
    rem

    rem Run
    rem Setting up the amount of the OMP threads to 1
set OMP_NUM_THREADS=1
set PATH=%dflowfmexedir%;%PATH%

"%mpidir%\mpiexec.exe" -n 3 -localonly "%dflowfmexedir%\dflowfm-cli.exe" --nodisplay --autostartstop %1%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
    rem pause
