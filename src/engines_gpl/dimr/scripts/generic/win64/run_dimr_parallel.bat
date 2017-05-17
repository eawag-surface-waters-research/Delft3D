@ echo off
title run_dimr_parallel
    rem
    rem This script runs dimr in parallel mode on Windows
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 03 Feb 2017
    rem 
    rem
setlocal enabledelayedexpansion


    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set numpar=%NUMBER_OF_PROCESSORS%
    set argfile=dimr_config.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    )
    set numpar=%1
    if [%2] EQU [] (
        set argfile=dimr_config.xml
    ) else (
        set argfile=%2
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)
echo number of partitions: %numpar%

set workdir=%CD%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set ARCH=win64
set D3D_HOME=%~dp0..\..



set delwaqexedir=%D3D_HOME%\%ARCH%\dwaq\bin
set dflowfmexedir=%D3D_HOME%\%ARCH%\dflowfm\bin
set dimrexedir=%D3D_HOME%\%ARCH%\dimr\bin
set esmfexedir=%D3D_HOME%\%ARCH%\esmf\bin
set esmfbatdir=%D3D_HOME%\%ARCH%\esmf\scripts
set flow1dexedir=%D3D_HOME%\%ARCH%\dflow1d\bin
set flow1d2dexedir=%D3D_HOME%\%ARCH%\dflow1d2d\bin
set rrexedir=%D3D_HOME%\%ARCH%\drr\bin
set rtctoolsexedir=%D3D_HOME%\%ARCH%\dfbc\bin
set swanexedir=%D3D_HOME%\%ARCH%\swan\bin
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set shareddir=%D3D_HOME%\%ARCH%\shared
set waveexedir=%D3D_HOME%\%ARCH%\dwaves\bin


    rem
    rem No adaptions needed below
    rem

    rem Run
set PATH=%dimrexedir%;%delwaqexedir%;%dflowfmexedir%;%flow1dexedir%;%flow1d2dexedir%;%rtctoolsexedir%;%rrexedir%;%waveexedir%;%swanbatdir%;%swanexedir%;%esmfbatdir%;%esmfexedir%;%shareddir%
    rem With debug info: "%dhydroexedir%\d_hydro.exe" -d 0xFFFFFFFF %argfile%
"%shareddir%\mpiexec.exe" -n %numpar% -localonly "%dimrexedir%\dimr.exe" -d 0xFFFFFFFF %argfile%

goto end

:usage
echo Usage:
echo run_dimr_parallel.bat [--help] [n] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     n              : (Optional) integer, number of partitions. Must match with the prepared D-Flow FM calculation.
echo                      Default value: %NUMBER_OF_PROCESSORS%
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
