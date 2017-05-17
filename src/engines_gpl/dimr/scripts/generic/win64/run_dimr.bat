@ echo off
title run_dimr
    rem
    rem This script runs dimr on Windows
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
    set argfile=dimr_config.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    )
    set argfile=%1
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)

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
"%dimrexedir%\dimr.exe" %argfile%

goto end

:usage
echo Usage:
echo run_dimr.bat [--help] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
