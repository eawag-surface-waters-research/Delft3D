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
    rem debuglevel=0:silent 8:major 32:detail
set debuglevel=8

    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    set argfile=dimr_config.xml
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        if [%1] EQU [-d] (
            set debuglevel=%2
            set argfile=%3
        ) else (
            set argfile=%1
            if [%2] EQU [-d] (
                set debuglevel=%3
            )
        )
    )
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
set D3D_HOME=%~dp0..\..

rem Two possibilities for %ARCH% are suitable (win64 and x64)
if exist %D3D_HOME%\win64 (
    set ARCH=win64
) else (
    set ARCH=x64
)


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
echo executing: "%dimrexedir%\dimr.exe" -d %debuglevel% %argfile%
"%dimrexedir%\dimr.exe" -d %debuglevel% %argfile%

goto end

:usage
echo Usage:
echo run_dimr.bat [--help] [-d debuglevel] [dimr_config.xml]
echo     --help         : (Optional) show this usage
echo     -d debuglevel  : (optional) debuglevel=0:silent, 8:major(default), 32:detail
echo     dimr_config.xml: (Optional) default: dimr_config.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
