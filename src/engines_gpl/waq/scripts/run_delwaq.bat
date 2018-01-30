@ echo off
title run_delwaq
    rem
    rem This script runs Delwaq on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

    rem
    rem Set the config file
    rem
set argfile= 
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set argfile=%1
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)


set workdir=%CD%
set argfile=%workdir%\%argfile%
echo Working directory: %workdir%
    rem
    rem Set the directories containing the binaries
    rem
set D3D_HOME=%~dp0..\..\..

rem Remove "\dwaq\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-22%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set waqdir=%D3D_HOME%\%ARCH%\dwaq\bin
set procfile=%D3D_HOME%\%ARCH%\dwaq\default\proc_def
set sharedir=%D3D_HOME%\%ARCH%\share\bin


    rem
    rem No adaptions needed below
    rem

    rem
    rem Run delwaq 1
    rem
set PATH=%waqdir%;%sharedir%
echo executing: "%waqdir%\delwaq1.exe" "%argfile%" -p "%procfile%"
"%waqdir%\delwaq1.exe" "%argfile%" -p "%procfile%"

if %ERRORLEVEL% neq 0 (
    echo.
    echo Delwaq1 did not run correctly, ending calculation
    goto end
)
echo. 
echo Delwaq1 did run without errors.
echo.

    rem Run delwaq 2
    rem
echo executing: "%waqdir%\delwaq2.exe" "%argfile%"
"%waqdir%\delwaq2.exe" "%argfile%"

if %ERRORLEVEL% neq 0 (
    echo.
    echo Delwaq2 did not run correctly
    goto end
)
echo. 
echo Delwaq2 did run without errors.


goto end

:usage
echo Usage:
echo run_delwaq.bat [--help] delwaq.inp
echo     --help     : (Optional) show this usage
echo     delwaq.inp : (Mandatory) Delwaq input file

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
