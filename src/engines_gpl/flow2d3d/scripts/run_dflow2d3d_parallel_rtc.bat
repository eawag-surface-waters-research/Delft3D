@ echo off
title run_flow2d3d_parallel_rtc
    rem
    rem This script runs Delft3D-FLOW in parallel online with RTC on Windows
    rem Adapt and use it for your own purpose
    rem
setlocal enabledelayedexpansion

set numpar= 
set argfile= 
    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [--help] (
        goto usage
    ) else (
        set numpar=%1
        if [%2] EQU [] (
            set argfile=config_d_hydro.xml
        ) else (
            set argfile=%2
        )
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
set D3D_HOME=%~dp0..\..\..

rem Remove "\dflow2d3d\scripts\..\..\.." from D3D_HOME
set D3DT=%D3D_HOME:~0,-27%
rem last directory will be the architecture directory
for %%f in ("%D3DT%") do set ARCH=%%~nxf

set dflow2d3ddir=%D3D_HOME%\%ARCH%\dflow2d3d\bin
set sharedir=%D3D_HOME%\%ARCH%\share\bin
set rtcexedir=%D3D_HOME%\%ARCH%\rtc\bin
set rtcdefaultdir=%D3D_HOME%\%ARCH%\rtc\default


    rem
    rem No adaptions needed below
    rem

    rem Start FLOW
set PATH=%dflow2d3ddir%;%sharedir%
echo executing in separate window: "%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %argfile%
              start "Delft3D-FLOW" "%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %argfile%





    rem Start RTC
title RTC simulation
    echo "    FLOW is started in a separate second command box"
    echo "    Wait until the following lines appears in the FLOW command box:"
    echo "        Part IV   - Reading complete MD-file..."
    echo "        Part V    - Initialisation and checking input..."
    echo "        Part VI   - Initialisation and checking second part..."
    echo "        Part VII  - Initialisation output..."
    echo "    Then press a key in the first command box to start RTC"
pause

set PATH=%rtcexedir%;%sharedir%
echo executing: "%rtcexedir%\rtc.exe" "%rtcdefaultdir%\RTC.FNM" "%rtcdefaultdir%\RTC.RTN"
                "%rtcexedir%\rtc.exe" "%rtcdefaultdir%\RTC.FNM" "%rtcdefaultdir%\RTC.RTN"



goto end

:usage
echo Usage:
echo "run_dflow2d3d_parallel_rtc.bat [--help] n [config_d_hydro.xml]"
echo     --help            : (Optional) show this usage
echo     n                 : (Mandatory) integer, number of partitions.
echo     config_d_hydro.xml: (Optional) default: config_d_hydro.xml

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
