@ echo off
title run_flow2d3d__parallel_dwaves
    rem When using intelMPI for the first time on a machine:
    rem Execute "hydra_service.exe -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\share\bin\hydra_service.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Windows Start button" -> type "cmd", right-click "Command Prompt" App, "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\share\bin (or your local copy)
    rem         hydra_service.exe -install
    rem         mpiexec.exe -register -username <user> -password <password> -noprompt
    rem     When there is an hydra_service/smpd already running on the machine, it must be ended first, using the Microsoft Task Manager, 
    rem     or in the command  box: hydra_service.exe -uninstall (smpd -uninstall)
    rem
    rem This script runs Delft3D-FLOW in parallel online with Delft3D-WAVE on Windows
    rem Adapt and use it for your own purpose
    rem
    rem Usage example:
    rem Leave this script where it is.
    rem Call this script from within the working directory:
    rem path\to\delft3d\installation\x64\dflow2d3d\scripts\run_dflow2d3d_parallel_dwaves.bat
    rem More examples: check run scripts in https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/*

setlocal enabledelayedexpansion

set numpar= 
set argfile= 
set mdwfile= 
    rem
    rem Set the config file
    rem
if [%1] EQU [] (
    goto usage
) else (
    if [%1] EQU [-w] (
        set numpar=%NUMBER_OF_PROCESSORS%
        set argfile=config_d_hydro.xml
        if [%2] EQU [] (
            goto usage
        )
        set mdwfile=%2
    ) else (
        if [%1] EQU [--help] (
            goto usage
        ) else (
            set numpar=%1
            set argfile=%2
            if [%3] EQU [-w] (
                if [%4] EQU [] (
                    goto usage
                ) else (
                    set mdwfile=%4
                )
            ) else (
                goto usage
            )
        )
    )
)
echo Configfile:%argfile%
if not exist %argfile% (
    echo ERROR: configfile "%argfile%" does not exist
    goto usage
)
echo mdw-file:%mdwfile%
if not exist %mdwfile% (
    echo ERROR: mdw-file "%mdwfile%" does not exist
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
set swanbatdir=%D3D_HOME%\%ARCH%\swan\scripts
set waveexedir=%D3D_HOME%\%ARCH%\dwaves\bin


    rem
    rem No adaptions needed below
    rem

    rem Start FLOW
set PATH=%dflow2d3ddir%;%sharedir%
if exist %sharedir%\vars.bat (
    echo executing: "%sharedir%\vars.bat"
        call "%sharedir%\vars.bat"
) else (
    echo "WARNING: File not found: %sharedir%\vars.bat"
    echo "         Problems may occur when using IntelMPI"
)

echo executing in separate window: "%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %argfile%
              start "Delft3D-FLOW" "%sharedir%\mpiexec.exe" -n %numpar% -localonly "%dflow2d3ddir%\d_hydro.exe" %argfile%

    rem Start WAVE
title Delft3D-WAVE simulation
set PATH=%waveexedir%;%swanbatdir%;%sharedir%
echo executing in this window: "%waveexedir%\wave.exe" %mdwfile% 1
"%waveexedir%\wave.exe" %mdwfile% 1
title %CD%

goto end

:usage
echo Usage:
echo "run_dflow2d3d_parallel_dwaves.bat [--help] n <config_d_hydro.xml> -w <mdw-file>"
echo     --help            : (Optional) show this usage
echo     n                 : (Mandatory) integer, number of partitions.
echo                         Number of processors on this machine: %NUMBER_OF_PROCESSORS%
echo     config_d_hydro.xml: (Mandatory) Delft3D-FLOW input file
echo     -w <mdw-file>     : (Mandatory) Delft3D-WAVE input file

:end
    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
