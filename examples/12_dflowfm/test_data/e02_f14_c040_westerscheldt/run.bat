@ echo off

    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install" as administrator:
    rem     Preparation: Check that your Delft3D installation contains "...\x64\share\bin\smpd.exe". Optionally copy it to a local directory (it will run as a service).
    rem     "Start" -> "All programs" -> "Accessories", right-click "Command Prompt", "Run as Administrator"
    rem     In this command box:
    rem         cd ...\x64\share\bin
    rem         smpd -install
    rem     When there is an smpd already running on the machine, it must be ended first, using the Microsoft Task Manager, 
    rem     or in the command  box: smpd -uninstall

rem call ..\..\..\..\src\bin\x64\dflowfm\scripts\run_dflowfm.bat "--partition:ndomains=3:icgsolver=6" westerscheldt.mdu


rem call ..\..\..\..\src\bin\x64\dimr\scripts\run_dimr_parallel.bat 3 dimr_config.xml

rem At present, this runscript will only work with build_configuration = build_all
rem For this to work, the build.bat script in the top folder of the source tree needs to be executed for the "all (build full OSS tree)" configuration
rem with automatic build, i.e. with the following option switched OFF: "Prepare only, no automatic compilation"

set build_configuration=build_all
set script_path=..\..\..\..\%build_configuration%\x64\dimr\scripts
call %script_path%\run_dimr_parallel.bat 3 dimr_config.xml


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
