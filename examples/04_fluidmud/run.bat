@ echo off

rem At present, this runscript will only work with build_configuration = build_all
rem For this to work, the build.bat script in the top folder of the source tree needs to be executed for the "all (build full OSS tree)" configuration
rem with automatic build, i.e. with the following option switched OFF: "Prepare only, no automatic compilation"

set build_configuration=build_all
set script_path=..\..\%build_configuration%\x64\dflow2d3d\scripts
call %script_path%\run_dflow2d3d_fluidmud.bat


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
