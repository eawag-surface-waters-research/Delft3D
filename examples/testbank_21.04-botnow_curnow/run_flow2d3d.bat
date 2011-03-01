@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 01 Mar 2011
    rem 
    rem
    rem This script starts a single-domain Delft3D-FLOW computation online with Delft3D-WAVE on Windows
    rem


    rem
    rem Set the config file and mdw file
    rem 
set argfile=config_flow2d3d.ini
set mdwfile=r17.mdw




    rem
    rem Set the directory containing delftflow.exe
    rem
set exedir=..\..\src\engines_gpl\deltares_hydro\bin\release
set dlldir=..\..\src\engines_gpl\flow2d3d\bin\release
set wavedir=..\..\src\engines_gpl\wave\bin\release
set swandir=..\..\src\third_party_open\swan\bin\win32
set swanbatdir=..\..\src\third_party_open\swan\scripts

    rem
    rem No adaptions needed below
    rem

    rem Set some (environment) parameters
set D3D_HOME=%CD%\..\..\src
    rem Only needed for the debug version:
    rem set inteldir=c:\Program Files\Intel\Compiler\11.0\072\fortran\lib\ia32
set PATH=%swanbatdir%;%exedir%;%dlldir%;%swandir%;%inteldir%;%PATH%


    rem Run
start %exedir%\deltares_hydro.exe %argfile%

%wavedir%\wave.exe %mdwfile% 1

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
