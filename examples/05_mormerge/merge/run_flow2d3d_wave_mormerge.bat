@ echo off

set TCL_EXE=..\..\..\src\third_party_open\tcl\bin\win32\tclkit.exe

set scriptname=../../../bin/win32/flow2d3d/scripts/mormerge.tcl

%TCL_EXE% %scriptname% -i basin_windows.mm -s %scriptname%

pause
