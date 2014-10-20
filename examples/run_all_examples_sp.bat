@ echo off

   rem Relative from INSIDE a testcase:
set TCL_EXE=..\..\src\third_party_open\tcl\bin\win32\tclkit.exe


echo "Running testcase 01_standard (sp) ..."
cd 01_standard
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run_flow2d3d.bat >screen.log 2>&1


echo "Running testcase 01_standard parallel (sp) ..."
   rem first fix the number of partitions to 2 (it's a very small model)
%TCL_EXE% ../sed_in_file.tcl run_flow2d3d_parallel.bat "-n %%NUMBER_OF_PROCESSORS%%" "-n 2"
call run_flow2d3d_parallel.bat >screen_parallel.log 2>&1
   rem Undo changes
%TCL_EXE% ../sed_in_file.tcl run_flow2d3d_parallel.bat "-n 2" "-n %%NUMBER_OF_PROCESSORS%%"
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 02_domaindecomposition (sp) ..."
cd 02_domaindecomposition
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run_flow2d3d.bat >screen.log 2>&1
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 03_flow-wave (sp) ..."
cd 03_flow-wave
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run_flow2d3d.bat >screen.log 2>&1


echo "Running testcase 03_flow-wave parallel (sp) ..."
call run_flow2d3d_parallel_wave.bat >screen_parallel.log 2>&1
%TCL_EXE% ../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 04_fluidmud (sp) ..."
cd 04_fluidmud
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_mud.xml "<library>flow2d3d" "<library>flow2d3d_sp"
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_sed.xml "<library>flow2d3d" "<library>flow2d3d_sp"
call run_flow2d3d_flm.bat >screen.log 2>&1
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_mud.xml "<library>flow2d3d_sp" "<library>flow2d3d"
%TCL_EXE% ../sed_in_file.tcl config_d_hydro_sed.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..


echo "Running testcase 05_mormerge (sp) ..."
cd 05_mormerge\input
..\%TCL_EXE% ../../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d" "<library>flow2d3d_sp"
cd ..\merge
call run_flow2d3d_wave_mormerge.bat >screen.log 2>&1
cd ..\input
..\%TCL_EXE% ../../sed_in_file.tcl config_d_hydro.xml "<library>flow2d3d_sp" "<library>flow2d3d"
cd ..\..


echo "Skipping testcases 06_delwaq, 07_wave, 08_part-tracer and 09_part-oil (no sp variant)"

echo ...finished
pause
