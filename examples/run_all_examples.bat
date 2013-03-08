@ echo off

   rem Relative from INSIDE a testcase:
set TCL_EXE=..\..\src\third_party_open\tcl\bin\win32\tclkit.exe


echo "Running testcase 01_standard ..."
cd 01_standard
call run_flow2d3d.bat >screen.log 2>&1


echo "Running testcase 01_standard parallel ..."
   rem first fix the number of partitions to 2 (it's a very small model)
%TCL_EXE% ../sed_in_file.tcl run_flow2d3d_parallel.bat "mpiexec -n %%NUMBER_OF_PROCESSORS%%" "mpiexec -n 2"
call run_flow2d3d_parallel.bat >screen_parallel.log 2>&1
%TCL_EXE% ../sed_in_file.tcl run_flow2d3d_parallel.bat "mpiexec -n 2" "mpiexec -n %%NUMBER_OF_PROCESSORS%%"
cd ..


echo "Running testcase 02_domaindecomposition ..."
cd 02_domaindecomposition
call run_flow2d3d.bat >screen.log 2>&1
cd ..


echo "Running testcase 03_flow-wave ..."
cd 03_flow-wave
call run_flow2d3d.bat >screen.log 2>&1
cd ..


echo "Running testcase 04_fluidmud ..."
cd 04_fluidmud
call run_flow2d3d_flm.bat >screen.log 2>&1
cd ..


echo "Running testcase 05_mormerge ..."
cd 05_mormerge\merge
call run_flow2d3d_wave_mormerge.bat >screen.log 2>&1
cd ..\..


echo ...finished
pause
