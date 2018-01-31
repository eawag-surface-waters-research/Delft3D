@ echo off


    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install" as administrator:
    rem     "Start" -> "All programs" -> "Accessories", right-click "Command Prompt", "Run as Administrator"
    rem     In this command box:
    rem         cd ...\src\bin\x64\share\bin
    rem         (or when not compiled yet)
    rem         cd ...\src\third_party_open\mpich2\bin
    rem         smpd -install
    rem     When there is an smpd already running on the machine, it must be ended first, using the Microsoft Task Manager




    rem This example testcase is that small, that it can run with a maximum of 5 partitions
set NPROC=%NUMBER_OF_PROCESSORS%
if %NPROC% gtr 5 set NPROC=5
call ..\..\src\bin\x64\dflow2d3d\scripts\run_dflow2d3d_parallel.bat %NPROC%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
