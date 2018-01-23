@ echo off

    rem This example testcase is that small, that it can run with a maximum of 5 partitions
set NPROC=%NUMBER_OF_PROCESSORS%
if %NPROC% gtr 5 set NPROC=5
call ..\..\src\bin\x64\dflow2d3d\scripts\run_dflow2d3d_parallel.bat %NPROC%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
rem pause
