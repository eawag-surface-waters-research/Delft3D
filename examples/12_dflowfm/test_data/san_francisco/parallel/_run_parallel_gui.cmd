cd %cd% 

"%ProgramFiles(x86)%\MPICH2\bin\mpiexec.exe" -wdir %cd% -n 4 ..\..\..\bin\Win32\Release\unstruc.exe r07e_bay.mdu
rem ..\..\..\third_party_open\mpich2\bin\mpiexec.exe -n 4 ..\..\..\bin\x64\Release\unstruc.exe r07e_bay.mdu