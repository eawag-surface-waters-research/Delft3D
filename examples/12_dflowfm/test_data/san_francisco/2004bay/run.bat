rem Call this script from within any working directory, and Unstruc will
rem be started with any required files first copied automatically.
rem PATH=D:\UNSTRUC\unstrucexec\;%PATH%
if not exist unstruc.ini copy D:\sourceDFM\bin\Release\unstruc.ini .
if not exist isocolour.hls copy D:\sourceDFM\bin\Release\isocolour.hls .
if not exist interact.ini  copy D:\sourceDFM\bin\Release\interact.ini .
rem if not exist dout.bat  copy D:\UNSTRUC\unstrucexec\dout.bat .
rem SET OMP_NUM_THREADS=8
"D:\sourceDFM\bin\Release\unstruc.exe" %1
