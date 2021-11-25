@echo off
set toolFound=false
set toolDebug=false
set cmakeConfiguration=build_all

if exist ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_import\Release\waqpb_import.exe     set toolFound=true
if exist ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_import\Debug\waqpb_import.exe (
    set toolFound=true
    set toolDebug=true
)

echo Searching in cmake build: %cmakeConfiguration%



if %toolFound%==true  (
    echo This will execute: waqpb_import.exe
    echo.
    echo This command will 'import' changes from the proces.asc file.
    echo Make sure it exists!
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
    if %toolDebug%==true ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_import\Debug\waqpb_import.exe -pdfprocesm.asc -newfrm
    if not %toolDebug%==true ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_import\Release\waqpb_import.exe -pdfprocesm.asc -newfrm
    copy proc_def.* ..\..\..\..\default
    if exist ..\..\..\..\..\..\..\%cmakeConfiguration%\x64\dwaq\default\ (
        copy proc_def.* ..\..\..\..\..\..\..\%cmakeConfiguration%\x64\dwaq\default\
    )
    echo.
    pause
) else (
    echo.
    echo waqpb_import.exe not found! 
    echo Please check cmake_configuration, or build the waq proces library tools.
    echo You can build it using build_all\all.sln
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
)