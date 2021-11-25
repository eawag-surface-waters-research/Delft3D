@echo off
set version=5.10
set serial=2021101901
set toolFound=false
set toolDebug=false
set cmakeConfiguration=build_all

if exist ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_export\Release\waqpb_export.exe     set toolFound=true
if exist ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_export\Debug\waqpb_export.exe (
    set toolFound=true
    set toolDebug=true
)

echo Searching in cmake build: %cmakeConfiguration%

if %toolFound%==true (
    echo This will execute: waqpb_export.exe -version%version% -serial%serial%
    echo.
    echo This command will 'export' the data in the csv-tables to a proc_def,
    echo a procesm.asc and latex files for the manuals. The proc_def files
    echo will be copied to src\engines_gpl\waq\default\
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
    if %toolDebug%==true ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_export\Debug\waqpb_export.exe -version%version% -serial%serial% -newfrm
    if not %toolDebug%==true ..\..\..\..\..\..\..\%cmakeConfiguration%\waqpb_export\Release\waqpb_export.exe -version%version% -serial%serial% -newfrm
    copy proc_def.* ..\..\..\..\default
    if exist ..\..\..\..\..\..\..\%cmakeConfiguration%\x64\dwaq\default\ (
        copy proc_def.* ..\..\..\..\..\..\..\%cmakeConfiguration%\x64\dwaq\default\
    )
    echo.
    pause
) else (
    echo.
    echo waqpb_export.exe not found! 
    echo Please check cmake_configuration, or build the waq proces library tools.
    echo You can build it using build_all\all.sln
    echo.
    echo Run waqpb_help.bat for more information
    echo.
    pause
)
