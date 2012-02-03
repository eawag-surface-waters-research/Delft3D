@echo off

set globalErrorLevel=0

echo oss-install...

rem Example calls:
rem > install.cmd <dest directory>                # Install entire solution
rem > install.cmd flow2d3d <dest directory>       # Install only project flow2d3d (and its dependencies)

rem 0. defaults:
set project=
set dest_main=

rem  The next statement is needed in order for the set commands to work inside the if statement
setlocal enabledelayedexpansion

if [%2] EQU [] (
    rem Install all engines, assume the first argument is a target directory

    set dest_main=%1
    set project=install_all
    echo Target directory: !dest_main!
    echo Source          : all engines
) else (
    rem Install the package/engine specified by the first argument. The second argument is assumed to be the target directory.

    set dest_main=%2
    set project=%1
    echo Target directory: !dest_main!
    echo Source          : package/engine !project!
)

if [%dest_main%] EQU [] (
    echo "ERROR: No target directory specified as argument of oss-install.cmd"
    goto end
)

rem Change to directory tree where this batch file resides (necessary when oss-install.cmd is called from outside of oss/trunk/src)
cd %~dp0\..\..

if not exist !dest_main! mkdir !dest_main!
copy scripts_lgpl\win32\convert_win32_to_old_directory_structure_w32.bat !dest_main!

call :!project!

goto end

rem  Actual install "routines"


rem ============================================================
rem === if the command before a call to handle_error returns ===
rem === an error, the script will return with an error       ===
rem ============================================================
:handle_error
    if NOT %ErrorLevel% EQU 0 (
        set globalErrorLevel=%ErrorLevel%
    )
    rem go back to call site
goto :endproc

rem =============================================================
rem === makeDir accepts one argument: the name of the         ===
rem === directory it will create if it doesn't already exists ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === return with an error code after executing the rest of ===
rem === its statements                                        ===
rem =============================================================
:makeDir
    set dirName=%~1
    if not exist !dirName! mkdir !dirName!
    if not !ErrorLevel! EQU 0 (
        echo ERROR: while creating directory "!dirName!"
    )
    call :handle_error
goto :endproc

rem =============================================================
rem === copyFile takes two arguments: the name of the file to ===
rem === copy to the destiny directory                         ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === with an error code after executing the rest of its    ===
rem === statements                                            ===
rem =============================================================
:copyFile
    set fileName=%~1
    set dest=%~2
    copy %fileName% %dest%
    if NOT !ErrorLevel! EQU 0 (
        echo ERROR: while copying "!fileName!" to "!dest!"
    )
    call :handle_error
goto :endproc

rem ===============
rem === INSTALL_ALL
rem ===============
:install_all
    echo "    installing all open source projects . . ."

    call :d_hydro
    call :flow2d3d
    call :flow2d3d_openda
    call :wave
    call :plugin_culvert
    call :plugin_delftflow_traform
    call :datsel
    call :kubint
    call :lint
    call :mormerge
    call :vs

goto :endproc


rem ========================
rem === INSTALL_DELFT3D-FLOW
rem ========================
:delft3d-flow
    echo "    installing delft3d-flow . . ."

    call :d_hydro
    call :flow2d3d
    call :flow2d3d_openda
    call :plugin_culvert
    call :plugin_delftflow_traform
    call :mormerge

goto :endproc



rem ==========================
rem === INSTALL_D_HYDRO
rem ==========================
:d_hydro
    echo "installing d_hydro . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"

    call :makeDir !dest_bin!
rem    if not exist !dest_bin!     mkdir !dest_bin!
    
    call :copyFile engines_gpl\d_hydro\bin\Release\d_hydro.exe            !dest_bin!
rem    call :handle_error
    call :copyFile third_party_open\tclkit\bin\win32\deltares_hydro.exe   !dest_bin!
rem    call :handle_error
goto :endproc



rem ====================
rem === INSTALL_FLOW2D3D
rem ====================
:flow2d3d
    echo "installing flow2d3d . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"
    set dest_lib="!dest_main!\win32\flow2d3d\lib"
    set dest_default="!dest_main!\win32\flow2d3d\default"
    set dest_scripts="!dest_main!\win32\flow2d3d\scripts"
    
    call :makeDir !dest_bin!
    call :makeDir !dest_lib!
    call :makeDir !dest_default!
    call :makeDir !dest_scripts!

    set ErrorLevel_flowdll=0
    copy engines_gpl\flow2d3d\bin\Release\flow2d3d.dll                       !dest_bin!
    if NOT %ErrorLevel%==0 (
        set ErrorLevel_flowdll=1
    )
    copy engines_gpl\flow2d3d\bin\Release\flow2d3d_sp.dll                    !dest_bin!
    if NOT !ErrorLevel!==0 (
        if NOT !ErrorLevel_flowdll!==0 (
            set GlobalErrorLevel=1
        )
    )
    rem One of these two dlls will not exist and cause an ErrorLevel=1. Reset it.
    set ErrorLevel=0
    call :copyFile engines_gpl\flow2d3d\scripts\meteo_old2new.m              !dest_scripts!
    call :copyFile third_party_open\pthreads\bin\win32\*.dll                 !dest_lib!
    call :copyFile third_party_open\mpich2\bin\*.exe                         !dest_bin!
    call :copyFile third_party_open\mpich2\lib\*.dll                         !dest_lib!
    call :copyFile third_party_open\expat\win32\bin\Release\libexpat.dll     !dest_lib!
    call :copyFile third_party_open\intel_fortran\lib\win32\*.dll            !dest_lib!
    call :copyFile engines_gpl\flow2d3d\default\*                            !dest_default!
goto :endproc



rem ===========================
rem === INSTALL_FLOW2D3D_OPENDA
rem ===========================
:flow2d3d_openda
    echo "installing flow2d3d_openda . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"
    set dest_lib="!dest_main!\win32\flow2d3d\lib"
    set dest_default="!dest_main!\win32\flow2d3d\default"
    set dest_scripts="!dest_main!\win32\flow2d3d\scripts"

    call :makeDir !dest_bin!
    call :makeDir !dest_lib!
    call :makeDir !dest_default!
    call :makeDir !dest_scripts!

    set ErrorLevel_opendadll=0
    copy engines_gpl\flow2d3d\bin\Release\flow2d3d_openda.dll                !dest_bin!
    if NOT %ErrorLevel%==0 (
        set ErrorLevel_opendadll=1
    )
    copy engines_gpl\flow2d3d\bin\Release\flow2d3d_openda_sp.dll             !dest_bin!
    if NOT !ErrorLevel!==0 (
        if NOT !ErrorLevel_opendadll!==0 (
            set GlobalErrorLevel=1
        )
    )
    rem One of these two dlls will not exist and cause an ErrorLevel=1. Reset it.
    set ErrorLevel=0
    call :copyFile engines_gpl\flow2d3d\scripts\meteo_old2new.m              !dest_scripts!
    call :copyFile third_party_open\pthreads\bin\win32\*.dll                 !dest_lib!
    call :copyFile third_party_open\mpich2\bin\*.exe                         !dest_bin!
    call :copyFile third_party_open\mpich2\lib\*.dll                         !dest_lib!
    call :copyFile third_party_open\expat\win32\bin\Release\libexpat.dll     !dest_lib!
    call :copyFile third_party_open\netcdf\lib\win32\release\netcdf.dll      !dest_lib!
    call :copyFile third_party_open\openda\core\native\lib\win32\*.dll       !dest_lib!
    call :copyFile third_party_open\intel_fortran\lib\win32\*.dll            !dest_lib!
    call :copyFile engines_gpl\flow2d3d\default\*.*                          !dest_default!
goto :endproc



rem ================
rem === INSTALL_WAVE
rem ================
:wave
    echo "installing wave . . ."

    set dest_bin="!dest_main!\win32\wave\bin"
    set dest_lib="!dest_main!\win32\wave\lib"
    set dest_default="!dest_main!\win32\wave\default"
    set dest_swan_bin="!dest_main!\win32\swan\bin"
    set dest_swan_lib="!dest_main!\win32\swan\lib"
    set dest_swan_scripts="!dest_main!\win32\swan\scripts"

    call :makeDir !dest_bin!
    call :makeDir !dest_lib!
    call :makeDir !dest_default!
    call :makeDir !dest_swan_bin!
    call :makeDir !dest_swan_lib!
    call :makeDir !dest_swan_scripts!

    call :copyFile engines_gpl\wave\bin\release\wave.exe           !dest_bin!
    call :copyFile engines_gpl\flow2d3d\default\dioconfig.ini      !dest_default!
    call :copyFile third_party_open\swan\bin\win32\*.dll           !dest_swan_lib!
    call :copyFile third_party_open\swan\bin\win32\*.exe           !dest_swan_bin!
    call :copyFile third_party_open\swan\scripts\swan_install.bat  !dest_swan_scripts!\swan.bat
goto :endproc



rem ==========================
rem === INSTALL_PLUGIN_CULVERT
rem ==========================
:plugin_culvert
    echo "installing plugin_culvert . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile plugins_lgpl\plugin_culvert\bin\Release\plugin_culvert.dll     !dest_bin!
goto :endproc



rem ====================================
rem === INSTALL_PLUGIN_DELFTFLOW_TRAFORM
rem ====================================
:plugin_delftflow_traform
    echo "installing plugin_delftflow_traform . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile plugins_lgpl\plugin_delftflow_traform\bin\Release\plugin_delftflow_traform.dll    !dest_bin!
goto :endproc



rem ==================
rem === INSTALL_DATSEL
rem ==================
:datsel
    echo "installing datsel . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\datsel\bin\Release\datsel.exe     !dest_bin!
goto :endproc



rem ==================
rem === INSTALL_KUBINT
rem ==================
:kubint
    echo "installing kubint . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\kubint\bin\Release\kubint.exe     !dest_bin!
goto :endproc



rem ================
rem === INSTALL_LINT
rem ================
:lint
    echo "installing lint . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\lint\bin\Release\lint.exe     !dest_bin!
goto :endproc



rem ====================
rem === INSTALL_MORMERGE
rem ====================
:mormerge
    echo "installing mormerge . . ."

    set dest_bin="!dest_main!\win32\flow2d3d\bin"
    set dest_scripts="!dest_main!\win32\flow2d3d\scripts"

    call :makeDir !dest_bin!
    call :makeDir !dest_scripts!

    call :copyFile engines_gpl\flow2d3d\scripts\mormerge.tcl       !dest_scripts!
    call :copyFile tools_gpl\mormerge\bin\Release\mormerge.exe     !dest_bin!
goto :endproc



rem ==============
rem === INSTALL_VS
rem ==============
:vs
    echo "installing vs . . ."

    set dest="!dest_main!\win32\util\bin"

    call :makeDir !dest!

    call :copyFile tools_gpl\vs\bin\Release\vs.exe     !dest!
goto :endproc






:end
if NOT %globalErrorLevel% EQU 0 (
    rem
    rem Only jump to :end when the script is completely finished
    rem 
    echo An error occurred while executing this file
    echo Returning with error number %globalErrorLevel%
    exit %globalErrorLevel%
)

:endproc
   rem
   rem No exit here
   rem Otherwise the script exits directly at the first missing artefact
