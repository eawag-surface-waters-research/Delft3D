@echo off

set globalErrorLevel=0

echo oss-install...

rem Usage:
rem > oss-install.cmd <destiny>
rem > oss-install.cmd [project] <destiny>
rem > oss-install.cmd [project] <destiny> ["compiler_redist_dir"]
rem > oss-install.cmd [project] <destiny> ["compiler_redist_dir"] ["mkl_redist_dir"]

rem with:
rem   <destiny>               : Target directory where all binaries etc. are going to be installed by this script
rem   [project]               : (optional) project to install. If missing, "everything" is installed
rem   ["compiler_redist_dir"] : (optional) Directory containing compiler specific dll's to be installed
rem   ["mkl_redist_dir"]      : (optional) Directory containing Intel math kernel library specific dll's to be installed
rem                      surrounded by quotes to be able to handle white spaces in the path

rem
rem Example calls:
rem > install.cmd <dest directory>                # Install entire solution
rem > install.cmd dflowfm <dest directory>        # Install only project dflowfm (and its dependencies)
rem > install.cmd dflowfm <dest directory> "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\redist\ia32\compiler\"      																				  # Install only project dflowfm (and its dependencies)
rem > install.cmd dflowfm <dest directory> "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\redist\ia32\compiler\"  "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\redist\ia32\mkl\"   # Install only project dflowfm (and its dependencies including mkl required dlls)
rem                                                                                                                          including compiler specific dll's

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

if [%3] EQU [] (
    set compiler_redist_dir=""
) else (
    set compiler_redist_dir_read=%3
    rem Remove leading and trailing quote (")
    rem These quotes MUST be present in argument number 3, because "compiler_redist_dir" may contain white spaces
    set compiler_redist_dir=!compiler_redist_dir_read:~1,-1!
)

if [%4] EQU [] (
    set mkl_redist_dir=""
) else (
    set mkl_redist_dir_read=%4
    rem Remove leading and trailing quote (")
    rem These quotes MUST be present in argument number 4, because "mkl_redist_dir_read" may contain white spaces
    set mkl_redist_dir=!mkl_redist_dir_read:~1,-1!
)

rem Change to directory tree where this batch file resides (necessary when oss-install.cmd is called from outside of oss/trunk/src)
cd %~dp0\..\..

    rem =================
    rem === LOCKFILE
    rem === Problems may occur when this install script is being executed more than once at the same time.
    rem === Workaround:
    rem === 1. Unique id for each instance
    rem ===    %RANDOM% is not good enough, so the seconds and milliseconds are summed to %RANDOM% to get a (more) unique id
    rem === 2. Unique name for a lockfile
    rem ===    using the unique id
    rem === 3. getlock
    rem ===    Count the number of lockfiles: 0: create my unique lockfile
    rem ===                                  >1: wait 3 seconds and try again
    rem ===    Wait a second
    rem ===    Count the number of lockfiles: 1: yes, we have locked it, continue
    rem ===                                  >1: multiple instances tried the same, remove my lockfile and try again
    rem ===    Continue without lock after 10 trials
    rem === 4. Do install actions
    rem === 5. Remove my lockfile
    rem ====================

set myid=%TIME%
set /A myid=(1%myid:~6,2%-100)*100 + (1%myid:~9,2%-100)
set /A myid=%myid%+%RANDOM%

    rem This echo is necessary, otherwise different instances started at the same time may have the same id
echo oss-install id:"%myid%"
    rem The directory containing the lockfiles must be present, also the 1 second wait is necessary
call :makeDir !dest_main!
call :waitfunction 2
set lockfile=!dest_main!\oss-install_lockfile_!myid!.txt
    rem echo lockfile:!lockfile!
call :getlock

call :generic
call :!project!

call :releaselock

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
    rem
    rem "echo f |" is (only) needed when dest does not exist
    rem and does not harm in other cases
    rem
    echo f | xcopy "%fileName%" %dest% /F /Y
    if NOT !ErrorLevel! EQU 0 (
        echo ERROR: while copying "!fileName!" to "!dest!"
    )
    call :handle_error
goto :endproc

rem =============================================================
rem === copyFolderContent takes two arguments: the name of    ===
rem === the folder to copy the content to the destiny         ===
rem === directory                                             ===
rem ===                                                       ===
rem === NOTE: errors will be reported and the script will     ===
rem === with an error code after executing the rest of its    ===
rem === statements                                            ===
rem =============================================================
:copyFolderContent
    set folderName=%~1
    set dest=%~2
    rem
    rem "echo f |" is (only) needed when dest does not exist
    rem and does not harm in other cases
    rem
    echo f | xcopy "%folderName%" "%dest%" /s /e /h /f /y /i
    if NOT !ErrorLevel! EQU 0 (
        echo ERROR: while copying "!folderName!" to "!dest!"
    )
    call :handle_error
goto :endproc

rem =============================================================
rem === copyNetcdf copy the appropriate netcdf.dll            ===
rem =============================================================
:copyNetcdf
    set dest=%~1
    if not exist !dest! mkdir !dest!
    if not !ErrorLevel! EQU 0 (
        echo ERROR: while creating directory "!dest!"
    )
    call :copyFile "third_party_open\netcdf\netCDF 4.6.1\bin\*" !dest!
goto :endproc



rem ===============
rem === INSTALL_ALL
rem ===============
:install_all
    echo " WARNING: DISABLED: oss-install_x64::install_all . . ."
goto :endproc



rem ===================
rem === INSTALL_DELFT3D
rem ===================
:delft3d
    call delft3d-flow



rem ========================
rem === INSTALL_DELFT3D-FLOW
rem ========================
:delft3d-flow
    echo "installing delft3d-flow . . ."

    call :d_hydro
    call :flow2d3d
    call :plugin_culvert
    call :plugin_delftflow_traform
goto :endproc



rem ====================
rem === INSTALL_GENERIC
rem ====================
:generic
    rem
    rem Put the newest version of generic dlls in dest_share.
    rem When compiling a kernel, the actual generic dlls are place in the kernels bin folder.
    rem The DIMRset collector runs script "...\src\engines_gpl\dimr\scripts\dimr_artifacts.py",
    rem which removes duplicate dlls, assuming the version in dest_share must be kept,
    rem assuming the newest version of the dll can be used in combination with kernels build with older versions.
    rem
    echo "installing generic . . ."

    set dest_share="!dest_main!\x64\share\bin"

    call :makeDir !dest_share!

    call :copyFile "third_party_open\expat\x64\x64\Release\libexpat.dll"        !dest_share!
    call :copyFile "third_party_open\pthreads\bin\x64\*.dll"                    !dest_share!
    call :copyNetcdf                                                            !dest_share!
    echo This directory is automatically created by script https://git.deltares.nl/oss/delft3d/-/tree/main/src/scripts_lgpl/win64/oss-install_x64.cmd >!dest_share!\readme.txt
    echo This script is executed via a post-build event >>!dest_share!\readme.txt
    echo Further modifications can be done via a Python script executed via "DIMR_collector" projects in TeamCity >>!dest_share!\readme.txt
goto :endproc



rem ===================
rem === INSTALL_D_HYDRO
rem ===================
:d_hydro
    echo "installing d_hydro . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile engines_gpl\d_hydro\bin\x64\Release\d_hydro.exe      !dest_bin!
goto :endproc



rem ====================
rem === INSTALL_DFLOWFM
rem ====================
:dflowfm
    echo " WARNING: DISABLED: oss-install_x64::dflowfm . . ."
goto :endproc



rem ================
rem === INSTALL_DIMR
rem ================
:dimr
    echo " WARNING: DISABLED: oss-install_x64 . . ."
goto :endproc



rem ====================
rem === INSTALL_FLOW2D3D
rem ====================
:flow2d3d
    echo "installing flow2d3d . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"
    set dest_default="!dest_main!\x64\dflow2d3d\default"
    set dest_scripts="!dest_main!\x64\dflow2d3d\scripts"
    set dest_plugins="!dest_main!\x64\plugins\bin"
    set dest_share="!dest_main!\x64\share\bin"

    call :makeDir !dest_bin!
    call :makeDir !dest_default!
    call :makeDir !dest_scripts!
    call :makeDir !dest_plugins!
    call :makeDir !dest_share!

    set ErrorLevel_flowdll=0
    copy engines_gpl\flow2d3d\bin\x64\Release\flow2d3d.dll !dest_bin!
    if NOT %ErrorLevel%==0 (
        set ErrorLevel_flowdll=1
    )
    copy engines_gpl\flow2d3d\bin\x64\Release\flow2d3d_sp.dll !dest_bin!
    if NOT !ErrorLevel!==0 (
        if NOT !ErrorLevel_flowdll!==0 (
            set GlobalErrorLevel=1
        )
    )
    rem One of these two dlls will not exist and cause an ErrorLevel=1. Reset it.
    set ErrorLevel=0
    call :copyFile "engines_gpl\flow2d3d\scripts\meteo_old2new.m"                   !dest_scripts!
    call :copyFile "engines_gpl\flow2d3d\default\*"                                 !dest_default!
    call :copyFile "utils_lgpl\delftonline\lib\x64\Release\dynamic\delftonline.dll" !dest_bin!
    call :copyFile "utils_lgpl\delftonline\lib\x64\Release\dynamic\delftonline.dll" !dest_plugins!
    call :copyFile "engines_gpl\flow2d3d\scripts\run_*.bat"                         !dest_scripts!
    call :copyFile "third_party_open\tcl\bin\win64\tclkitsh852.exe"                 !dest_share!

    rem IntelMPI, copied from "oss-post_build.cmd"
    call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\*.dll"                !dest_share!
    call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\*.exe"                !dest_share!
    call :copyFile "%I_MPI_ONEAPI_ROOT%\bin\release\*.dll"        !dest_share!
    call :copyFile "%I_MPI_ONEAPI_ROOT%\libfabric\bin\*.dll"      !dest_share!
    call :copyFile "%I_MPI_ONEAPI_ROOT%\libfabric\bin\*.dll"      !dest_share!


    if !compiler_redist_dir!=="" (
        rem Compiler_dir not set
    ) else (
        rem "Compiler_dir:!compiler_redist_dir!"
        set localstring="!compiler_redist_dir!*.dll"
        rem Note the awkward usage of !-characters
        call :copyFile !!localstring! !dest_bin!!
    )
goto :endproc



rem ===========================
rem === INSTALL_FLOW2D3D_OPENDA
rem ===========================
:flow2d3d_openda
    echo " WARNING: DISABLED: oss-install_x64::flow2d3d_openda . . ."
goto :endproc



rem ===================
rem === INSTALL_DELWAQ1
rem ===================
:delwaq1
    echo " WARNING: DISABLED: oss-install_x64::delwaq1 . . ."
ase\delwaq1.exe                     !dest_bin!
goto :endproc



rem ===================
rem === INSTALL_DELWAQ2
rem ===================
:delwaq2
    echo " WARNING: DISABLED: oss-install_x64::delwaq2 . . ."
goto :endproc



rem ============================
rem === INSTALL_DELWAQ_DIMR_TEST
rem ============================
:delwaq_dimr_test
    echo " WARNING: DISABLED: oss-install_x64::delwaq_dimr_test . . ."
goto :endproc



rem ======================
rem === INSTALL_DELWAQ_DLL
rem ======================
:delwaq_dll
    echo " WARNING: DISABLED: oss-install_x64::delwaq dll . . ."
goto :endproc



rem ==============================
rem === INSTALL_DELWAQ2_OPENDA_LIB
rem ==============================
:delwaq2_openda_lib
    echo " WARNING: DISABLED: oss-install_x64::delwaq2_openda_lib . . ."
goto :endproc



rem ================================
rem === INSTALL_WAQ_PLUGIN_WASTELOAD
rem ================================
:waq_plugin_wasteload
    echo " WARNING: DISABLED: oss-install_x64::waq_plugin_wasteload . . ."
goto :endproc





rem ================
rem === INSTALL PART
rem ================
:part
    echo " WARNING: DISABLED: oss-install_x64::part . . ."
goto :endproc


rem ===================
rem === INSTALL_AGRHYD
rem ===================
:agrhyd
    echo " WARNING: DISABLED: oss-install_x64::agrhyd . . ."
goto :endproc

rem ===================
rem === INSTALL_MAPTONETCDF
rem ===================
:maptonetcdf
    echo " WARNING: DISABLED: oss-install_x64::maptonetcdf . . ."
goto :endproc


rem ===================
rem === INSTALL_DDCOUPLE
rem ===================
:ddcouple
    echo " WARNING: DISABLED: oss-install_x64::ddcouple . . ."
goto :endproc


rem ===================
rem === INSTALL_WAQMERGE
rem ===================
:waqmerge
    echo " WARNING: DISABLED: oss-install_x64::waqmerge . . ."
goto :endproc



rem ================
rem === INSTALL_WAVE
rem ================
:wave
    echo " WARNING: DISABLED: oss-install_x64::installing wave . . .%1"
goto :endproc



rem ===================
rem === INSTALL_WAVEEXE
rem ===================
:waveexe
    echo " WARNING: DISABLED: oss-install_x64::waveexe . . ."
goto :endproc



rem ==========================
rem === INSTALL_PLUGIN_CULVERT
rem ==========================
:plugin_culvert
    echo "installing plugin_culvert . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile plugins_lgpl\plugin_culvert\bin\x64\Release\plugin_culvert.dll !dest_bin!
goto :endproc



rem ====================================
rem === INSTALL_PLUGIN_DELFTFLOW_TRAFORM
rem ====================================
:plugin_delftflow_traform
    echo "installing plugin_delftflow_traform . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile plugins_lgpl\plugin_delftflow_traform\bin\x64\Release\plugin_delftflow_traform.dll !dest_bin!
goto :endproc



rem ==================
rem === INSTALL_DATSEL
rem ==================
:datsel
    echo "installing datsel . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\datsel\bin\x64\Release\datsel.exe !dest_bin!
goto :endproc



rem ==================
rem === INSTALL_KUBINT
rem ==================
:kubint
    echo "installing kubint . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\kubint\bin\x64\Release\kubint.exe !dest_bin!
goto :endproc



rem ================
rem === INSTALL_LINT
rem ================
:lint
    echo "installing lint . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\lint\bin\x64\Release\lint.exe !dest_bin!
goto :endproc



rem ====================
rem === INSTALL_MORMERGE
rem ====================
:mormerge
    echo "installing mormerge . . ."

    set dest_bin="!dest_main!\x64\dmor\bin"
    set dest_scripts="!dest_main!\x64\dmor\scripts"
    set dest_share="!dest_main!\x64\share\bin"

    call :makeDir !dest_bin!
    call :makeDir !dest_scripts!

    call :copyFile tools_gpl\mormerge\scripts\mormerge.tcl                       !dest_scripts!
    call :copyFile tools_gpl\mormerge\scripts\run_mormerge.bat                   !dest_scripts!
    call :copyFile tools_gpl\mormerge\packages\mormerge\x64\Release\mormerge.exe !dest_bin!
    call :copyFile third_party_open\tcl\bin\win64\tclkitsh852.exe                !dest_share!
goto :endproc



rem ==============
rem === INSTALL_VS
rem ==============
:vs
    echo "installing vs . . ."

    set dest="!dest_main!\x64\util\bin"

    call :makeDir !dest!

    call :copyFile tools_gpl\vs\bin\x64\Release\vs.exe !dest!
goto :endproc



rem ===================
rem === INSTALL NESTHD1
rem ===================
:nesthd1
    echo "installing nesthd1 . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\nesthd1\packages\nesthd1\x64\Release\nesthd1.exe !dest_bin!
goto :endproc



rem ===================
rem === INSTALL NESTHD2
rem ===================
:nesthd2
    echo "installing nesthd2 . . ."

    set dest_bin="!dest_main!\x64\dflow2d3d\bin"

    call :makeDir !dest_bin!

    call :copyFile tools_gpl\nesthd2\packages\nesthd2\x64\Release\nesthd2.exe !dest_bin!
goto :endproc



rem ===================
rem === INSTALL NESTWQ1
rem ===================
:nestwq1
    rem echo "installing nestwq1 . . ."

    rem set dest_bin="!dest_main!\x64\dwaq\bin"

    rem call :makeDir !dest_bin!

    rem call :copyFile tools\nestwq1\packages\nestwq1\x64\Release\nestwq1.exe !dest_bin!
goto :endproc



rem ===================
rem === INSTALL NESTWQ2
rem ===================
:nestwq2
    rem echo "installing nestwq2 . . ."

    rem set dest_bin="!dest_main!\x64\dwaq\bin"

    rem call :makeDir !dest_bin!

    rem call :copyFile tools\nestwq2\packages\nestwq2\x64\Release\nestwq2.exe !dest_bin!
goto :endproc



rem =====================
rem === INSTALL IO_NETCDF
rem =====================
:io_netcdf
    echo " WARNING: DISABLED: oss-install_x64::io_netcdf . . ."
goto :endproc



rem =====================
rem === INSTALL EC_MODULE
rem =====================
:ec_module
    echo " WARNING: DISABLED: oss-install_x64::ec_module . . ."
goto :endproc



rem =====================
rem === INSTALL GRIDGEOM
rem =====================
:gridgeom
    echo " WARNING: DISABLED: oss-install_x64::gridgeom . . ."
goto :endproc




rem =======================
rem === GET LOCK ==========
rem =======================
:getlock
    rem echo "getlock start"
    set counter=0
    :getlockloop
        set filecount=0
        for %%x in (!dest_main!\oss-install_lockfile_*.txt) do set /a filecount+=1
        rem echo filecount: !filecount!
        if !filecount! GTR 0 (
            set /A counter=%counter%+1
            rem echo getlock waits for !counter! time
            call :waitfunction 5
        ) else (
            rem echo Creating lockfile named !lockfile!
            echo This file is created by oss-install_x64.cmd in directory %~dp0 >!lockfile!
            call :waitfunction 2
        )
        if !counter! GTR 10 (
            goto getlockfinishedwaiting
        )
        set filecount=0
        for %%x in (!dest_main!\oss-install_lockfile_*.txt) do set /a filecount+=1
        rem echo filecount: !filecount!
        if !filecount! EQU 1 (
            rem echo !myid!: yes I have a lock
            goto getlockfinishedwaiting
        ) else (
            rem echo !myid!: too many lock trials, removing mine and try again
            del /f "!lockfile!" > del_!myid!.log 2>&1
            del /f del_!myid!.log
        )
    goto :getlockloop
    :getlockfinishedwaiting
        if !counter! GTR 10 (
            rem echo Unable to lock destination directory, continueing without lock
        )
    rem echo "getlock end"
goto :endproc





rem =======================
rem === RELEASE LOCK ======
rem =======================
:releaselock
    rem echo "releaselock start"
    if exist !lockfile! (
        rem echo Deleting !lockfile!
        del /f "!lockfile!" > del_!myid!.log 2>&1
        del /f del_!myid!.log
    ) else (
        rem echo !lockfile! does not exist
    )
    rem echo "releaselock end"
goto :endproc



rem =======================
rem === WAITFUNCTION ======
rem =======================
:waitfunction
    rem See https://www.robvanderwoude.com/wait.php
    rem "timeout" is not allowed by VisualStudio: ERROR: Input redirection is not supported, exiting the process immediately.

    rem echo waiting %~1 pings
    PING localhost -n %~1 >NUL
goto :endproc




:end
if NOT %globalErrorLevel% EQU 0 (
    rem
    rem Only jump to :end when the script is completely finished
    rem
    echo An error occurred while executing this file
    echo Returning with error number %globalErrorLevel%
    exit /B %globalErrorLevel%
)

:endproc
   rem
   rem No exit here
   rem Otherwise the script exits directly at the first missing artefact
