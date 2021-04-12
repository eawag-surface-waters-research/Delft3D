@ echo off

setlocal enabledelayedexpansion
set globalErrorLevel=0
set prepareonly=0
set config=
set generator=

rem Jump to the directory where this build.bat script is
cd %~dp0
set root=%CD%



rem ===
rem === Check CMake installation
rem ===
echo.
echo "Checking whether CMake is installed ..."
set count=1
for /f "tokens=* usebackq" %%f in (`cmake --version`) do (
  set var!count!=%%f
  set /a count=!count!+1
)
if "!var1:~0,13!" == "cmake version" (
    echo cmake version: !var1:~13,20!
) else (
    echo ERROR: CMake not found.
    echo        Download page: https://cmake.org/download/
    echo        Be sure that the cmake directory is added to environment parameter PATH
    goto :end
)



rem ===
rem === Command line argument
rem ===
rem No arguments: call prepare_sln.py
if [%1] EQU [] (
    call :preparesln
)
if [%1] EQU [--help] (
    goto usage
)
if [%1] EQU [-p] (
    set prepareonly=1
    set config=%2
)
if [%2] EQU [-p] (
    set prepareonly=1
    set config=%1
)
if [%1] EQU [--prepareonly] (
    set prepareonly=1
    set config=%2
)
if [%2] EQU [--prepareonly] (
    set prepareonly=1
    set config=%1
)



rem ===
rem === Echo
rem ===
echo.
echo     config      : !config!
echo     generator   : !generator!
echo     prepareonly : !prepareonly!
echo.
if "!config!" == "" (
    echo ERROR: config is empty.
    goto :end
)
if "!generator!" == "" (
    echo ERROR: generator is empty.
    echo        Possible causes:
    echo            In prepare_sln.py:
    echo                Choosen Visual Studio version is not installed
    goto :end
)


rem ===
rem === vcvarsall
rem ===
rem Execute vcvarsall.bat in case of compilation
if NOT !prepareonly! EQU 1 (
    echo.
    if NOT !generator!=="Visual Studio 15 2017" (
        echo ERROR build.bat: compilation is only implemented bases on !generator!
        goto :end
    )
    echo "Calling vcvarsall.bat for VisualStudio 2017 ..."
    call "%VS2017INSTALLDIR%\VC\Auxiliary\Build\vcvarsall.bat" amd64

    rem Execution of vcvarsall results in a jump to the C-drive. Jump back to the script directory
    cd /d "%root%\"
)
    


rem ===
rem === CMake and build
rem ===
call :CMakeBuild dimr
call :CMakeBuild dflowfm
call :traditionalBuild



echo.
echo Visual Studio sln-files:
echo D-Flow FM : %root%\build_dflowfm\dflowfm.sln
echo DIMR      : %root%\build_dimr\dimr.sln
echo Other     : %root%\src\delft3d_open.sln
echo             %root%\src\ec_module.sln
echo             %root%\src\io_netcdf.sln
echo             %root%\src\nefis.sln
echo             %root%\src\tests.sln
echo             %root%\src\utils_lgpl.sln
echo.
echo Finished
goto :end
rem ===
rem === Finished
rem ===






rem ===
rem === Subroutines
rem ===



rem =======================
rem === Prepare_sln    ====
rem =======================
:preparesln
    echo.
    echo "Calling prepare_sln.py ..."
    cd /d "%root%\src\"

    rem Catch stdout, each line in parameter var1, var2 etc.
    set count=1
    for /f "tokens=* usebackq" %%f in (`python prepare_sln.py`) do (
      set var!count!=%%f
      set /a count=!count!+1
    )

    rem Get config      from var1="CMake configuration    : all"
    set config=!var1:~25,20!

    rem Get generator   from var2="Visual Studio  Version : 2017"
    set VS=!var2:~25,4!
    if "!VS!" == "2017" set generator="Visual Studio 15 2017"
    if "!VS!" == "2019" set generator="Visual Studio 16 2019"

    rem Get prepareonly from var6="Preparation only       : 0"
    if "!var6:~25,1!" == "0" set prepareonly=0
    if "!var6:~25,1!" == "1" set prepareonly=1
    goto :endproc



rem =======================
rem === CMakeBuild     ====
rem =======================
:CMakeBuild
    set result=false
    if "%config%" == "%~1"  set result=true
    if "%config%" == "all"  set result=true
    if "%result%" == "true" (
        echo.
        echo "Processing %~1 ..."
        call :createCMakeDir build_%~1
        call :runCMake %~1
        if NOT %prepareonly% EQU 1 (
            cd /d "%root%\build_%~1\"
            call :VSbuild %~1
        )
        cd /d "%root%\"
    )
    goto :endproc



rem =========================
rem === traditionalBuild ====
rem =========================
:traditionalBuild
    if NOT !prepareonly! EQU 1 (
        set result=false
        if "!config!" == "all"  set result="true"
        if !result! == "true" (
            echo.
            echo "Building in the traditional way (only when config is all) ..."
            cd /d "%root%\src\"
            call :VSbuild delft3d_open
            call :VSbuild io_netcdf
            call :VSbuild nefis
            cd /d "%root%\"
        )
    )
    goto :endproc


rem =======================
rem === createCMakeDir ====
rem =======================
:createCMakeDir
    echo.
    echo "Creating directory %root%\%~1 ..."
    cd /d %root%
    if exist "%root%\%~1\" rmdir /s/q "%root%\%~1\" > del.log 2>&1
    mkdir    "%root%\%~1\"                          > del.log 2>&1
    del /f/q del.log
    cd /d "%root%\%~1\"
    goto :endproc



rem =======================
rem === runCMake       ====
rem =======================
:runCMake
    echo.
    echo "Running CMake for %~1 ..."
    cmake ..\src\cmake -G %generator% -A x64 -B "." -D CONFIGURATION_TYPE="%~1" 1>cmake_%~1.log 2>&1
    goto :endproc



rem =======================
rem === VSbuild        ====
rem =======================
:VSbuild
    echo.
    echo "Building with VisualStudio: %~1 ..."
    echo     In case of crash:
    echo          Is the Fortran compiler installed and available?
    echo          Common problem: NetExtender needs to be switched on to reach the license server
    devenv.com %~1.sln /Build "Release|x64" /Out build_%~1.log 1>devenv.log 2>&1
    if NOT %ErrorLevel% EQU 0 (
        echo "Error in compiling %~1.sln: %ErrorLevel%"
        set globalErrorLevel=%ErrorLevel%
    )

    rem In build.log, replace "error" by TeamCity messages
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" build_%~1.log 
    goto :endproc



rem =======================
rem === USAGE        ======
rem =======================
:usage
    echo.
    echo.
    echo.
    echo Usage:
    echo "build.bat <CONFIG> [OPTIONS]"
    echo "- Create directory 'build_<CONFIG>'"
    echo "  Delete it when it already existed"
    echo "- Execute 'CMake <CONFIG>' to create makefile inside 'build_<CONFIG>'"
    echo "- Execute 'make install'"
    echo "- Execute prepare_sln.py"
    echo "- Compile all engines that are not CMaked yet"
    echo.
    echo "<CONFIG>:"
    echo "- missing: execute prepare_sln.py"
    echo "- 'all':"
    echo "  - build dflowfm_open"
    echo "  - build dimr"
    echo "  - build all other engines in the traditional way"
    echo "  - combine all binaries"
    echo "- dflowfm_open"
    echo "- dimr"
    echo.
    echo "Options:"
    echo "-p, --preparationsonly"
    echo "       Only CMake, no make, no src/build.cmd"
    goto :end


rem =======================
rem === END TAG      ======
rem =======================
:end

if NOT %globalErrorLevel% EQU 0 (
    echo An error occurred in one or more compilation steps
    echo Returning with error number %globalErrorLevel%
    exit /B %globalErrorLevel%
)

    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause



rem =======================
rem === ENDPROC TAG  ======
rem =======================
:endproc
   rem
   rem No exit  here, otherwise the script exits directly at the first missing artefact
   rem No pause here, otherwise a pause will appear after each procedure execution
   

