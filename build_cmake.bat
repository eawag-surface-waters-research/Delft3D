@ echo off

rem ===
rem Fill in or select your Visual Studio version here (CMake will find your newest Fortran compiler)
rem ===
set generator="Visual Studio 15 2017"
rem set generator="Visual Studio 16 2019"




setlocal enabledelayedexpansion
set globalErrorLevel=0


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
rem === CMake configuration
rem ===
call :CMakeBuild dimr
call :CMakeBuild dflowfm



echo.
echo Visual Studio sln-files:
echo D-Flow FM : %root%\build_dflowfm\dflowfm.sln
echo DIMR      : %root%\build_dimr\dimr.sln
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
rem === CMakeBuild     ====
rem =======================
:CMakeBuild
        echo.
        echo "Processing %~1 ..."
        call :createCMakeDir build_%~1
        call :runCMake %~1
        cd /d "%root%\"
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
   

