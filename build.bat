@ echo off

setlocal enabledelayedexpansion
set prepareonly=0
set mode=
set config=
set generator=
set vs=
set ifort=
set coverage=0

rem # Jump to the directory where this build.bat script is
cd %~dp0
set root=%CD%




call :CheckCMakeInstallation
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :GetArguments %1 %2
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :GetEnvironmentVars
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :PrepareSln
if !ERRORLEVEL! NEQ 0 exit /B %~1
call :SetGenerator
if !ERRORLEVEL! NEQ 0 exit /B %~1


echo.
echo     config      : !config!
echo     generator   : !generator!
echo     ifort       : !ifort!
echo     mode        : !mode!
echo     prepareonly : !prepareonly!
echo     coverage    : !coverage!
echo     vs          : !vs!
echo.



call :Checks
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :vcvarsall
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :traditionalBuild
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :DoCMake !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

if "!config!" == "dflowfm_interacter" call :set_dflowfm_interacter_link_flag
if !ERRORLEVEL! NEQ 0 exit /B %~1

if !coverage! EQU 1 call :insertCoverage !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :Build !config!
if !ERRORLEVEL! NEQ 0 exit /B %~1

call :installall
if !ERRORLEVEL! NEQ 0 exit /B %~1

echo.
echo Visual Studio sln-files:
echo D-Flow FM : %root%\build_dflowfm\dflowfm.sln
echo DIMR      : %root%\build_dimr\dimr.sln
echo DWAQ      : %root%\build_dwaq\dwaq.sln
echo Tests     : %root%\build_tests\tests.sln
echo Other     : %root%\src\delft3d_open.sln
echo             %root%\src\ec_module.sln
echo             %root%\src\io_netcdf.sln
echo             %root%\src\nefis.sln
echo             %root%\src\utils_lgpl.sln
echo             %root%\src\utils_lgpl_no_tests.sln
echo.
echo Finished
goto :end










rem ===
rem === PROCEDURES
rem ===



rem =================================
rem === Command line arguments    ===
rem =================================
:GetArguments
    echo.
    echo "Get command line arguments ..."
    set mode=quiet
    set prepareonly=0
    if [%1] EQU [] (
        set mode=interactive
        set prepareonly=1
    )
    if "%1" == "--help" (
        goto usage
    )
    if "%1" == "all" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%1" == "dimr" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%1" == "dflowfm" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%1" == "dflowfm_interacter" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%1" == "dwaq" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%1" == "swan" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%1" == "tests" (
        set prepareonly=0
        set config=%1
        set mode=quiet
    )
    if "%2" == "--coverage" (
        set coverage=1
    )
    goto :endproc



rem =================================
rem === Get environment variables ===
rem =================================
:GetEnvironmentVars
    echo.
    echo "Get environment variables ..."
    rem # Check combinations!
    rem # VISUAL STUDIO: First try the official version (2017), then newer (2019) then older (2015), stop on success
    rem # IFORT: Order (overwriting when already found) in VS17: IFORT21, IFORT19, IFORT18(=official combination)
    rem #                                               in VS19: IFORT21, IFORT19
    rem #                                               in VS15: IFORT21, IFORT19, IFORT18, IFORT16(=previous official combination)
    rem # On TeamCity VS2019 is installed without IFORT
    if NOT "%VS2017INSTALLDIR%" == "" (
        set vs=2017
        echo Found: VisualStudio 15 2017
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=21
            echo Found: Intel Fortran 2021
        )
        if NOT "%IFORT_COMPILER19%" == "" (
            set ifort=19
            echo Found: Intel Fortran 2019
        )
        if NOT "%IFORT_COMPILER18%" == "" (
            set ifort=18
            echo Found: Intel Fortran 2018
        )
        goto :endproc
    )
    if NOT "%VS2019INSTALLDIR%" == "" (
        set vs=2019
        echo Found: VisualStudio 16 2019
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=21
            echo Found: Intel Fortran 2021
        )
        if NOT "%IFORT_COMPILER19%" == "" (
            set ifort=19
            echo Found: Intel Fortran 2019
        )
        goto :endproc
    )
    if NOT "%VS2015INSTALLDIR%" == "" (
        set vs=2015
        echo Found: VisualStudio 14 2015
        if NOT "%IFORT_COMPILER21%" == "" (
            set ifort=21
            echo Found: Intel Fortran 2021
        )
        if NOT "%IFORT_COMPILER19%" == "" (
            set ifort=19
            echo Found: Intel Fortran 2019
        )
        if NOT "%IFORT_COMPILER18%" == "" (
            set ifort=18
            echo Found: Intel Fortran 2018
        )
        if NOT "%IFORT_COMPILER16%" == "" (
            set ifort=16
            echo Found: Intel Fortran 2016
        )
        goto :endproc
    )
    goto :endproc



rem ================================
rem === Check CMake installation ===
rem ================================
:CheckCMakeInstallation
    echo.
    echo "Checking whether CMake is installed ..."
    set count=1
    for /f "tokens=* usebackq" %%f in (`cmake --version`) do (
      if !count! LEQ 1 (
          set var!count!=%%f
          set /a count=!count!+1
      )
    )
    if "!var1:~0,13!" == "cmake version" (
        echo cmake version: !var1:~13,20!
    ) else (
        echo ERROR: CMake not found.
        echo        Download page: https://cmake.org/download/
        echo        Be sure that the cmake directory is added to environment parameter PATH
        goto :end
    )
    goto :endproc



rem =======================
rem === Prepare_sln    ====
rem =======================
:PrepareSln
    if "!mode!" == "quiet" (
        if "%config%" == "all" (
            echo.
            echo "Calling prepare_sln.py in quiet mode ..."
            cd /d "%root%\src\"
            python prepare_sln.py -vs !vs! -ifort !ifort! -cmakeConfig none -cmakePreparationOnly no
        )
    ) else (
        echo.
        echo "Calling prepare_sln.py in interactive mode ..."
        cd /d "%root%\src\"
        rem # Catch stdout, each line in parameter var1, var2 etc.
        for /f "tokens=* usebackq" %%f in (`python prepare_sln.py`) do (
            set var=%%f
            if "!var:~0,19!" == "CMake configuration"    (
                set config=!var:~25,20!
            )
            if "!var:~0,22!" == "Visual Studio  Version" (
                set vs=!var:~25,4!
            )
            if "!var:~0,16!" == "Preparation only"       (
                set prepareonly=!var:~25,1!
            )
        )
    )
    goto :endproc



rem =======================
rem === SetGenerator   ====
rem =======================
:SetGenerator
    if "!vs!" == "2015" (
        set generator="Visual Studio 14 2015"
    )
    if "!vs!" == "2017" (
        set generator="Visual Studio 15 2017"
    )
    if "!vs!" == "2019" (
        set generator="Visual Studio 16 2019"
    )
    goto :endproc



rem =======================
rem === Checks         ====
rem =======================
:Checks
    if "!config!" == "" (
        echo ERROR: config is empty.
        set ERRORLEVEL=1
        goto :end
    )
    if "!generator!" == "" (
        echo ERROR: generator is empty.
        echo        Possible causes:
        echo            In prepare_sln.py:
        echo                Choosen Visual Studio version is not installed
        set ERRORLEVEL=1
        goto :end
    )
    goto :endproc



rem =================
rem === vcvarsall ===
rem =================
:vcvarsall
    rem # Execute vcvarsall.bat in case of compilation
    if %prepareonly% EQU 1 goto :endproc
    if !ERRORLEVEL! NEQ 0 goto :endproc
    
    echo.
    if !generator! == "Visual Studio 14 2015" (
        echo "Calling vcvarsall.bat for VisualStudio 2015 ..."
        call "%VS140COMNTOOLS%..\..\VC\vcvarsall.bat" amd64
    )
    if !generator! == "Visual Studio 15 2017" (
        echo "Calling vcvarsall.bat for VisualStudio 2017 ..."
        call "%VS2017INSTALLDIR%\VC\Auxiliary\Build\vcvarsall.bat" amd64
    )
    if !generator! == "Visual Studio 16 2019" (
        echo "Calling vcvarsall.bat for VisualStudio 2019 ..."
        call "%VS2019INSTALLDIR%\VC\Auxiliary\Build\vcvarsall.bat" amd64
    )
    
    rem # Execution of vcvarsall results in a jump to the C-drive. Jump back to the script directory
    cd /d "%root%\"
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :endproc



rem =======================
rem === DoCMake        ====
rem =======================
:DoCMake
    if !ERRORLEVEL! NEQ 0 goto :endproc
    echo.
    call :createCMakeDir build_%~1
    echo "Running CMake for %~1 ..."
    cd /d "%root%\build_%~1\"
    cmake ..\src\cmake -G %generator% -A x64 -B "." -D CONFIGURATION_TYPE="%~1" 1>cmake_%~1.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage
    goto :endproc



rem =========================================
rem === set_dflowfm_interacter_link_flag ====
rem =========================================
:set_dflowfm_interacter_link_flag
    rem # Ugly workaround to change "LinkLibraryDependencies=false" into "LinkLibraryDependencies=true"
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "s/LinkLibraryDependencies=\"false\"/LinkLibraryDependencies=\"true\"/g" "%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli.vfproj" >"%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli_new.vfproj"
    del "%root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli.vfproj"  > del.log 2>&1
    del /f/q del.log
    rename %root%\build_dflowfm_interacter\dflowfm_cli_exe\dflowfm-cli_new.vfproj dflowfm-cli.vfproj
    goto :endproc



rem =======================
rem === insertCoverage ====
rem =======================
:insertCoverage
    rem Insert options to implement the build objects with hooks for the code-coverage tool.
    rem This code is running from within build_%~1
    python %root%\src\scripts_lgpl\win64\testcoverage\addcovoptions.py %~1.sln


rem =======================
rem === Build          ====
rem =======================
:Build
    if %prepareonly% EQU 1 goto :endproc
    if !ERRORLEVEL! NEQ 0 goto :endproc
    echo.
    echo "Building %~1 ..."
    cd /d "%root%\build_%~1\"
    call :VSbuild %~1
    cd /d "%root%\"
    goto :endproc



rem =========================
rem === traditionalBuild ====
rem =========================
:traditionalBuild
    if %prepareonly% EQU 1 goto :endproc
    if !ERRORLEVEL! NEQ 0 goto :endproc
    
    if "!config!" == "all" (
        echo.
        echo "Building in the traditional way (only when config is all) ..."

        echo "    Clean directory %root%\src\bin\x64 ..."
        rmdir /s /q "%root%\src\bin\x64"  > del.log 2>&1
        del /f/q del.log

        cd /d "%root%\src\"
        echo "    Compiling delft3d_open.sln ..."
        call :VSbuild delft3d_open
        echo "    Compiling utils_lgpl_no_tests.sln ..."
        call :VSbuild utils_lgpl_no_tests
        rem # Disabled: causes errors: call :VSbuild nefis
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
    goto :endproc



rem =======================
rem === VSbuild        ====
rem =======================
:VSbuild
    echo.
    echo "Building with VisualStudio: %~1 ..."
    
    set currentWorkDir=%CD%
    devenv.com %~1.sln /Clean "Release|x64"
    devenv.com %~1.sln /Build "Release|x64" 1>%currentWorkDir%\build_%~1.log 2>&1
    if !ERRORLEVEL! NEQ 0 call :errorMessage

    rem # In build.log, replace "error" by TeamCity messages
    %root%\src\third_party_open\commandline\bin\win32\sed.exe -e "/[Ee]rror[\:\ ]/s/^/\#\#teamcity\[buildStatus status\=\'FAILURE\' text\=\' /g;/buildStatus/s/$/\'\]/g" %currentWorkDir%\build_%~1.log 
    goto :endproc



rem =======================
rem === InstallAll     ====
rem =======================
:installall
    if %prepareonly% EQU 1                goto :endproc
    if !ERRORLEVEL! NEQ 0                 goto :endproc

    if "!config!" == "all" (
        echo.
        echo "Installing in build_all ..."
        xcopy %root%\src\bin\x64                                %root%\build_all\x64\                 /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dimr                 %root%\build_all\x64\dimr\            /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\bin\dflowfm* %root%\build_all\x64\dflowfm\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\bin\dfm*     %root%\build_all\x64\dflowfm\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\default      %root%\build_all\x64\dflowfm\default\ /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dflowfm\scripts      %root%\build_all\x64\dflowfm\scripts\ /E /C /Y /Q > del.log 2>&1

        xcopy %root%\build_all\x64\Release\dwaq\bin\delwaq.dll      %root%\build_all\x64\dwaq\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\delwaq*.exe     %root%\build_all\x64\dwaq\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\bin\waq_plugin*.dll %root%\build_all\x64\dwaq\bin\     /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\default             %root%\build_all\x64\dwaq\default\ /E /C /Y /Q > del.log 2>&1
        xcopy %root%\build_all\x64\Release\dwaq\scripts             %root%\build_all\x64\dwaq\scripts\ /E /C /Y /Q > del.log 2>&1


        xcopy %root%\build_all\x64\Release\share\bin            %root%\build_all\x64\share\bin\       /E /C /Y /Q > del.log 2>&1
        rmdir /s /q %root%\build_all\x64\Release > del.log 2>&1
        del /f/q del.log
    )
    goto :endproc



rem =======================
rem === USAGE        ======
rem =======================
:usage
    echo.
    echo.
    echo.
    echo Usage:
    echo "build.bat"
    echo "build.bat <CONFIG> [OPTIONS]"
    echo "  Without <CONFIG>:"
    echo "    The prepare_sln.py window will pop-up. Related commands will be executed."
    echo "  With    <CONFIG>:"
    echo "    The following actions will be executed:"
    echo "    - Only when <CONFIG>=all: Execute prepare_sln.py in silent mode"
    echo "    - Only when <CONFIG>=all: Compile all engines that are not CMaked yet in the traditional way"
    echo "    - Create directory 'build_<CONFIG>'"
    echo "      Delete it when it already existed"
    echo "    - Execute 'CMake <CONFIG>' to create file '<CONFIG>.sln' inside 'build_<CONFIG>'"
    echo "    - Execute 'devenv.com <CONFIG>.sln /Build'"
    echo "    - Only when <CONFIG>=all: Combine all binaries in 'build_<CONFIG>\x64'"
    echo.
    echo "<CONFIG>:"
    echo "- all: All CMaked projects, currently D-Flow FM, DWAQ and DIMR, and not-CMaked projects"
    echo "- dflowfm"
    echo "- dflowfm_interacter"
    echo "- dwaq"
    echo "- dimr"
    echo "- tests"
    echo.
    echo "Options:"
    echo "--coverage"
    echo "    Instrument object files for code-coverage tool (codecov)."
    echo.
    echo "More info  : https://oss.deltares.nl/web/delft3d/source-code"
    echo "About CMake: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/cmake/doc/README"
    echo.
    set ERRORLEVEL=1
    goto :end



rem =======================
rem === errorMessage ======
rem =======================
:errorMessage
    echo.
    echo.
    echo.
    echo ERROR: Check log files in build_* directories
    echo        Is the correct Fortran compiler installed, available and selected?
    echo        Common problem: NetExtender needs to be switched on to reach the license server.
    goto :endproc


rem =======================
rem === END TAG      ======
rem =======================
:end
    rem # To prevent the DOS box from disappearing immediately: remove the rem on the following line
    if "!mode!" == "interactive" (
        pause
    )
    if !ERRORLEVEL! NEQ 0 (
        exit /B %~1
    ) else (
        exit /B
    )

rem =======================
rem === ENDPROC TAG  ======
rem =======================
:endproc
   rem # No exit  here, otherwise the script exits directly at the first missing artefact
   rem # No pause here, otherwise a pause will appear after each procedure execution
   

