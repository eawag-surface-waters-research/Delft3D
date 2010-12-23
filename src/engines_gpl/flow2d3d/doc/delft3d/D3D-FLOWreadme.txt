===============================
=== Delft3D-FLOW README.TXT ===
===============================
About this file: version: 1.0
                 date   : Sep 12, 2003
                 author : A.J. Mourits

This file descibes how to compile and run Delft3D-FLOW.

This file contains the following six chapters:
1) Getting FLOW source code
2) Compiling FLOW on PC
3) Compiling FLOW on UNIX
4) Running FLOW on PC
5) Running FLOW on UNIX
6) Example batch/script files


==========================
1 GETTING FLOW SOURCE CODE
==========================
ASSUMPTION:
- You have access to VSS

WARNING:
- Do not use "get latest version"

PROCEDURE:
This procedure should be followed when getting FLOW source code out
of VSS:
1) Select project $/Delft3D/modules/d3d-morflow/flowsrc
2) View "Project History" 
   (menu-tools-show history, check "labels only")
3) Select the labelled version you want to get and read the detailed
   information. Select another labelled version if the comments 
   contain something like 
   "WARNING: do not use this version as release, but only for further
    developments"
4) Get the FLOW code. 
   choose a destination directory (on network, local PC or UNIX),
   check "Recursive",
   check "Built tree (override working folders)", 
   check "Make writable" (not greyed out!).

NOTES:
- When starting new developments: write down carefully with what 
  version number you started! Also notify Jan Mooiman about the 
  number of the version you have taken out of VSS.
- VSS automatically adds a file named "vssver.scc" to every
  subdirectory.

======================
2 COMPILING FLOW ON PC
======================
ASSUMPTION:
- Microsoft Visual Studio (Developer Studio) is properly installed,
  including the Digital Fortran Compiler.

WARNING:
- Do not use "partial build" (menu-build-build subproject)
  when creating test versions.

PROCEDURE:
1) In Windows Commander or Explorer: double click on the file 
   "flow.dsw" in the directory "flow". Visual Studio will be
   started and the flow-project will be loaded automatically.
[optional: 2) Check that subproject "allflow files" is active, check
              that "win32 release" version is selected (or 
              "win32 debug" when needed).]
3) Build the complete project
   (menu-build-rebuild all)
   All libraries are compiled, the application is linked, the
   following three executables are generated in subdirectory 
   "bin\w32\release" (for release version) or 
   "bin\w32\debug" (for debug version):
   a) md-ver.exe
   b) tdatom.exe
   c) trisim.exe

NOTE:
- Additional files are generated in several directories (with
  extensions like .lib, .exp, .obj, .ilk, .res, .lst, .pdb). They are
  only used for partial build actions.


========================
3 COMPILING FLOW ON UNIX
========================
ASSUMPTION:
- A platform specific Fortran90 compiler is properly installed.
  Currently the Intel compiler is used. To be able to use it,
  the following line must be executed
  (you may add it to your S<user> initialization file):
  . /app/intelfort/compiler70/ia32/bin/ifcvars.sh

WARNINGS:
- Do not use "partial make" when creating test versions.

PROCEDURE:
1) Execute "make" in "d3d-morflow"; the following three 
   executables are generated in subdirectory "bin/release":
   a) md-ver.exe
   b) tdatom.exe
   c) trisim.exe

NOTES:
- To create a debug version:
  - load "arch\$(ARCH)\os.mkf" in an editor
  - Place the following three lines:
       F_DBG_OR_OPTIM	= $(FDEBUG)
       C_DBG_OR_OPTIM	= $(CDEBUG)
       DIR_DBG_OR_OPTIM	= debug
    below the three lines:
       F_DBG_OR_OPTIM	= $(FOPTIM)
       C_DBG_OR_OPTIM	= $(COPTIM)
       DIR_DBG_OR_OPTIM	= release
  - Execute "make clobber all" in d3d-morflow; the executables
    are generated in subdirectory "bin/debug"


====================
4 RUNNING FLOW ON PC
====================
ASSUMPTION:
- A working directory is present in which input/output files should
  be placed.
- The dll "pthreadvce.dll" is available;
  copy it from directory "lib\extern\w32" to, for example, "c:\windows\system32"

WARNING:
- The newly generated executables are NOT automatically used when
  starting flow via an official Delft3D-Menu release. There are
  two options to use the new executables:
  a) Use batch files to run the new executables (see procedure).
  b) Replace the officially released flow executables by the newly
     created ones (not recommended!).

PROCEDURE:
1) Check that the following network drive is mapped to the 
   J-directory:
   \\ELAND\app
2) Check that the environment variable "LM_LICENSE_FILE" is set to 
   the following value:
   J:\flexlm\etc\license.dat
3) Check that the environment variable "D3D_HOME" has the right 
   value. "D3D_HOME" is used to locate the "flow\default" directory,
   containing the three files "d3d_visu"-files. It must
   have a value such that "%D3D_HOME%\w32\flow\default" contains
   correct "d3d_visu"-files (see notes).
4) The following two files must be present in the working directory:
   a) A correct md-file with a name in the format "<runid>.mdf"
      Example: "f34.mdf"
   b) An ASCII file named "runid", containing the <runid> of the
      md-file to be executed.
      Example: the file named "runid" contains the text "f34"
[ optional: 5a) Execute the newly generated tdatom.exe in the working
                directory to generate the TMP-files containing time
                dependent information.
            5b) Execute the newly generated md-ver.exe in the working
                directory to check whether the md-file is correct.]
6a) Execute the newly generated tdatom.exe in the working directory
    to generate the TMP-files containing time dependent information.
6b) Execute the newly generated trisim.exe in the working directory
    to perform the calculation.

NOTES:
- Normally the "D3D_HOME"-value of an official Delft3D-Menu release 
  can be used.
- Script files can be used to "enhance" this process. For example
  "exampletest.bat" at the bottom of this file (for step 4 and 6).


======================
5 RUNNING FLOW ON UNIX
======================
SEE 4) RUNNING FLOW ON PC. Differences are listed below.

ASSUMPTIONS:
No differences with PC version

WARNINGS (Differences with PC version):
- Replacement of the officially released flow executables is not
  possible on UNIX.

PROCEDURE (Differences with PC version):
1) Mapping network drive is not needed on UNIX.
2) Environment variable "LM_LICENSE_FILE" should be set to 
   the following value:
   /app/delft3d/license.dat (see notes)
3) Environment variable "D3D_HOME" (and "ARCH") should be set such 
   that "$D3D_HOME/$ARCH/flow/default" contains
   correct "d3d_visu"-files. (see notes)
4) No differences with PC version.
[ optional: 5) No differences with PC version.]
6) No differences with PC version.

NOTES:
- Normally the "D3D_HOME"-value of the standard Delft3D profile can
  be used. Also for "LM_LICENSE_FILE".
- Script files can be used to "enhance" this process. For example
  "exampletest" at the bottom of this file (for step 3, 4 and 6).


============================
6 EXAMPLE BATCH/SCRIPT FILES
============================
Two batch/script files are listed below:
1) exampletest.bat; for enhancing running on PC
2) exampletest; for enhancing running on UNIX

NOTES:
- Different scripts are needed for a run using on of the following features:
  - Domain decomposition
  - Fluid mud
  - Real time control
  Contact the Delft3D team for more information
- Additional scripts may be necessary on UNIX (ipclean, esm_create, esm_delete)
  Contact the Delft3D team for more information

-----------------------
--- exampletest.bat ---
-----------------------
@ echo off

set runid=b01

set exedir=d:\delft3d\code\modules\d3d-morflow\bin\w32\release
           
echo exedir=%exedir%

rem ============ remove output files
del runid
del TMP*.*
del tri-diag.*
del *.msg
del com*.*
del fourier*.*
del md-diag*.*

rem  =========== create file runid
echo %runid% > runid

echo === start tdatom.exe ===
%exedir%\tdatom.exe
echo === end tdatom.exe ===

rem pause

echo === start trisim.exe ===
%exedir%\trisim.exe
rem %exedir%\md-ver.exe
echo === end trisim.exe ===

rem pause




-------------------
--- exampletest ---
-------------------
#!/usr/bin/sh

runid=l05

D3D_HOME=/u/mourits/delft3d
exedir=$D3D_HOME/code/modules/d3d-morflow/bin/$ARCH/release

export D3D_HOME

# remove temp files
rm runid
rm TMP*.*
rm tri*.*
rm *.msg
rm com*.*
rm fourier*.*
rm md-diag*.*

#  create file runid
echo $runid > runid

echo === start tdatom.exe ===
$exedir/tdatom.exe
echo === end tdatom.exe ===

echo === start trisim.exe ===
$exedir/trisim.exe
echo === end trisim.exe ===





==============================
=== END OF FILE README.TXT ===
==============================
