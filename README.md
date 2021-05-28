About compiling https://svn.oss.deltares.nl/repos/delft3d/trunk :
=======================================================
This code is in a transition phase, switching to using CMake. Both on Windows and Linux.
This transition is planned to end in 2021.

For now: Please use the latest tagged versions without CMake dependency:
D-Flow FM: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3dfm/68819
Delft3D4 : https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936

In case you do want to compile the trunk:
Currently, D-Flow FM and DIMR are using CMake; the rest is built in the traditional way (although CMake preparations are present).
At Deltares, this works fine (not tested outside Deltares):

At the toplevel, https://svn.oss.deltares.nl/repos/delft3d/trunk :
Windows:

- build.bat
  prepare_sln.py interface pops up, all sln-files are prepared when clicking "Apply"

- build.bat all

  Silent building of the full tree

- build.bat dflowfm
  Only create an sln-file for D-Flow FM (without Interacter)

- build.bat dflowfm_interacter
  Only create an sln-file for D-Flow FM with the not-open-source Interacter. You have to provide an Interacter library yourself.
  See ...\src\engines_gpl\dflowfm\interacter\README

Linux:

- ./build.sh all
- ./build.sh dflowfm

More information:
https://oss.deltares.nl/web/delft3d/source-code
CMake: ...\src\cmake\doc\README

