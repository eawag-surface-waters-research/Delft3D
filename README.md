About compiling https://svn.oss.deltares.nl/repos/delft3d/trunk
===============================================================
This code is in a transition phase, switching to using CMake. Both on Windows and Linux.
This transition is planned to end in 2021.

For now: Please use the latest tagged versions without CMake dependency:
D-Flow FM: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3dfm/68819
Delft3D4 : https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936

In case you do want to compile the trunk:
Currently, D-Flow FM, D-WAQ, D-Waves and DIMR,  are using CMake; the rest is built in the traditional way (although CMake preparations are present).
At Deltares, this works fine (not tested outside Deltares):

At the toplevel, https://svn.oss.deltares.nl/repos/delft3d/trunk :
Windows:
- build.bat
  Execute "build.bat --help" to show the usage
  Currently used as default build process: "build.bat all -vs 2019 -ifort 21"

Linux:
- build.sh
  Execute "./build.sh --help" to show the usage
  Currently used as default build process: "./build.sh all --compiler intel21"

More information:
https://oss.deltares.nl/web/delft3d/source-code
CMake: ...\src\cmake\README



Debugging DIMR in VisualStudio
==============================
- Use build.bat/prepare_sln to prepare the "all" configuration
- Open "...\build_all\all.sln" in VisualStudio and build the complete release version
  Directory "...\build_all\x64\Release\share\bin" will be created
- Build the debug versie of what you need (e.g. dimr and dflowfm, waq, wave)
- dimr project -> Set as Startup Project
- dimr project -> properties -> Debugging:
    -> Command Arguments: dimr_config.xml
    -> Working Directory: ...\examples\12_dflowfm\test_data\e100_f02_c02-FriesianInlet_schematic_FM
    -> Environment: PATH=...\build_all\x64\Debug;%PATH%;...\build_all\x64\Release\share\bin


Note: in this README.md:
Replace "..." by the actual path on your system to the checkout directory.
