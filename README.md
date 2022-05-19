# About compiling https://svn.oss.deltares.nl/repos/delft3d/trunk
For compiling Delft3D4-FLOW: Do not use this "README.md", but "src\README".   
Use this "README.md" for compiling all other kernels in this repository.


### At the toplevel, https://svn.oss.deltares.nl/repos/delft3d/trunk :   
#### Windows:   
- build.bat   
  Execute "build.bat --help" to show the usage   
  Currently used as default build process: "build.bat all -vs 2019 -ifort 21"   
  This will execute "Microsoft_VisualStudio\vcvarsall.bat". When using other versions, modifications will be needed.   

#### Linux:
- build.sh   
  Execute "./build.sh --help" to show the usage   
  Currently used as default build process: "./build.sh all --compiler intel21"   
  This will execute "src/setenv.sh" on Deltares systems. On other systems, the environment must be prepared upfront.   

#### Alternative: without build-script (Windows and Linux)
See ...\src\cmake\README   
WARNING: When building without build-script, the collection of the resulting binaries will need attention

#### More information:
- Delft3D FM suite: https://oss.deltares.nl/web/delft3dfm/get-started
- Delft3D 4  suite: https://oss.deltares.nl/web/delft3d/get-started



# Debugging DIMR in VisualStudio
- Use build.bat to prepare the "all" configuration
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
