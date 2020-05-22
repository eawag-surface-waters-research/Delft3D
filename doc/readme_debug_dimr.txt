Debugging a coupled D-Flow FM - D-Waves computation in Visual Studio
====================================================================

Below: replace "<root>" by the location of the source code on your machine, e.g. "c:\checkouts\oss"

0. Run prepare_sln.py
1. Compile Release version of D-Waves
   This ensures that SWAN and ESMF_regridder are in the correct location: <root>\src\bin\x64
2. Compile Debug version
   DIMR, D-Flow FM and D-Waves
3. DIMR is the StartUp Project, in properties->Debugging:
   a. Set workdir
   b. Set input file (dimr_config.xml)
   c. Environment: Add all relevant paths to environment parameter PATH. Syntax/example:
      PATH=<root>\src\engines_gpl\dimr\bin\x64\Debug;<root>\src\bin\x64\Debug\dflowfm\bin;<root>\src\engines_gpl\wave\bin\x64\Debug;<root>\src\bin\x64\swan\scripts;<root>\src\bin\x64\swan\bin;<root>\src\bin\x64\esmf\scripts;<root>\src\bin\x64\esmf\bin;%PATH%;<root>\src\bin\x64\share\bin
      Ending with "%PATH%;sharedir" gives the highest chance on success.
4. Start debugging
   Use "F11" to jump from DIMR to D-Flow FM/D-Waves. Put breakpoints at the (three) lines with "->dllinitialize" in "dimr_lib->src->dimr.cpp". After jumping into D-Flow FM/D-Waves with "F11", you can add more breakpoints.

Adri.Mourits@deltares.nl

