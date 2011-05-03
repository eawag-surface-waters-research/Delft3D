#!/bin/sh
    #
    # This script is an example for running Delft3D-FLOW
    # Adapt and use it for your own purpose
    #
    # adri.mourits@deltares.nl
    # 27 Dec 2010
    # 
    #
    # This script starts a single-domain Delft3D-FLOW computation on Linux
    #


    #
    # Set the config file here
    # 
argfile=config_flow2d3d.ini





    #
    # Set the directory containing delftflow.exe here
    #
export ARCH=intel
export D3D_HOME=../../bin
exedir=$D3D_HOME/$ARCH/flow/bin
 
    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
    # Only needed for the debug version:
    #. /opt/intel/Compiler/11.0/081/bin/ifortvars.sh ia32
export LD_LIBRARY_PATH=$exedir:$LD_LIBRARY_PATH 

    # Run
$exedir/deltares_hydro.exe $argfile
