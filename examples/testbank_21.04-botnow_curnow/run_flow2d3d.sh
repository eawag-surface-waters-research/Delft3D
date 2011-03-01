#!/bin/sh
    #
    # This script is an example for running Delft3D-FLOW
    # Adapt and use it for your own purpose
    #
    # adri.mourits@deltares.nl
    # 01 Mar 2011
    # 
    #
    # This script starts a single-domain Delft3D-FLOW computation online with Delft3D-WAVE on Linux
    #


    #
    # Set the config file and mdw file
    # 
argfile=config_flow2d3d.ini
mdwfile=r17.mdw




    #
    # Set the directory containing delftflow.exe
    #
exedir=../../src/bin
libdir=../../src/lib
wavedir=$exedir
swandir=../../src/third_party_open/swan/bin/intel
swanbatdir=../../src/third_party_open/swan/scripts
 
    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
curdir=`pwd`
export D3D_HOME=$curdir\..\..\src
    # Only needed for the debug version:
    #. /opt/intel/Compiler/11.0/081/bin/ifortvars.sh ia32
export LD_LIBRARY_PATH=$swanbatdir:$exedir:$libdir:$LD_LIBRARY_PATH 

    # Run
$exedir/deltares_hydro.exe $argfile $

$wavedir\wave.exe $mdwfile 1
