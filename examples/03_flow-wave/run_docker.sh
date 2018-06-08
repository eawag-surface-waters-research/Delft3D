#!/bin/bash
    #
    # This script is an example for running Delft3D-FLOW in Docker (Linux container)
    # Adapt and use it for your own purpose
    #
    # 
    #
    # This script starts a single-domain Delft3D-FLOW computation online with Delft3D-WAVE on Linux 
    #

    #
    # Set the config file here
    # 
argfile=config_d_hydro.xml
mdwfile=r17.mdw




    #
    # Set the directories containing the binaries here
    #
export ARCH=lnx64
export D3D_HOME=/opt/delft3d_latest
flowexedir=$D3D_HOME/$ARCH/flow2d3d/bin
waveexedir=$D3D_HOME/$ARCH/wave/bin
swanexedir=$D3D_HOME/$ARCH/swan/bin
swanbatdir=$D3D_HOME/$ARCH/swan/scripts
 
    #
    # No adaptions needed below
    #

    # Start FLOW
export LD_LIBRARY_PATH=$flowexedir:$LD_LIBRARY_PATH
$flowexedir/d_hydro.exe $argfile &

    # Start WAVE
export LD_LIBRARY_PATH=$swanbatdir:$swanexedir:$waveexedir:$LD_LIBRARY_PATH 
export PATH=$swanbatdir:$PATH 
$waveexedir/wave.exe $mdwfile 1

    # Wait until all child processes are finished
wait
