#!/bin/bash
    #
    # This script is an example for running Delft3D-FLOW in Docker (Linux container)
    # Adapt and use it for your own purpose
    #
    # 
    #
    # This script starts a parallel Delft3D-FLOW computation on Linux
    #

    #
    # Set the config file here
    # 
argfile=config_d_hydro.xml


export PATH=/usr/lib64/mpich/bin:$PATH
    #
    # Start mpich2
    # mpd may already have been started
mpd &
    # mpdboot: optional: --ncpus=$processes_per_node (adapted machinefile needed)
mpdboot -n 3 --rsh=/usr/bin/rsh 

    #
    # Set the directories containing the binaries here
    #
export ARCH=lnx64
export D3D_HOME=/opt/delft3d_latest
flowexedir=$D3D_HOME/$ARCH/flow2d3d/bin
    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
export LD_LIBRARY_PATH=$flowexedir:$LD_LIBRARY_PATH 

    # Run
mpirun -np 3 $flowexedir/d_hydro.exe $argfile

    # Wait until all child processes are finished
wait
mpdallexit
