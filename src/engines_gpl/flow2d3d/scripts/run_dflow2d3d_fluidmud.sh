#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs two Delft3D-FLOW instances on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run two Delft3D-FLOW instances on Linux."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-wconfig config_d_hydro_sed.xml"
    echo "       config file for water phase, default config_d_hydro_sed.xml"
    echo "-mconfig config_d_hydro_mud.xml"
    echo "       config file for mud   phase, default config_d_hydro_mud.xml"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
waterconfigfile=config_d_hydro_sed.xml
mudconfigfile=config_d_hydro_mud.xml
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -h|--help)
    print_usage_info
    ;;
    -mconfig)
    mudconfigfile="$1"
    shift
    ;;
    -wconfig)
    waterconfigfile="$1"
    shift
    ;;
    --D3D_HOME)
    D3D_HOME="$1"
    shift
    ;;
	--NNODES)
    NNODES="$1"
    shift
    ;;
esac
done


if [ ! -f $waterconfigfile ]; then
    echo "ERROR: waterconfigfile $waterconfigfile does not exist"
    print_usage_info
fi
if [ ! -f $mudconfigfile ]; then
    echo "ERROR: mudconfigfile $mudconfigfile does not exist"
    print_usage_info
fi


workdir=`pwd`

if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    D3D_HOME=$scriptdir/../..
else
    # D3D_HOME is passed through via argument --D3D_HOME
    # Commonly its value is "/some/path/lnx64/scripts/../.."
    # Remove "/../.." at the end of the string
    scriptdir=${D3D_HOME%"/../.."}
fi
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME
 
    # find ARCH from scriptdir path
pth=( $( echo $scriptdir | tr "/" "\n" ) )
a=${#pth[@]}-2
export ARCH=${pth[a]}

echo "    Water config file: $waterconfigfile"
echo "    Mud   config file: $mudconfigfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    ARCH             : $ARCH"
echo "    Working directory: $workdir"
echo 

    #
    # Set the directories containing the binaries
    #

flow2d3dexedir=$D3D_HOME/$ARCH/dflow2d3d/bin
shareddir=$D3D_HOME/$ARCH/shared


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$shareddir:$flow2d3dexedir

    # Create shared memory block
esmContext=`$flow2d3dexedir/esm_create 2> /dev/null`
if [ $? -eq 0 ]; then
    # use context
    DIO_SHM_ESM=$esmContext
    export DIO_SHM_ESM
    echo "ESM context created: $esmContext"
else
    echo "Cannot create ESM context:"
    $flow2d3dexedir/esm_create
fi 

    echo "executing in background:"
    echo "$flow2d3dexedir/d_hydro.exe $waterconfigfile &"
    echo 
$flow2d3dexedir/d_hydro.exe $waterconfigfile &

echo waiting 5 seconds
sleep 5

    echo "executing in foreground:"
    echo "$flow2d3dexedir/d_hydro.exe $mudconfigfile"
    echo 
$flow2d3dexedir/d_hydro.exe $mudconfigfile


    # Wait until all child processes are finished
wait

$flow2d3dexedir/esm_delete $esmContext 2> /dev/null
if [ $? -ne 0 ]; then
    echo "Cannot delete ESM context $esmContext:"
    $flow2d3dexedir/esm_delete $esmContext
else
    echo "ESM context deleted: $esmContext"
fi 

