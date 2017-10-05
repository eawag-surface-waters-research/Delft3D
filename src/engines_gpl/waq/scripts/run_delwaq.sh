#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs Delwaq on Linux
    # Adapt and use it for your own purpose
    #

function print_usage_info {
    echo "Usage: ${0##*/} <delwaq.inp> [OPTION]..."
    echo "Run a Delwaq model on Linux."
    echo
    echo "<delwaq.inp>"
    echo "       Delwaq input file"
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       print this help message and exit"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
configfile=
procfile=
D3D_HOME=
ulimit -s unlimited


#
## Start processing command line options:

configfile=$1
shift
while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -h|--help)
    print_usage_info
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


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
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

procfile=$D3D_HOME/$ARCH/dwaq/default/proc_def
if [ ! -f $configfile ]; then
    echo "ERROR: procfile $procfile does not exist"
    print_usage_info
fi

echo "    Configfile       : $configfile"
echo "    Procfile         : $procfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    ARCH             : $ARCH"
echo "    Working directory: $workdir"
echo 

    #
    # Set the directories containing the binaries
    #

waqexedir=$D3D_HOME/$ARCH/dwaq/bin
shareddir=$D3D_HOME/$ARCH/shared


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$waqexedir:$shareddir:$LD_LIBRARY_PATH


    echo "executing:"
    echo "$waqexedir/delwaq1 $configfile -p $procfile"
    echo 
$waqexedir/delwaq1 $configfile -p "$procfile"

    #
    # Wait for any key to run delwaq 2
    #
wait
if [ $? == 0 ]
  then
    echo ""
    echo "Delwaq1 did run without errors."

    #
    # Run delwaq 2
    #
    echo "executing:"
    echo "$waqexedir/delwaq2 $configfile"
$waqexedir/delwaq2 $configfile

    if [ $? -eq 0 ]
      then
        echo ""
        echo "Delwaq2 did run without errors."
      else
        echo ""
        echo "Delwaq2 did not run correctly."
    fi
else
    echo ""
    echo "Delwaq1 did not run correctly, ending calculation"
fi



    # Wait until all child processes are finished
wait

