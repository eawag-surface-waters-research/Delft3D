#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs dimr on Linux
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # run_dimr.sh
    #
    # adri.mourits@deltares.nl
    # 15 Feb 2017
    #

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run a dimr model on Linux."
    echo
    echo "Options:"
    echo "-c, --corespernode <M>"
    echo "       number of cores per node, default $corespernodedefault"
    echo "-d, --debug 0xFFFFFFFF"
    echo "       maximum debug output"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-m, --masterfile <filename>"
    echo "       dimr configuration filename, default dimr_config.xml"
    echo "    --D3D_HOME <path>"
    echo "       path to binaries and scripts"
    echo "    --NNODES <N>"
    echo "       number of slots=NNODES*CoresPerNode, default 1 (not parallel)"
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
corespernodedefault=1
corespernode=1
debuglevel=0
configfile=dimr_config.xml
D3D_HOME=
runscript_extraopts=
NNODES=1
ulimit -s unlimited


#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -c|--corespernode)
    corespernode=$1
    shift
    ;;
    -d|--debug)
    debuglevel="$1"
    shift
    ;;
    -h|--help)
    print_usage_info
    ;;
    -m|--masterfile)
    configfile="$1"
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
    --)
    echo "-- sign detected, remained options are going to be passed to dimr"
    runscript_extraopts="$runscript_extraopts $*"
    break       # exit loop, stop shifting, all remaining arguments without dashes handled below
    ;;
    -*)
    echo "option ${key} seems dedicated for dimr, therefore passing it and the following ones to the dimr"
    runscript_extraopts="$key $*"
    break       # exit loop, $key+all remaining options to dflowfm executable
    ;;
esac
done


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi

# set the number of OpenMP threads equal to max(2,NumberOfPhysicalCores-2)
if [ -z ${OMP_NUM_THREADS+x} ]; then 
   export NumberOfPhysicalCores=`cat /proc/cpuinfo | grep "cpu cores" | uniq | awk -F: '{print $2}'` 
   export OMP_NUM_THREADS=`expr $NumberOfPhysicalCores - 2`
   if [ $OMP_NUM_THREADS -lt 2 ];then
      export OMP_NUM_THREADS=2
   fi
   else echo "OMP_NUM_THREADS is already defined"
fi
echo "OMP_NUM_THREADS" is $OMP_NUM_THREADS

export NSLOTS=`expr $NNODES \* $corespernode` 

workdir=`pwd`

if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    export D3D_HOME=$scriptdir/../..
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

echo "    Configfile       : $configfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    ARCH             : $ARCH"
echo "    Working directory: $workdir"
echo "    Number of slots  : $NSLOTS"
echo 

    #
    # Set the directories containing the binaries
    #

delwaqexedir=$D3D_HOME/$ARCH/dwaq/bin
dflowfmexedir=$D3D_HOME/$ARCH/dflowfm/lib
dimrexedir=$D3D_HOME/$ARCH/dimr/bin
esmfexedir=$D3D_HOME/$ARCH/esmf/bin
esmfbatdir=$D3D_HOME/$ARCH/esmf/scripts
flow1dexedir=$D3D_HOME/$ARCH/dflow1d/bin
flow1d2dexedir=$D3D_HOME/$ARCH/dflow1d2d/bin
rrexedir=$D3D_HOME/$ARCH/drr/bin
rtcexedir=$D3D_HOME/$ARCH/dfbc/bin
swanexedir=$D3D_HOME/$ARCH/swan/bin
swanbatdir=$D3D_HOME/$ARCH/swan/scripts
shareddir=$D3D_HOME/$ARCH/shared
waveexedir=$D3D_HOME/$ARCH/dwaves/bin


    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$shareddir:$dimrexedir:$dflowfmexedir:$flow1dexedir:$flow1d2dexedir:$delwaqexedir:$rtcexedir:$rrexedir:$waveexedir:$swanbatdir:$swanexedir:$esmfbatdir:$esmfexedir
export PATH=$swanbatdir:$esmfbatdir:$PATH
export LD_PRELOAD=$shareddir/libmkl_core.so

# For debugging only (should be related to debuglevel?)
# if [ 1 ]; then
if [  ]; then
    echo === LD_LIBRARY_PATH =========================================
    echo $LD_LIBRARY_PATH
    echo =========================================================
    echo " "
    echo === ldd DFlowFM =========================================
    ldd $dflowfmexedir/libdflowfm.so
    echo =========================================================
    echo " "
    echo ===  DFlowFM -v =========================================
    $dflowfmexedir/../bin/dflowfm -v
    echo =========================================================
    echo " "
    echo ===  ldd Dimr =========================================
    ldd $dimrexedir/dimr.exe
    echo ========================================================
    echo " "
    echo ===  ldd libDimr =======================================
    ldd $dimrexedir/libdimr.so
    echo =========================================================
fi


if [ $NSLOTS -eq 1 ]; then
    echo "executing:"
    echo "$dimrexedir/dimr.exe $configfile -d $debuglevel"
    echo 
    $dimrexedir/dimr.exe $configfile -d $debuglevel
else
    #
    # Create machinefile using $PE_HOSTFILE
    if [ -n $corespernode ]; then
        if [ -e $(pwd)/machinefile ]; then
            rm -f machinefile
        fi
        for (( i = 1 ; i <= $corespernode; i++ )); do
            awk '{print $1":"1}' $PE_HOSTFILE >> $(pwd)/machinefile
        done
    else
       awk '{print $1":"2}' $PE_HOSTFILE > $(pwd)/machinefile
    fi
    echo Contents of machinefile:
    cat $(pwd)/machinefile
    echo ----------------------------------------------------------------------

    #if [ $NNODES -eq 1 ]; then
    #    echo "Starting mpd..."
    #    mpd &
    #fi

    node_number=$NSLOTS
    while [ $node_number -ge 1 ]; do
       node_number=`expr $node_number - 1`
       ln -s /dev/null log$node_number.irlog
    done

    echo "/opt/mpich2/1.4.1_intel14.0.3/bin/mpiexec -np $NSLOTS $dimrexedir/dimr.exe $configfile -d $debuglevel"
          /opt/mpich2/1.4.1_intel14.0.3/bin/mpiexec -np $NSLOTS $dimrexedir/dimr.exe $configfile -d $debuglevel


    rm -f log*.irlog
fi



    # Wait until all child processes are finished
wait

