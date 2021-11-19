#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd
    #
    # This script runs dimr on Linux
    # Adapt and use it for your own purpose
    #
    # Usage example:
    # Execute in the working directory:
    # /path/to/delft3d/installation/lnx64/bin/run_dimr.sh
    # More examples: check run scripts in https://svn.oss.deltares.nl/repos/delft3d/trunk/examples/*

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Run a dimr model on Linux."
    echo
    echo "Options:"
    echo "-c, --corespernode <M>"
    echo "       number of cores per node, default $corespernodedefault"
    echo "-d, --debug <D>"
    echo "       0:ALL, 6:SILENT"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-m, --masterfile <filename>"
    echo "       dimr configuration filename, default dimr_config.xml"
    echo "--dockerparallel"
    echo "       A parallel run inside docker"
    echo "The following arguments are used when called by submit_dimr.sh:"
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
corespernode=$corespernodedefault
debuglevel=-1
configfile=dimr_config.xml
dockerprl=0
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
    --dockerparallel)
    dockerprl=1
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

# Check configfile    
if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi

# Check debuglevel, translate into argument for dimr
if [ $debuglevel -eq -1 ]; then
    debugarg=
else
    debugarg="-d $debuglevel"
fi

# set the number of OpenMP threads equal to max(2,NumberOfPhysicalCores-2)
if [ -z ${OMP_NUM_THREADS+x} ]; then 
    # If OMP_NUM_THREADS is not already defined:
    # Since OMP_NUM_THREADS is advised to be 1, don't do any smart setting, just set it to 1
    # export NumberOfPhysicalCores=`cat /proc/cpuinfo | grep "cpu cores" | uniq | awk -F: '{print $2}'` 
    # export OMP_NUM_THREADS=`expr $NumberOfPhysicalCores - 2`
    # if [ $OMP_NUM_THREADS -lt 2 ]; then
    #     export OMP_NUM_THREADS=2
    # fi
    export OMP_NUM_THREADS=1
else echo "OMP_NUM_THREADS is already defined"
fi

export NSLOTS=`expr $NNODES \* $corespernode` 

workdir=`pwd`

if [ -z "${D3D_HOME}" ]; then
    scriptdirname=`readlink \-f \$0`
    scriptdir=`dirname $scriptdirname`
    export D3D_HOME=$scriptdir/..
else
    # D3D_HOME is passed through via argument --D3D_HOME
    # Commonly its value is "/some/path/bin/.."
    # To obtain scriptdir: remove "/.." at the end of the string
    scriptdir=${D3D_HOME%"/.."}
fi
if [ ! -d $D3D_HOME ]; then
    echo "ERROR: directory $D3D_HOME does not exist"
    print_usage_info
fi
export D3D_HOME
PROC_DEF_DIR=$D3D_HOME/share/delft3d
export PROC_DEF_DIR

# Always try the following module load
module load intelmpi/21.2.0 &>/dev/null

export FI_PROVIDER=tcp
 
echo "    Configfile       : $configfile"
echo "    D3D_HOME         : $D3D_HOME"
echo "    PROC_DEF_DIR     : $PROC_DEF_DIR"
echo "    Working directory: $workdir"
echo "    Number of nodes  : $NNODES"
echo "    Number of slots  : $NSLOTS"
echo "    OMP_NUM_THREADS  : $OMP_NUM_THREADS"
echo "    Docker parallel  : $dockerprl"
echo "    `type mpiexec`"
echo "    FI_PROVIDER      : $FI_PROVIDER"
echo "    I_MPI_FABRICS    : $I_MPI_FABRICS"
echo

    #
    # Set the directories containing the binaries
    #

bindir=$D3D_HOME/bin
libdir=$D3D_HOME/lib

    #
    # No adaptions needed below
    #

    # Run
export LD_LIBRARY_PATH=$libdir:$LD_LIBRARY_PATH
export PATH=$bindir:$PATH

# For debugging only
if [ $debuglevel -eq 0 ]; then
    echo === LD_LIBRARY_PATH =========================================
       echo $LD_LIBRARY_PATH
    echo =========================================================
    echo " "
    echo === ldd $libdir/libdflowfm.so =========================================
             ldd $libdir/libdflowfm.so
    echo =========================================================
    echo " "
    echo ===  $bindir/dflowfm -v =========================================
              $bindir/dflowfm -v
    echo =========================================================
    echo " "
    echo ===  ldd $bindir/dimr =========================================
              ldd $bindir/dimr
    echo ========================================================
    echo " "
    echo ===  ldd $libdir/libdimr.so =======================================
              ldd $libdir/libdimr.so
    echo =========================================================
fi


if [ $NSLOTS -eq 1 ]; then
    echo "executing:"
    echo "$bindir/dimr $configfile $debugarg"
          $bindir/dimr $configfile $debugarg
else
    if [ $dockerprl -eq 1 ]; then
        #
        # Parallel in Docker
        # Assumption: 1 node

        node_number=$NSLOTS
        while [ $node_number -ge 1 ]; do
           node_number=`expr $node_number - 1`
           ln -s /dev/null log$node_number.irlog
        done

        echo "executing:"
        echo "mpirun -np $NSLOTS $bindir/dimr $configfile $debugarg"
              mpirun -np $NSLOTS $bindir/dimr $configfile $debugarg
    else
        #
        # Create machinefile using $PE_HOSTFILE
        if [ $NNODES -eq 1 ]; then
            echo " ">$(pwd)/machinefile
        else
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
        fi
        echo Contents of machinefile:
        cat $(pwd)/machinefile
        echo ----------------------------------------------------------------------


        echo "executing:"
        echo "mpiexec -np $NSLOTS $bindir/dimr $configfile $debugarg"
              mpiexec -np $NSLOTS $bindir/dimr $configfile $debugarg
    fi
fi


    # Wait until all child processes are finished
wait

    # Nefis files don't get write permission for the group bit
    # Add it explicitly, only when stderr = 0
if [ $? -eq 0 ]; then
    chmod -R g+rw *.dat *.def &>/dev/null || true
fi
