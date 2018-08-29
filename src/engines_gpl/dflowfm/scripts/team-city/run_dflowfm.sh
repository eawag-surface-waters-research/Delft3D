#/bin/bash
	#
	# This script is an example for running DflowFM on Linux 
	# Adapt and use it for your own purposes
	#
	# michal.kleczek@deltares.nl 
	# 28 June 2015 
	#

function usage () {
    echo "Usage:"
    echo "run_dflowfm.sh [--help] [--mpi <ndomains>] YOUR_MDU_FILE.mdu"
    echo "    --help    	: [Optional] show this usage"
    echo "    --mpi <ndom> 	: [Optional] parallel simulation with <ndom> domains"
    echo "    --<sth>		: [Optional] all following arguments are passed to the D-Flow FM."
    exit
}

#Assuming that default is sequential simulation
NDOM=1

## NOT NECESSARY WHEN USING MPICH version >= 1.1.4
##Assuming that default is one machine
##NHOST=1
##mpdboot -n $NHOST    
###################################################

while [[ $# -ge 1 ]]
do
key="$1"
shift
case $key in
    -h|--help)
    usage
    ;;
    --mpi)
	NDOM="$1"
	shift
    ;;
    --)
    dfmoptions=$*
    break	# exit loop, all remaining options to dflowfm executable
    ;;
    *)
    dfmoptions="$key $*"
    break	# exit loop, $key+all remaining options to dflowfm executable
    ;;
esac
done

# Determine the location of the script:
SCRIPT_DIR=$( cd ${0%/*} && pwd -P )

# Static location of the dflowfm executable in reference to this script location
dflowexec=`/usr/bin/readlink -m $SCRIPT_DIR/../bin`
dflowlib=`/usr/bin/readlink -m $SCRIPT_DIR/../lib`

# Setting up PATH and LD_LIBRARY_PATH environmental variables
export PATH=$SCRIPT_DIR:$dflowexec:$PATH
export LD_LIBRARY_PATH=$SCRIPT_DIR:$dflowexec:$dflowlib:$LD_LIBRARY_PATH

echo "NUMBER OF DOMAINS: $NDOM"

if [ "$NDOM" -gt 1 ]; then
	echo "PARALLEL SIMULATION"
	mpiexec -np $NDOM $dflowexec/dflowfm --nodisplay --autostartstop $dfmoptions
	exit
else
	echo "SEQUENTIAL SIMULATION"
	$dflowexec/dflowfm --nodisplay --autostartstop $dfmoptions
	exit
fi

