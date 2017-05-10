#!/bin/bash

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Submits a dimr model to the queue (parallel or sequential)."
    echo
    echo "Options:"
    echo "-c, --corespernode <M>"
    echo "       number of cores per node, default $corespernodedefault"
    echo "-d, --debug 0xFFFFFFFF"
    echo "       maximum debug output, default 0"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo "-j, --jobname <jobname>"
    echo "       jobname prefix, default dimr"
    # echo "-l, --localdir"
    # echo "       run the model from each node's local disk, by first copying"
    # echo "       the current model working dir to a temporary directory."
    echo "-m, --masterfile <filename>"
    echo "       dimr configuration filename, default dimr_config.xml"
    echo "-n, --numnode <N>"
    echo "       number of nodes, default 1"
    echo "-q, --queue <qname>"
    echo "       queue, default normal-e3"
    echo "-s, --sequential"
    echo "       sequential (non-MPI) run, equivalent to -n 1 -c 1"
    exit 1
}


# ============
# === MAIN ===
# ============


#
## Defaults
debuglevel=0
configfile=dimr_config.xml
queue=normal-e3
JOBNAME=dimr
D3D_HOME=
runscript_extraopts=
corespernodedefault=1
corespernode=$corespernodedefault
numnode=1
do_mpi=0
runscript_extraopts=

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
    -n|--numnode)
    numnode="$1"
    shift
    ;;
    -q|--queue)
    queue="$1"
    shift
    ;;
    -s|--sequential)
    do_mpi=0
    numnode=1
    corespernode=1
    ;;
    -j|--jobname)
    JOBNAME="$1"
    shift
    ;;
    -m|--masterfile)
    configfile="$1"
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


if [ ${numnode} -ge 2 ] || [ ${corespernode} -ge 2 ]; then
    do_mpi=1
fi


export OMP_NUM_THREADS=1


if [ ! -f $configfile ]; then
    echo "ERROR: configfile $configfile does not exist"
    print_usage_info
fi


## MPI processes:
if [ -z "${numnode}" ]; then
		echo "Error: number of nodes missing on commandline."
		print_usage_info
fi


JOBNAME="${JOBNAME}_${numnode}x${corespernode}"


scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
D3D_HOME=$scriptdir/../..
RUNSCRIPT=$scriptdir/run_dimr.sh

runscript_opts="-m ${configfile} -d ${debuglevel} -c $corespernode --NNODES $numnode --D3D_HOME ${D3D_HOME}"
runscript_opts="$runscript_opts $runscript_extraopts"
if [[ $do_mpi -eq 0 ]]; then
    echo "qsub -q $queue -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}"
    qsub -q $queue -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}
else
    echo "qsub -q $queue -pe distrib ${numnode} -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}"
    qsub -q $queue -pe distrib ${numnode} -N ${JOBNAME} ${RUNSCRIPT} ${runscript_opts}
fi



