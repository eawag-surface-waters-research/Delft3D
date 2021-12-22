#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

# Usage:
# To submit and execute a job on an SGE cluster, for
# D-Flow FM computations using a Singularity container,
# The script "run_singularity.sh" is much simpler, but can only be used for
# either sequential, or parallel computations using one node.
# For parallel using multiple nodes: use submit_singularity.sh.
#
# To start:
# 1. Be sure that a Singularity container is available, 
#    together with an execute_singularity.sh script in the same folder
# 2. Copy this script into your working folder, i.e. the folder containing the dimr config file
# 3. Modify this script, see remarks below
# 4. Execute this script from the command line
#    This script assumes a SGE queueing system is present and will execute qsub
#
#
# Submit a job in the queue:
#     Execute this script from the command line without arguments.
#     The "STEP 1" part below will be entered, finishing with putting this script,
#     "submit_singularity.sh", with a set of arguments, in the queue
#
# Execute a job from the queue:
#     When the queueing system has allocated resources, this script,
#     "submit_singularity.sh", with a set of arguments, will be executed.
#     The "STEP 2" part below will be entered, finishing with:
#     - executing script "execute_singularity.sh"
#       In case of a single node run. The mpirun/mpiexec inside will refer to
#       the external IntelMPI installation.
#     - executing "mpirun ... execute_singularity.sh"
#       In case of a multi node run. It's essential that "mpirun" is outside Singularity.


# "execute_singularity.sh -p 2": Parent level to mount:
# If your working folder does not contain all the input files, then you have to set this -p flag.
# Let's define the "top level" as the folder containing all the input files.
# The value of -p must be the number of folder levels between the dimr config file and the top level.
# A higher value will not harm, as long as it exists.
# 


#
#
# --- You will need to change the lines below -----------------------------

# Set number of nodes and partitions
nNodes=2
nProc=3

# Set the path to the folder containing the singularity image
singularitydir=/p/d-hydro/delft3dfm_containers/delft3dfm_2022.02_test

# select queue; one of : normal-e3-c7 , normal-e5-c7
queue=test-c7

# DIMR input-file; must already exist!
dimrFile=dimr_config.xml


#
#
# --- You shouldn't need to change the lines below ------------------------

# stop after an error occured:
set -e

nPart=$((nNodes * nProc))

if (( $# < 4 )); then
    #
    # This script is called with 3 arguments or less:
    # STEP 1:
    # Prepare computation and execute "qsub", executing this script, "submit_singularity.sh" (with >=4 arguments), when the nodes are allocated

    # Replace number of processes in DIMR file
    PROCESSSTR="$(seq -s " " 0 $((nPart-1)))"
    sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile

    # Read MDU file from DIMR-file
    mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' $dimrFile)".mdu

    # jobName: dimrsif_$FOLDERNAME_nodesxprocs
    export jobName="dimrsif_${PWD##*/}_${nNodes}x${nProc}"


    if [ "$nPart" == "1" ]; then
        echo Adding a job to the queue for one node, one partition, based on this script "submit_singularity.sh" ...
        qsub -q $queue -N $jobName submit_singularity.sh $nNodes $nProc $singularitydir/execute_singularity.sh run_dimr.sh -m $dimrFile
    else
        cd dflowfm
        echo partitioning...
        $singularitydir/execute_singularity.sh run_dflowfm.sh --partition:ndomains=$nPart:icgsolver=6 $mduFile
        cd ..
        if [ "$nNodes" == "1" ]; then
            echo Adding a job to the queue for one node, multi partitions, based on this script "submit_singularity.sh" ...
            qsub -q $queue -N $jobName submit_singularity.sh $nNodes $nProc $singularitydir/execute_singularity.sh run_dimr.sh -m $dimrFile
        else
            echo Adding a job to the queue for multi nodes, based on this script "submit_singularity.sh" ...
            qsub -q $queue -pe distrib $nNodes -N $jobName submit_singularity.sh $nNodes $nProc $singularitydir/execute_singularity.sh run_dimr.sh -m $dimrFile
        fi
    fi

else
    #
    # This script is called with 4 arguments or more:
    # STEP 2:
    # The queueing system allocated nodes, execute "execute_singularity.sh",
    # possibly with "mpirun" outside (multi node)

    module load intelmpi/21.2.0
    
    pwd=`pwd`
    nNodes=${1}
    nProc=${2}
    exec_sif=${3}
    run_dimr=${4}
    
    scriptdirname=`readlink \-f \$0`
    echo ---------------------------------------------------------------------- 
    echo $scriptdirname
    echo submit_singularity.sh executing "execute_singularity.sh"
    echo pwd          : $pwd
    echo nNodes       : $nNodes
    echo nProc        : $nProc
    echo exec_sif     : $exec_sif
    echo run_dimr     : $run_dimr
    echo arg5         : ${5}
    echo arg6         : ${6}
    echo arg7         : ${7}
    echo arg8         : ${8}
    echo
    #
    # Create machinefile using $PE_HOSTFILE
    if [ "$nNodes" == "1" ]; then
        echo " ">$(pwd)/machinefile
    else
        if [ -n $nProc ]; then
            if [ -e $(pwd)/machinefile ]; then
                rm -f machinefile
            fi
            for (( i = 1 ; i <= `expr $nProc`; i++ )); do
                awk '{print $1":"1}' $PE_HOSTFILE >> $(pwd)/machinefile
            done
        else
           awk '{print $1":"2}' $PE_HOSTFILE > $(pwd)/machinefile
        fi
    fi
    echo Contents of machinefile:
    cat $(pwd)/machinefile
    echo ---------------------------------------------------------------------- 

    if [ "$nNodes" == "1" ]; then
        echo One node execution: execute_singularity.sh run_dimr.sh -c nProc
        $exec_sif $run_dimr -c $nProc ${5} ${6} ${7} ${8} ${9} ${10} ${11} ${12} 
    else
        echo Multi node execution: mpirun -n nPart execute_singularity.sh run_dimr.sh -c 1
        mpirun -n $nPart -ppn $nProc -f machinefile $exec_sif $run_dimr ${5} ${6} ${7} ${8} ${9} ${10} ${11} ${12} 
    fi
fi
