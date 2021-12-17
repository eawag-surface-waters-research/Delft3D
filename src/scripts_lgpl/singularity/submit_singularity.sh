#!/bin/bash
#$ -V
#$ -j yes
#$ -cwd

# To submit Singularity on an SGE cluster, execute this script

# stop after an error occured:
set -e
 
# Set numbers of hosts and cores per host
nNodes=1
nProc=3

# set path to the directory containing the sif-file to be used
singularityDir=/p/d-hydro/delft3dfm_containers/delft3dfm_2022.02_test

# select queue; one of : normal-e3-c7 , normal-e5-c7
# queue=normal-e3-c7
queue=test-c7

# DIMR input-file; must already exist!
dimrFile=dimr_config.xml



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

    # jobName: $FOLDERNAME_nodesxprocs
    export jobName="dimrsif_${PWD##*/}_${nNodes}x${nProc}"


    if [ "$nPart" == "1" ]; then
        echo Adding a job to the queue for one node, one partition, based on this script "submit_singularity.sh" ...
        qsub -q $queue -N $jobName submit_singularity.sh $nNodes $nProc $singularityDir/execute_singularity.sh run_dimr.sh -m $dimrFile
    else
        cd dflowfm
        echo partitioning...
        $singularityDir/execute_singularity.sh run_dflowfm.sh --partition:ndomains=$nPart:icgsolver=6 $mduFile
        cd ..
        if [ "$nNodes" == "1" ]; then
            echo Adding a job to the queue for one node, multi partitions, based on this script "submit_singularity.sh" ...
            qsub -q $queue -N $jobName submit_singularity.sh $nNodes $nProc $singularityDir/execute_singularity.sh run_dimr.sh -m $dimrFile
        else
            echo Adding a job to the queue for multi nodes, based on this script "submit_singularity.sh" ...
            qsub -q $queue -pe distrib $nNodes -N $jobName submit_singularity.sh $nNodes $nProc $singularityDir/execute_singularity.sh run_dimr.sh -m $dimrFile
        fi
        # $singularityDir/execute_singularity.sh run_dfmoutput.sh --help
    fi

else
    #
    # This script is called with 4 arguments or more:
    # STEP 2:
    # The queueing system allocated nodes, execute "mpirun"

    module load intelmpi/21.2.0
    
    pwd=`pwd`
    nNodes=${1}
    nProc=${2}
    exec_sif=${3}
    run_dimr=${4}
    min_m=${5}
    dimrfile=${6}
    arg7=${7}
    arg8=${8}
    
    scriptdirname=`readlink \-f \$0`
    echo ---------------------------------------------------------------------- 
    echo $scriptdirname
    echo submit_singularity.sh executing "mpirun"
    echo pwd          : $pwd
    echo nNodes       : $nNodes
    echo nProc        : $nProc
    echo exec_sif     : $exec_sif
    echo run_dimr     : $run_dimr
    echo min_m        : $min_m
    echo dimrfile     : $dimrfile
    echo arg7         : $arg7
    echo arg8         : $arg8
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
        echo One node execution: singularity exec run_dimr.sh -c nProc
        $exec_sif $run_dimr $min_m $dimrfile -c $nProc ${7} ${8} ${9} ${10} ${11} ${12} 
    else
        echo Multi node execution: mpirun -n nPart singularity exec run_dimr.sh -c 1
        mpirun -n $nPart -ppn $nProc -f machinefile ${3} ${4} ${5} ${6} ${7} ${8} ${9} ${10} ${11} ${12} 
    fi
fi
