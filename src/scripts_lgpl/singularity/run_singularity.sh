#!/bin/bash

# To start Dimr, execute this script

# stop after an error occured:
set -e
 
# Set numbers of hosts and cores per host
nNodes=1
nProc=3

# set DIMR version to be used
singularitydir=/p/d-hydro/delft3dfm_containers/delft3dfm_2022.02_test

# select queue; one of : normal-e3-c7 , normal-e5-c7
queue=test-c7

nPart=$((nNodes * nProc))

# DIMR input-file; must already exist!
dimrFile=dimr_config.xml

# Replace number of processes in DIMR file
PROCESSSTR="$(seq -s " " 0 $((nPart-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile

# Read MDU file from DIMR-file
mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' $dimrFile)".mdu

# jobName: $FOLDERNAME
export jobName="${PWD##*/}"


if [ "$nPart" == "1" ]; then
    $singularitydir/execute_singularity.sh run_dimr.sh $dimrFile
else
    cd dflowfm
    echo partitioning...
    $singularitydir/execute_singularity.sh run_dflowfm.sh --partition:ndomains=$nPart:icgsolver=6 $mduFile
    cd ..
    echo computation...
    $singularitydir/execute_singularity.sh run_dimr.sh -m $dimrFile -c $nProc
    #$singularitydir/execute_singularity.sh run_dfmoutput.sh --help
fi
