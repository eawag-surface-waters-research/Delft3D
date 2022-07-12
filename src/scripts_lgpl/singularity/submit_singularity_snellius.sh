#! /bin/bash

# Usage: 
#   - Place this script in the same folder as the dimr config xml file
#   - Modify this script where needed (e.g. number of nodes, number of tasks per node, singularity version, model folder)
#   - Execute this script using:
#     sbatch ./submit_singularity_snellius.sh
#
# This is a SNELLIUS specific script

#SBATCH --nodes=1               #-N, -n is total numer of nodes. $SLURM_NTASKS = "--nodes" times "--ntasks-per-node"
#SBATCH --ntasks-per-node=2     #you pay for a minimum of 1/4 of the cores on a node
#SBATCH --job-name=tst          #-J
#SBATCH --time 01:00:00         #-t, reduce the expected time if possible to increase your priority
#SBATCH --chdir=./              #chdir set as /path/to/runfolder is useful when calling this script from a different directory
#SBATCH --partition=thin        #type of node
#SBATCH --exclusive
#SBATCH --contiguous
 
 
echo "---Load modules..."
module purge
module load 2021
module load intel/2021a
module load Delft3DFM/2022.04-intel-2021a 


#---You will need to modify the input below this line---

# The root folder of the model, i.e. the folder that contains ALL of the input files and sub-folders:
modelFolder=${PWD}/../../..
# Or, for large models that generate a lot of output, copying the model to your scratch file space '/scratch-shared/<username>' and running from there might be faster.
# See: https://servicedesk.surf.nl/wiki/display/WIKI/Snellius+hardware+and+file+systems#Snelliushardwareandfilesystems-Filesystems
# Don't forget to copy your results back to a permanent location on Snellius since data on the scratch space is removed automatically!

# The folder containing the dimr config file:
dimrconfigFolder=${PWD}

# The folder containing the mdu file:
mdufileFolder=${PWD}

# The name of the dimr config file. The default is dimr_config.xml:
dimrFile=dimr_config.xml


#---You do not need to modify anything below this line---

# Set the location of the Singularity container.
singularityFolder=${EBROOTDELFT3DFM}/bin

# Use SLURM_NTASKS to update the line "<process>" in the dimrFile.
PROCESSSTR="$(seq -s " " 0 $((SLURM_NTASKS-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESSSTR\2/" $dimrFile
# Retrieve the name of the mduFile from dimrFile.
mduFile="$(sed -n 's/\r//; s/<inputFile>\(.*\).mdu<\/inputFile>/\1/p' $dimrFile)".mdu

echo ""
echo "Partitioning..."
cd $mdufileFolder
$singularityFolder/execute_singularity_snellius.sh $modelFolder run_dflowfm.sh --partition:ndomains=$SLURM_NTASKS:icgsolver=6 $mduFile

echo ""
echo "Simulation..."
cd $dimrconfigFolder
$singularityFolder/execute_singularity_snellius.sh $modelFolder run_dimr.sh -m $dimrFile
