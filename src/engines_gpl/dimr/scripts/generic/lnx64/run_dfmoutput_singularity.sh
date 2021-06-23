#!/bin/sh

function print_usage_info {
    echo "Usage: ${0##*/} dimr_config_file [RUN_DIMR.SH OPTIONS]..."
    echo "       ${0##*/} [-h | --help]"
    echo "Runs the run_dimr.sh command of a Singularity container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    exit 1
}

# Variables
executable=run_dfmoutput.sh
executable_extraopts=
container_libdir=/opt/delft3dfm_latest/lnx64/bin/ # The directory WITHIN the container that contains all the executables

# Main
if [[ $# -eq 0 ]]; then
    print_usage_info
fi

if [[ $# -ge 1 ]]; then
    # Check the first argument
    arg=$1
    if [[ $arg = -h ]] || [[ $arg = --help ]]; then 
        print_usage_info
    fi
    
    # Pass the arguments as additional arguments to the executable
    executable_extraopts="$*"
fi

# Retrieve the script directory
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`

# Scan script directory for sif containers
shopt -s nullglob
container_file_paths=($(ls $scriptdir/*.sif))
container_file_path=

if [ ${#container_file_paths[@]} -eq 1 ]; then
    container_file_path=${container_file_paths[0]}
else
    echo "ERROR: Directory must contain one and only one *.sif file."
    exit 2 # Exit code 2 to indicate that no such file is present
fi
shopt -u nullglob

# Set container properties
# Note that the working directory is set to a custom mounting directory
# for the container runtime environment. This mounting is to prevent 
# clashes with the internal opt directory of the container
workingdir=$(pwd)
mountdir=/mnt/data
echo "Executing Singularity container with:"
echo "Singularity container directory   :   $container_dir"
echo "Singularity container name        :   $container_file_path"
echo "Executable                        :   $executable"
echo "Working directory                 :   $workingdir"
echo "Mounting directory                :   $mountdir"
echo "Extra executable flags            :   $executable_extraopts"
singularity exec --bind $workingdir:$mountdir --pwd $mountdir $container_file_path $container_libdir/$executable -- $executable_extraopts