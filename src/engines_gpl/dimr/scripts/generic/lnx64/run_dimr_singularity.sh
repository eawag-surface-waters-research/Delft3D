#!/bin/sh

function print_usage_info {
    echo "Usage: ${0##*/} dimr_config_file [RUN_DIMR.SH OPTIONS]..."
    echo "       ${0##*/} [-h | --help]"
    echo "Runs the run_dimr.sh command of a Singularity container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    echo
    echo "Note: this script will mount the parent of the current directory to the Singularity container."
    exit 1
}

# Variables
executable=run_dimr.sh
executable_extraopts=
dimr_config_file=
container_libdir=/opt/delft3dfm_latest/lnx64/bin/ # The directory WITHIN the container that contains all the executables

# Main
if [[ $# -eq 0 ]]; then
    print_usage_info
fi

# Parse the first argument of the script
if [[ $# -ge 1 ]]; then
    arg=$1
    case $arg in
        -h|--help)
        print_usage_info
        ;;
        *)
        dimr_config_file=$arg
        ;;
    esac
    shift

    # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
    executable_extraopts="$*"
fi

# Check if the dimr_config_file exists
if [ ! -z "$dimr_config_file" ] && [ ! -f "$dimr_config_file" ]; then
    echo "ERROR: Dimr config file must be set and existent."
    exit 2 # Exit code 2 to indicate that no such file is present
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
workingdir=$(pwd)/..
workingdir_name=$(basename $(pwd))
mountdir=/mnt/data
echo "Executing Singularity container with:"
echo "Singularity container directory   :   $scriptdir"
echo "Singularity container name        :   $container_file_path"
echo "Executable                        :   $executable"
echo "Working directory                 :   $workingdir"
echo "Working directory name            :   $workingdir_name"
echo "Mounting directory                :   $mountdir"
echo "Pwd of container                  :   $mountdir/$workingdir_name"
echo "Extra executable flags            :   $executable_extraopts"
singularity exec --bind $workingdir:$mountdir --pwd $mountdir/$workingdir_name $container_file_path $container_libdir/$executable -m $dimr_config_file $executable_extraopts