#!/bin/sh

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]..."
    echo "Runs the run_dimr.sh command of a Singularity container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    exit 1
}

# Variables
executable=run_dimr.sh
executable_extraopts=
container_libdir=/opt/delft3dfm_latest/lnx64/bin/ # The directory WITHIN the container that contains all the executables

# Main
while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -h|--help)
    print_usage_info
    ;;
    --)
    echo "-- sign detected, option passed to $executable"
    executable_extraopts="$executable_extraopts $*"
    break       # exit loop, stop shifting, all remaining arguments without dashes handled below
    ;;
    -*)
    echo "option ${key} passed to $executable"
    executable_extraopts="$key $*"
    break       # exit loop, $key+all remaining options to the executable
    ;;
esac
done

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
echo "Executing Singularity container with:"
echo "Singularity container directory   :   $scriptdir"
echo "Singularity container name        :   $container_file_path"
echo "Extra executable flags            :   $executable_extraopts"
singularity exec $container_file_path $container_libdir/$executable $executable_extraopts