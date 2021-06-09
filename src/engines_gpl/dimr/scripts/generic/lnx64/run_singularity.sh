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

# Main
executable=run_dimr.sh
executable_extraopts=

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

# Set container properties
container_name=delft3dfm-cli-hmwq_lnx64.sif
container_libdir=/opt/delft3dfm_latest/lnx64/bin/ # The directory WITHIN the container that contains all the executables

echo "Executing Singularity container with:"
echo "Singularity container directory   :   $scriptdir"
echo "Singularity container name        :   $container_name"
echo "Extra executable flags            :   $executable_extraopts"
singularity exec $scriptdir/$container_name $container_libdir/$executable $executable_extraopts