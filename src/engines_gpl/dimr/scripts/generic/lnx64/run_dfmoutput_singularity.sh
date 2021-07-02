#!/bin/sh

function print_usage_info {
    echo "Usage: ${0##*/} [run_dfmoutput.sh OPTIONS]..."
    echo "       ${0##*/} [-p | --parentlevel] 3 [run_dfmoutput.sh OPTIONS]..."
    echo "Runs the run_dfmoutput.sh command of a Singularity container by wrapping and passing additional arguments."
    echo
    echo "Options:"
    echo "-h, --help"
    echo "       Print this help message and exit"
    echo "-p, --parentlevel"
    echo "       A numeric value which specifies the amount of levels to navigate to the parent directory to mount"
    echo "       (Default value: 1)"
    exit 1
}

# Variables
parent_level=1
executable=run_dfmoutput.sh
executable_extraopts=
container_libdir=/opt/delft3dfm_latest/lnx64/bin/ # The directory WITHIN the container that contains all the executables

# Main
if [[ $# -eq 0 ]]; then
    print_usage_info
fi

# Parse the first argument of the script
while [[ $# -ge 1 ]]
do
    key="$1"
    shift
    case $key in
         -h|--help)
        print_usage_info
        ;;
        -p|--parentlevel)
        parent_level=$1
        shift
        ;;
        *)
        executable_extraopts="$key $*" # Parse the remaining arguments and pass it as additional arguments to the executable as extra options
        break
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
# Note that the working directory is set to a custom mounting directory
# for the container runtime environment. This mounting is to prevent 
# clashes with the internal opt directory of the container
current_working_dir=$(pwd)
mountdir=/mnt/data

working_dir=
container_working_dir=

if [[ $parent_level -ge 1 ]]; then 
    let target_workingdir_level=$parent_level+1
    working_dir=$(echo $current_working_dir | rev | cut -d'/' -f$target_workingdir_level- | rev) # Returns the desired mounting parent directory 
    container_working_dir=$mountdir/$(echo $current_working_dir | rev | cut -d'/' -f-$parent_level | rev) # Extract the directories that are traversed
elif [[ $parent_level -eq 0 ]]; then
    # Parent directory is equal to the current directory and as such there is no need to traverse the directory structure
    # while mounting
    working_dir=$current_working_dir
    container_working_dir=$mountdir
else 
    echo "Invalid parent level setting: value must be greater or equal to 0"
    exit 1
fi

echo "Executing Singularity container with:"
echo "Container file                    :   $container_file_path"
echo "Current working directory         :   $current_working_dir"
echo "Mounting source directory         :   $working_dir"
echo "Mounting target directory         :   $mountdir"
echo "Container working directory       :   $container_working_dir"
echo "Executable                        :   $executable"
echo "Extra executable flags            :   $executable_extraopts"
singularity exec --bind $working_dir:$mountdir --pwd $container_working_dir $container_file_path $container_libdir/$executable -- $executable_extraopts