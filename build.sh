#!/bin/bash


function print_usage_info {
    echo
    echo
    echo "Usage: ${0##*/} <CONFIG> [OPTIONS]"
    echo "- Create directory 'build_<CONFIG>'"
    echo "  Delete it when it already existed"
    echo "- Execute 'src/setenv.sh' to load modules"
    echo "- Execute 'CMake <CONFIG>' to create makefile inside 'build_<CONFIG>'"
    echo "- Execute 'make install'"
    echo
    echo "<CONFIG>:"
    echo "- missing: print_usage_info"
    echo "- 'all':"
    echo "  - build dflowfm"
    echo "  - build dimr"
    echo "  - src/build_h6c7.sh -intel18"
    echo "  - combine all binaries"
    echo "- dflowfm"
    echo "- dimr"
    echo
    echo "Options:"
    echo "-p, --prepareonly"
    echo "       Only CMake, no make, no src/build_h6c7.sh"
    echo
    echo
    exit 1
}


# ============
# === MAIN ===
# ============

#
## Defaults
prepareonly=0
config=


#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
key="$1"
shift

case $key in
    -p|--prepareonly)
    prepareonly=1
    shift
    ;;
    -h|--help)
    print_usage_info
    ;;
    all)
    config="all"
    shift
    ;;
    dflowfm)
    config="dflowfm"
    shift
    ;;
    dimr)
    config="dimr"
    shift
    ;;
esac
done

#
# Check configfile    
if [ -z $config ]; then
    print_usage_info
fi

echo
echo "    config      : $config"
echo "    prepareonly : $prepareonly"
echo

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
root=$scriptdir
 
#
# Dot setenv.sh to load the modules needed
. $root/src/setenv.sh

#
# D-Flow FM:
if [ "$config" = "dflowfm" ] || [ "$config" = "all"  ]; then
    cd     $root
    rm -rf $root/build_dflowfm
    mkdir  $root/build_dflowfm
    cd     $root/build_dflowfm

    #
    # CMake
    echo "cmake ../src/cmake -G "Unix Makefiles" -B "." -D CONFIGURATION_TYPE="dflowfm" -D CMAKE_BUILD_TYPE=Release &>build_dflowfm/cmake.log"
          cmake ../src/cmake -G "Unix Makefiles" -B "." -D CONFIGURATION_TYPE="dflowfm" -D CMAKE_BUILD_TYPE=Release &>cmake.log

    # Make
    echo "make VERBOSE=1 install &>build_dflowfm/make.log"
          make VERBOSE=1 install &>make.log
          
    cd $root
 fi 
 



#
# DIMR:
if [ "$config" = "dimr" ] || [ "$config" = "all"  ]; then
    cd     $root
    rm -rf $root/build_dimr
    mkdir  $root/build_dimr
    cd     $root/build_dimr

    #
    # CMake
    echo "cmake ../src/cmake -G "Unix Makefiles" -B "." -D CONFIGURATION_TYPE="dimr" -D CMAKE_BUILD_TYPE=Release &>build_dimr/cmake.log"
          cmake ../src/cmake -G "Unix Makefiles" -B "." -D CONFIGURATION_TYPE="dimr" -D CMAKE_BUILD_TYPE=Release &>cmake.log

    # Make
    echo "make VERBOSE=1 install &>build_dimr;/make.log"
          make VERBOSE=1 install &>make.log
          
    cd $root
 fi 



