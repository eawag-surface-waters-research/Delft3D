#!/bin/bash


# ================
# === Usage    ===
# ================
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
    echo "- If <CONFIG> is missing, this usage will be print"
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



# =========================
# === CreateCMakedir    ===
# =========================
function CreateCMakedir () {
    if [ "$config" = "$1" ] || [ "$config" = "all"  ]; then
        echo
        echo "Create CMake dir for $1 ..."
        cd     $root
        rm -rf $root/build_$1
        mkdir  $root/build_$1
    fi
    
    return
}



# ==================
# === DoCMake    ===
# ==================
function DoCMake () {
    if [ "$config" = "$1" ] || [ "$config" = "all"  ]; then
        echo
        echo "Executing CMake for $1 ..."
        cd    $root/build_$1
        echo "cmake ../src/cmake -G "$generator" -B "." -D CONFIGURATION_TYPE="$1" -D CMAKE_BUILD_TYPE=Release &>build_$1/cmake_$1.log"
              cmake ../src/cmake -G "$generator" -B "." -D CONFIGURATION_TYPE="$1" -D CMAKE_BUILD_TYPE=Release &>cmake_$1
    fi

    return
}



# =====================
# === BuildCMake    ===
# =====================
function BuildCMake () {
    if [ "$config" = "$1" ] || [ "$config" = "all"  ]; then
        echo
        echo "Building (make) based on CMake preparations for $1 ..."
        cd    $root/build_$1
        echo "make VERBOSE=1 install &>build_$1/make_$1.log"
              make VERBOSE=1 install &>make_$1.log
    fi

    return
}


# ============
# === MAIN ===
# ============

#
## Defaults
prepareonly=0
mode=quiet
config=
generator="Unix Makefiles"
compiler=intel18


#
## Start processing command line options:

while [[ $# -ge 1 ]]
do
key="$1"

echo key:$key
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
# Check config parameter    
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
echo ". $root/src/setenv.sh"
      . $root/src/setenv.sh

CreateCMakedir dimr
CreateCMakedir dflowfm

DoCMake dimr
DoCMake dflowfm

if [ "$prepareonly" = "1" ]; then
    echo Finished with preparations only
    exit 0
fi

BuildCMake dimr
BuildCMake dflowfm

echo "Building the traditional way ..."
echo "First some svn cleaning is needed in third_party_open..."
cd $root/src/third_party_open
echo "module purge"
      module purge
echo "svn revert . -R"
      svn revert . -R
echo "svn status --no-ignore | grep '^[I?]' | cut -c 9- | while IFS= read -r f; do rm -rf "$f"; done"
      svn status --no-ignore | grep '^[I?]' | cut -c 9- | while IFS= read -r f; do rm -rf "$f"; done
echo "Build in src..."
cd $root/src
echo "./build_h6c7.sh -$compiler"
      ./build_h6c7.sh -$compiler

echo Finished
