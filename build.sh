#!/bin/bash


# ================
# === Usage    ===
# ================
function print_usage_info {
    echo
    echo
    echo "Usage: ${0##*/} <CONFIG> [OPTIONS]"
    echo "- Only when <CONFIG>=all: Compile all engines that are not CMaked yet in the traditional way"
    echo "- Create directory 'build_<CONFIG>'"
    echo "  Delete it when it already existed"
    echo "- Execute '. src/setenv.sh' to load modules"
    echo "- Execute 'CMake <CONFIG>' to create makefile inside 'build_<CONFIG>'"
    echo "- Execute 'make VERBOSE=1 install'"
    echo "- Only when <CONFIG>=all: Combine all binaries in 'build_<CONFIG>\lnx64'"
    echo
    echo "<CONFIG>:"
    echo "- If <CONFIG> is missing, this usage will be print"
    echo "- all: All CMaked projects, currently D-Flow FM and DIMR"
    echo "- dflowfm"
    echo "- dimr"
    echo "- tests"
    echo
    echo "Options:"
    echo "-p, --prepareonly"
    echo "       Only CMake, no make, no src/build_h6c7.sh"
    echo
    echo "More info  : https://oss.deltares.nl/web/delft3d/source-code"
    echo "About CMake: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/cmake/doc/README"
    echo
    exit 1
}



# =========================
# === CreateCMakedir    ===
# =========================
function CreateCMakedir () {
    echo
    echo "Create CMake dir for $1 ..."
    cd     $root
    rm -rf $root/build_$1
    mkdir  $root/build_$1
    
    return
}



# ==================
# === DoCMake    ===
# ==================
function DoCMake () {
    echo
    echo "Executing CMake for $1 ..."
    cd    $root/build_$1
    echo "cmake ../src/cmake -G "$generator" -B "." -D CONFIGURATION_TYPE="$1" -D CMAKE_BUILD_TYPE=Release &>build_$1/cmake_$1.log"
          cmake ../src/cmake -G "$generator" -B "." -D CONFIGURATION_TYPE="$1" -D CMAKE_BUILD_TYPE=Release &>cmake_$1.log

    return
}



# =====================
# === BuildCMake    ===
# =====================
function BuildCMake () {
    echo
    echo "Building (make) based on CMake preparations for $1 ..."
    cd    $root/build_$1
    echo "make VERBOSE=1 install &>build_$1/make_$1.log"
          make VERBOSE=1 install &>make_$1.log

    return
}



# =========================
# === InstallAll        ===
# =========================
function InstallAll () {
    if [ "$config" = "all"  ]; then
        echo
        echo "Installing in build_all ..."
        cd     $root
        rm -rf $root/build_all/lnx64
        mkdir -p $root/build_all/lnx64
        # Start with artifacts from traditional build
        cp -rf $root/src/bin/ $root/build_all/lnx64/ &>/dev/null
        cp -rf $root/src/lib/ $root/build_all/lnx64/ &>/dev/null
        cp -rf $root/src/share/ $root/build_all/lnx64/ &>/dev/null
        # Delete DIMR/D-Flow FM related files: they will be added from the CMake build tasks
        rm -f $root/build_all/lnx64/bin/dflowfm        &>/dev/null
        rm -f $root/build_all/lnx64/bin/dimr           &>/dev/null
        rm -f $root/build_all/lnx64/lib/libdflowfm.so* &>/dev/null
        rm -f $root/build_all/lnx64/lib/libdimr.so*    &>/dev/null

        # CMaked stuff
        cp -rf $root/build_all/install/* $root/build_all/lnx64/ &>/dev/null
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
    tests)
    config="tests"
    shift
    ;;
    *)
    echo ERROR: Unknown command line argument $key
    exit 1
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




if [ "$prepareonly" = "0" ] && [ "$config" = "all"  ]; then
    cd $root/src
    echo "Building the traditional way ..."
    echo "./build_h6c7.sh -$compiler"
          ./build_h6c7.sh -$compiler
    cd $root
fi



 
#
# Dot setenv.sh to load the modules needed
echo ". $root/src/setenv.sh"
      . $root/src/setenv.sh

CreateCMakedir $config

DoCMake $config

if [ "$prepareonly" = "1" ]; then
    echo Finished with preparations only
    exit 0
fi

BuildCMake $config

InstallAll

echo Finished

