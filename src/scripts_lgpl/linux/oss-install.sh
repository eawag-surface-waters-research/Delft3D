#!/bin/bash

globalErrorLevel=0


# This script uses the command ldd to collect all dynamic libraries used:
gatherScript=scripts_lgpl/linux/gatherlibraries.rb
# The following libraries must be removed from the list created by gatherScript:
# - system dependent libraries in the directories /lib and /lib64
# - libraries generated in the oss tree itself
gatherExcludeFilter="-e '^/lib/' -e '^/lib64/' -e 'flow2d3d' -e 'DelftOnline'"
gatherIncludeFilter="-e 'expat' -e 'libssl' -e 'libcrypto' -e 'libpng' -e 'libjpeg' "



# =======================
# === INSTALL_ESMF ======
# =======================
function gatherESMF () {
    cp $srcroot/third_party_open/esmf/lnx64/bin/libesmf.so $prefix/lib
}



# =============================
# === INSTALL_DEPENDENCIES ====
# =============================
function gatherDependencies () {
    echo "Gathering dependent libraries . . ."

    echo "Gathering libraries for lib/* ..."
    cp -u `$gatherScript $prefix/lib/* | eval grep -v $gatherExcludeFilter` $prefix/lib
    cp -u `$gatherScript $prefix/lib/* | eval grep $gatherIncludeFilter` $prefix/lib


    echo "Gathering libraries for bin/* ..."
    cp -u `$gatherScript $prefix/bin/* | eval grep -v $gatherExcludeFilter` $prefix/lib
    cp -u `$gatherScript $prefix/bin/* | eval grep $gatherIncludeFilter` $prefix/lib

    return
}


# ============
# === MAIN ===
# ============

echo oss-install...

# Example calls:
# > oss-install.sh <prefix> <dest directory>              # Install entire solution

# 0. defaults:
prefix=$1
dest_main=$2
curdir=`pwd`

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
srcroot=$scriptdir/../..


if [ "$prefix" == '' ]; then
    echo "ERROR: No prefix directory specified as argument of oss-install.sh"
    exit 1
fi

if [ "$dest_main" == '' ]; then
    echo "ERROR: No destination directory specified as argument of oss-install.sh"
    exit 1
fi


echo Prefix            : $prefix
echo Target directory  : $dest_main
echo Current directory : $curdir
echo Source root dir   : $srcroot


gatherESMF

gatherDependencies

# Set executable bit
cd $prefix/bin
chmod a+x `find . -type f -exec file {} \; | grep executable | grep -v "\.svn" | cut -d ":" -f 1 | xargs`

cd $srcdir


exit $globalErrorLevel

