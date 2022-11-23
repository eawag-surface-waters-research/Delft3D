#!/bin/bash

# Program will replace $1 by the $1.svn and will replace VERSION_BUILD_NUMBER by a corresponding svn version using svnversion command

# $1 - path to the target source file
# $2 - path to the folder to be used to check svnversion
# $3 - Single file with version number information version_number.ini
# $4 - Top directory of the build tree: used to define VN_DIR
# $5 - CMake_Binary_Dir: used to define VN_DIR

#===============================================================================

echo Generating version number in the $1
curdir=`pwd`
cd $2
MODDIR=`pwd`

# go back to the previous directory
cd $curdir
cd $4
TOPDIR=`pwd`
# Check if CMake_Binary_Dir is null or unset. Then use location inside src, else use CMake_Binary_Dir
if [ -z "$5" ] ; then
    VN_DIR=$TOPDIR/third_party_open/version_number/packages/version_number/src
else
    VN_DIR=$5/version_number
fi

cd $MODDIR

#   =====================================
#   Execute svnrevision
#   =====================================

#
# Be sure that SVN_VERSION always has a value
SVN_VERSION="000000";
cd $MODDIR
if svnversion . >/dev/null 2>/dev/null ; then
   SVN_VERSION=`svnversion -n $MODDIR`;

#   if [[ "$SVN_VERSION" =~ ^.*exported.*$ || "$SVN_VERSION" =~ ^Unversioned.* ]]; then
   if [[ "$SVN_VERSION" =~ ^exported.*$ || "$SVN_VERSION" =~ ^Unversioned.*$ ]]; then
      SVN_VERSION="000000"
   fi
fi;
#   also write it to file
# echo $SVN_VERSION > $MODDIR/$SVN_VERSION


# Generate version number source module using version_number.exe
$VN_DIR/version_number $SVN_VERSION $3 $1.svn $1.temp

if [ -f $1 ]; then
    diff $1 $1.temp > nul
    ERRORLEVEL=$?
else
    ERRORLEVEL=1
fi

if [ $ERRORLEVEL==1 ]; then
    mv $1.temp $1
    echo Done, new version number is: $SVN_VERSION
else
    rm $1.temp
    echo Done, file is up to date, no need to update version
fi
