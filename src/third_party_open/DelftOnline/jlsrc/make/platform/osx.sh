#-------------------------------------------------------------------------------
#   WL Makefile -- Platform Definitions
#
#   Irv.Elshoff@Deltares.NL
#   27 may 10
#
#   Copyright (C) 2006-2010, WL | Deltares
#-------------------------------------------------------------------------------


#-----	Initialize the GNU compilers

# nothing to do here

#-----	Initialize the Java environment

jdk="/System/Library/Frameworks/JavaVM.framework"

sdk="$jdk"
# OSX doesn't seem to have a separate jdk/jre
jre="$jdk"

if [ ! -d $sdk ]; then
    echo "Cannot find Java SDK \"$sdk\""
fi

if [ ! -d $jre ]; then
    echo "Cannot find Java JRE \"$jre\""
fi
# set JAVALIB, PATH, DYLD_LIBRARY_PATH here



