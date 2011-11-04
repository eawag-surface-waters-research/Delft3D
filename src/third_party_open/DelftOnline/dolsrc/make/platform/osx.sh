#-------------------------------------------------------------------------------
#   WL Makefile -- Platform Definitions
#       Java for DelftOnline 
#
#   Irv.Elshoff@Deltares.NL
#   27 may 10
#
#   Copyright (C) 2006-2010, WL | Deltares
#-------------------------------------------------------------------------------


#-----  Initialize the GNU C and Fortran compiler 

# nothing to do here....

#-----  Initialize the Java environment

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

javalaunch=`cd extern/JavaLaunch ; /bin/pwd`

# Set DYLD_LIBRARY_PATH if needed here

# Set CLASSPATH if needed here




