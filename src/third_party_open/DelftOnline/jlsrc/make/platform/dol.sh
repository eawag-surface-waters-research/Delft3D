#-------------------------------------------------------------------------------
#   WL Makefile -- Platform Definitions
#
#   Irv.Elshoff@Deltares.NL
#   27 may 10
#
#   Copyright (C) 2006-2010, WL | Deltares
#-------------------------------------------------------------------------------


#-----	Initialize the Intel C++ compiler

cinit="/opt/intel/Compiler/11.0/081/bin/iccvars.sh"

if [ $cinit ]; then
    if [ ! -x $cinit ]; then
        echo "Cannot find C compiler \"$cinit\""
    else
        . $cinit ia32
    fi
fi


#-----	Initialize the Java environment

jdk="/opt/jdk1.6"

sdk="$jdk"
jre="$jdk/jre"

if [ ! -d $sdk ]; then
    echo "Cannot find Java SDK \"$sdk\""
fi

if [ ! -d $jre ]; then
    echo "Cannot find Java JRE \"$jre\""
fi

export JAVALIB="$jre/lib/i386/client"

export PATH="$sdk/bin:$PATH"

export LD_LIBRARY_PATH="$jre/lib/i386:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$jre/lib/i386/client:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="./lib/$MAKE_PLATFORM:$LD_LIBRARY_PATH"




