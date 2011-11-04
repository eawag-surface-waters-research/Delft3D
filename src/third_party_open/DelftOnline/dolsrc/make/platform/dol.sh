#-------------------------------------------------------------------------------
#   WL Makefile -- Platform Definitions
#       Java for DelftOnline 
#
#   Irv.Elshoff@Deltares.NL
#   27 may 10
#
#   Copyright (C) 2006-2010, WL | Deltares
#-------------------------------------------------------------------------------


#-----  Initialize the Intel C++ and Fortran compilers

cinit="/opt/intel/Compiler/11.0/081/bin/iccvars.sh"
finit="/opt/intel/Compiler/11.0/081/bin/ifortvars.sh"

if [ $cinit ]; then
    if [ ! -x $cinit ]; then
        echo "Cannot find C compiler \"$cinit\""
    else
        . $cinit ia32
    fi
fi

if [ $finit ]; then
    if [ ! -x $finit ]; then
        echo "Cannot find Fortran compiler \"$finit\""
    else
        . $finit ia32
    fi
fi


#-----  Initialize the Java environment

jdk="/opt/jdk1.6"

sdk="$jdk"
jre="$jdk/jre"

if [ ! -d $sdk ]; then
    echo "Cannot find Java SDK \"$sdk\""
fi

if [ ! -d $jre ]; then
    echo "Cannot find Java JRE \"$jre\""
fi

export PATH="$sdk/bin:$PATH"

javalaunch=`cd extern/JavaLaunch ; /bin/pwd`

export LD_LIBRARY_PATH="$MAKE_HOME/lib/$MAKE_PLATFORM:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$javalaunch/lib:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$jre/lib/i386:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$jre/lib/i386/client:$LD_LIBRARY_PATH"

export CLASSPATH="$MAKE_HOME/classes/DelftOnline.jar:$CLASSPATH"



