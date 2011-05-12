#! /bin/bash

#-------------------------------------------------------------------------------
#   Top-Level Build Script for DeltaresHydro
#
#   There are command-line options to select Fortran compiler and debug or not.
#
#   ToDo:  Remove stripping of executables when the debug flag is set.
#   It's even debatable whether stipping belongs in the build.  I think not.
#
#   ToDo: Don't preintialize the compiler, the user should do this himself
#   so that he's aware exactly which version he's using.  Besides, we can't
#   keep up with every new compiler update.  This script should be ultra-low
#   maintanence.
#
#   Irv.Elshoff@Deltares.NL
#   9 mar 11
#
#   Copyright © 2011, Stichting Deltares
#-------------------------------------------------------------------------------


compiler='intel11'
platform='ia32'
debug=0
noMake=0


function usage {
    echo "Usage: `basename $0` [-intel10|-intel11|-intel12] [-debug] [-make] [-intel64] [-?]"
    }

function log {
    echo "`date +%Y%m%d.%H%M%S` :: $*"
    }


#-----  Process command-line arguments

while [ $# -gt 0 ]; do
    case $1 in
        -intel10)
            compiler='intel10'
            ;;
        -intel11)
            compiler='intel11'
            ;;
        -intel12)
            compiler='intel12'
            ;;
        -intel64)
            platform='intel64'
            ;;
        -d|-debug)
            debug=1
            ;;
        -m|-make)
            noMake=1
            ;;
        -?)
            usage
            exit 0
            ;;
        *)
            usage
            exit 1
            ;;
    esac
    shift
done

mkdir -p logs

if [ "$BASH_ENV" != '' ]; then
    echo 'Warning: Unsetting BASH_ENV'
    unset BASH_ENV
fi

# extra for linux64:
# - work-around to omit delft_online on linux-64bit
# - test pointersize in precision
if test "$platform" = "intel64"
then
    cp engines_gpl/flow2d3d/packages/flow2d3d/src/Makefile64bit.am engines_gpl/flow2d3d/packages/flow2d3d/src/Makefile.am
    cp engines_gpl/flow2d3d/packages/data/src/parallel_mpi/Makefile64bit.am engines_gpl/flow2d3d/packages/data/src/parallel_mpi/Makefile.am 
    chk8bit=`grep '^integer.*pntrsize.*8' utils_lgpl/precision/packages/precision/src/precision.f90`
    if test -z "$chk8bit"
    then
        echo "Error: pntrsize in precision <> 8"
        exit 1
    fi
fi

#-----  Initialize Fortran compiler

# Note:  If the 11.1 compiler is used and the svml library is used in common.am,
# the following message will be produced when running delftflow.exe:
# ./delftflow.exe: symbol lookup error: /opt/intel/Compiler/11.1/072/lib/ia32/libsvml.so: undefined symbol: __intel_cpu_indicator
# Solution: don't use the svml library in commmon.am

case $compiler in
    intel12)
        ifortInit=". /opt/intel/bin/ifortvars.sh $platform"
        #idbInit='. /opt/intel/bin/idbvars.sh'
        echo "Using Intel 12 Fortran ($platform) compiler"
        ;;
    intel11)
        if [ -d /opt/intel/Compiler/11.1/072 ]; then
            ifortInit=". /opt/intel/Compiler/11.1/072/bin/ifortvars.sh $platform"
            idbInit=". /opt/intel/Compiler/11.1/072/bin/$platform/idbvars.sh"
            echo "Using Intel 11.1 Fortran ($platform) compiler"
        elif [ -d /opt/intel/Compiler/11.0/081 ]; then
            ifortInit=". /opt/intel/Compiler/11.0/081/bin/ifortvars.sh $platform"
            idbInit=". /opt/intel/Compiler/11.0/081/bin/$platform/idbvars.sh"
            echo "Using Intel 11.0 Fortran ($platform) compiler"
        fi
        ;;
    intel10)
        ifortInit='. /opt/intel/fc/10/bin/ifortvars.sh'
        idbInit='. /opt/intel/idb/10/bin/idbvars.sh'
        echo "Using Intel 10 Fortran compiler"
        ;;
esac

eval $ifortInit
if [ $? -ne 0 ]; then
    echo 'Initialization of Intel Fortran compiler fails!'
    exit 1
fi

#eval $idbInit
#if [ $? -ne 0 ]; then
#    echo 'Initialization of Intel debugger fails!'
#    exit 1
#fi


#-----  Make scripts/binaries executable in case checked out via Windows

scripts="
    ./third_party_open/version_number/bin/linux/version_number.exe
    "

for file in $scripts; do
    chmod +x $file
done


#-----  Create configure script

log='logs/autoreconf.log'
command="autoreconf -ivf &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Autoreconfig fails!'
    exit 1
fi


#-----  Create makefiles

log='logs/configure.log'

if [ $debug -eq 1 ]; then
    flags='-g -O0'
else
    flags='-O2'
fi

if test "$platform" = "intel64"
then
   command=" \
       CFLAGS='$flags -fPIC -m64 $CFLAGS' \
       CXXFLAGS='$flags -fPIC -m64 $CXXFLAGS' \
       FFLAGS='$flags -fPIC -m64 $FFLAGS' \
       FCFLAGS='$flags -fPIC -m64 $FCFLAGS' \
       ./configure --prefix=`pwd` &> $log \
       "
else
   command=" \
       CFLAGS='$flags $CFLAGS' \
       CXXFLAGS='$flags $CXXFLAGS' \
       FFLAGS='$flags -DWITH_DELFTONLINE $FFLAGS' \
       FCFLAGS='$flags $FCFLAGS' \
       ./configure --prefix=`pwd` &> $log \
       "
fi

log "Running `echo $command | sed 's/ +/ /g'`"
eval $command

if [ $? -ne 0 ]; then
    log 'Configure fails!'
    exit 1
fi


#-----  Build and install everything

if [ $noMake -eq 1 ]; then
    log "Skipping make; execute the following command before manual makes:"
    echo $ifortInit
    exit 0
fi

log='logs/make.log'
command="make ds-install &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Make fails!'
    exit 1
fi

log "Build finished"
exit 0
