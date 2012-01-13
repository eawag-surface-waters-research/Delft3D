#! /bin/bash

#-------------------------------------------------------------------------------
#   Top-Level Build Script for Delft3D Open Source Code
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
#   13 dec 11
#
#   Copyright � 2011, Stichting Deltares
#-------------------------------------------------------------------------------

# Default values
compiler=''
platform='ia32'
debug=0
noMake=0
useSp=0


#-------------------------------------------------------------------------------
function usage {
    echo "Usage: `basename $0` <compiler> [-debug] [-make] [-64bit] [-sp] [-?]"
    echo "Compiler is one of:"
    echo "    -gnu"
    echo "    -intel10"
    echo "    -intel11.0 (-intel11)"
    echo "    -intel11.1"
    echo "    -intel12"
    }


#-------------------------------------------------------------------------------
# Add date time to logging info
function log {
    echo "`date +%Y%m%d.%H%M%S` :: $*"
    }



#-------------------------------------------------------------------------------
# Add a directory to an environment parameter
function addpath {
    path="$1"
    shift

    for dir in $*; do
        if [ -d $dir ]; then
            eval "export $path=\"$dir:\$$path\""
        fi
    done
    }


#-------------------------------------------------------------------------------
# Identify which program is used
function witch {
    w=`which $1`
    (
        cd `dirname $w`
        /bin/pwd
    )
    }


#===============================================================================
# Process command-line arguments

while [ $# -gt 0 ]; do
    case $1 in
        -gnu)
            compiler='gnu'
            ;;
        -intel10)
            compiler='intel10'
            ;;
        -intel11.0|-intel11)
            compiler='intel11.0'
            ;;
        -intel11.1)
            compiler='intel11.1'
            ;;
        -intel12)
            compiler='intel12'
            ;;
        -64bit)
            platform='intel64'
            ;;
        -d|-debug)
            debug=1
            ;;
        -m|-make)
            noMake=1
            ;;
        -sp)
            useSp=1
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

if [ "$compiler" == '' ]; then
    echo "You must specify a compiler"
    usage
    exit 1
fi

mkdir -p logs

if [ "$BASH_ENV" != '' ]; then
    echo 'Warning: Unsetting BASH_ENV'
    unset BASH_ENV
fi


#===============================================================================
# Initialize Fortran compiler

case $compiler in
    gnu)
        ifortInit=""
        addpath PATH /opt/gcc/bin
        addpath LD_LIBRARY_PATH /opt/gcc/lib /opt/gcc/lib64
        echo "Using GNU compilers in `witch gfortran`"
        ;;

    intel12)
        ifortInit=". /opt/intel/bin/ifortvars.sh $platform"
        #idbInit='. /opt/intel/bin/idbvars.sh'
        echo "Using Intel 12 Fortran ($platform) compiler"
        ;;

    intel11.1)
        if [ "$platform" == 'intel64' ]; then
            if [ -d /opt/intel/Compiler/11.1/072/bin/intel64 ]; then
                ifortInit=". /opt/intel/Compiler/11.1/072/bin/intel64/ifortvars_intel64.sh $platform"
                idbInit=". /opt/intel/Compiler/11.1/072/bin/intel64/idbvars.sh"
                echo "Using Intel 11.1 Fortran ($platform) compiler"
            fi
        else
            if [ -d /opt/intel/Compiler/11.1/072 ]; then
                ifortInit=". /opt/intel/Compiler/11.1/072/bin/ifortvars.sh $platform"
                idbInit=". /opt/intel/Compiler/11.1/072/bin/$platform/idbvars.sh"
                echo "Using Intel 11.1 Fortran ($platform) compiler"
            fi
        fi
        ;;

    intel11.0)
        if [ -d /opt/intel/Compiler/11.0/081 ]; then
            ifortInit=". /opt/intel/Compiler/11.0/081/bin/ifortvars.sh $platform"
            idbInit=". /opt/intel/Compiler/11.0/081/bin/$platform/idbvars.sh"
            echo "Using Intel 11.0 Fortran ($platform) compiler"
        fi
        ;;

    intel10)
        ifortInit='. /opt/intel/fc/10/bin/ifortvars.sh'
        idbInit='. /opt/intel/idb/10/bin/idbvars.sh'
        echo "Using Intel 10 Fortran compiler (DEPRECATED!)"
        ;;

    *)
        ifortInit='/bin/true'
        echo "Using default Linux Fortran compiler"
        ;;
esac

if [ "$ifortInit" != '' ]; then
    eval $ifortInit
    if [ $? -ne 0 ]; then
        echo 'Initialization of the Fortran compiler fails!'
        exit 1
    fi
fi


#===============================================================================
# Use the correct Autotools

# When the autotools are not installed in the default location,
# point to them explicitly
addpath PATH \
    /opt/automake/bin \
    /opt/autoconf/bin \
    /opt/libtool/bin


#===============================================================================
# Additional library settings

#---------------------
# mpich2
if [ "$compiler" = 'gnu' ]; then
    addpath PATH /opt/mpich2-1.4.1-gcc-4.6.2/bin
    export MPI_INCLUDE=/opt/mpich2-1.4.1-gcc-4.6.2/include
    export MPILIBS_ADDITIONAL="-L/opt/mpich2-1.4.1-gcc-4.6.2/lib -lfmpich -lmpich -lmpl"
    # export MPILIBS_ADDITIONAL=" "
    export MPIFC=/opt/mpich2-1.4.1-gcc-4.6.2/bin/mpif90  
else
    # Intel compilers
    addpath PATH /opt/mpich2/bin
    export MPI_INCLUDE=/opt/mpich2/include
    # export MPILIBS_ADDITIONAL="-L/opt/mpich2/lib -lfmpich"
    export MPILIBS_ADDITIONAL=" "
    if [ "$platform" = 'intel64' ]; then
        export MPIFC=/opt/mpich2-1.0.8-intel64/bin/mpif90  
    fi
fi


#---------------------
# Additional link flags/libraries
if [ "$compiler" = 'gnu' ]; then
    export LDFLAGSMT_ADDITIONAL=" "
    export FCLIBS_ADDITIONAL=" "
else
    # Intel compilers
    export LDFLAGSMT_ADDITIONAL="-lifcoremt"
    export FCLIBS_ADDITIONAL="-lifcore -limf"
fi


#---------------------
# netcdf
# export PKG_CONFIG_PATH=/opt/netcdf-4.1.1/ifort/lib/pkgconfig:$PKG_CONFIG_PATH
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/netcdf-4.1.1/ifort/lib:/opt/hdf5-1.8.5/lib


#===============================================================================
echo "Current settings:"
echo "export ACLOCAL=\"$ACLOCAL\""
echo "export AUTOMAKE=\"$AUTOMAKE\""
echo "export AUTOHEADER=\"$AUTOHEADER\""
echo "export AUTOCONF=\"$AUTOCONF\""
echo "export AUTORECONF_FLAGS=\"$AUTORECONF_FLAGS\""
echo "export FCLIBS_ADDITIONAL=\"$FCLIBS_ADDITIONAL\""
echo "export LIBTOOLIZE=\"$LIBTOOLIZE\""
echo "export LDFLAGS=\"$LDFLAGS\""
echo "export LDFLAGSMT_ADDITIONAL=\"$LDFLAGSMT_ADDITIONAL\""
echo "export LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\""
echo "export MPIFC=\"$MPIFC\""
echo "export MPI_INCLUDE=\"$MPI_INCLUDE\""
echo "export MPILIBS_ADDITIONAL=\"$MPILIBS_ADDITIONAL\""
echo "export PKG_CONFIG_PATH=\"$PKG_CONFIG_PATH\""
echo "export PATH=\"$PATH\""
echo


#===============================================================================
# Single precision executables require preparation before hand

if [ $useSp -eq 1 ]; then
    (
        cd utils_lgpl/precision
        command='scripts/changeprecision.tcl single'
        log "Executing \"$command\" in \"$PWD\" for single-precision executables"
        eval $command
        if [ $? -ne 0 ]; then
            log 'ABORT: Single-precision script failed'
            exit 1
        fi
    )
fi


#===============================================================================
# autogen: sanity checks, libtoolize and autoreconf

log='logs/autogen.log'
command="./autogen.sh &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Autogen fails! Check $log'
    exit 1
fi


#===============================================================================
# configure: Create makefiles

log='logs/configure.log'

if [ $debug -eq 1 ]; then
    flags='-g -O0'
else
    flags='-O2'
fi

# fPIC is the result of the mixing of static and libtool libraries. 
# If you want to avoid this you can use convenience libraries. 
# Don't do this for non AMD64 because it will lead to worse performance. 
# More information here:
# http://www.gentoo.org/proj/en/base/amd64/howtos/index.xml?full=1#book_part1_chap3

if [ "$platform" = 'intel64' ]; then
    command=" \
        CFLAGS='$flags -fPIC -m64 $CFLAGS' \
        CXXFLAGS='$flags -fPIC -m64 $CXXFLAGS' \
        FFLAGS='$flags -fPIC -m64 $FFLAGS' \
        FCFLAGS='$flags -fPIC -m64 $FCFLAGS' \
            ./configure --prefix=`pwd` &> $log \
        "
else
    command=" \
        CFLAGS='$flags -fPIC $CFLAGS' \
        CXXFLAGS='$flags -fPIC $CXXFLAGS' \
        FFLAGS='$flags -fPIC $FFLAGS' \
        FCFLAGS='$flags -fPIC $FCFLAGS' \
            ./configure --prefix=`pwd` &> $log \
        "
fi

log "Running `echo $command | sed 's/ +/ /g'`"
eval $command

if [ $? -ne 0 ]; then
    log 'Configure fails!'
    exit 1
fi


#===============================================================================
# make: Build and install everything

if [ $noMake -eq 1 ]; then
    log "Skipping make; execute the following command before doing manual makes:"
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
