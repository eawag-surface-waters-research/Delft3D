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
#   ToDo: Move DelftOnline to utils and treat it as an ordinary library
#
#   Irv.Elshoff@Deltares.NL
#   6 nov 11
#
#   Copyright © 2011, Stichting Deltares
#-------------------------------------------------------------------------------


compiler=''
platform='ia32'
debug=0
noMake=0
useSp=0

export PATH="/opt/mpich2/bin:$PATH"
export PKG_CONFIG_PATH=/opt/netcdf-4.1.1/ifort/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/netcdf-4.1.1/ifort/lib:/opt/hdf5-1.8.5/lib

# The updated autotools are not installed in the default location on the devux64,
# So we have to set all these variables manually. 
export ACLOCAL=/opt/automake-1.11.1/bin/aclocal
export AUTOMAKE=/opt/automake-1.11.1/bin/automake
export AUTOHEADER=/opt/autoconf-2.68/bin/autoheader
export AUTOCONF=/opt/autoconf-2.68/bin/autoconf
export LIBTOOLIZE=/opt/libtool-2.4.2/bin/libtoolize

# The extra macros are in this directory:
export AUTORECONF_FLAGS="-I /opt/automake-1.11.1/share/aclocal-1.11 -I/opt/libtool-2.4.2/share/aclocal -I/opt/autoconf-2.68/share/autoconf -I/usr/share/aclocal"

# And add them to the path...
export PATH=/opt/automake-1.11.1/bin:/opt/autoconf-2.68/bin:/opt/libtool-2.4.2/bin:$PATH

# This is needed to find the 64 bit ifort based mpi compiler
export MPIFC=/opt/mpich2-1.0.8-intel64/bin/mpif90  

# This is needed because that is where we installed DelftOnline 
# DelftOnline was prebuild in the repository
# What you could do is add the configure to the main configure so DelftOnline is build automaticly 
export LDFLAGS=-L`pwd`/lib 





function usage {
    echo "Usage: `basename $0` <compiler> [-debug] [-make] [-intel64] [-sp] [-?]"
    echo "Compiler is one of:"
    echo "    -gnu"
    echo "    -intel10"
    echo "    -intel11.0 (-intel11)"
    echo "    -intel11.1"
    echo "    -intel12"
    }

function log {
    echo "`date +%Y%m%d.%H%M%S` :: $*"
    }


#-----  Process command-line arguments

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
        -intel64)
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

#-----  Initialize Fortran compiler

case $compiler in
    gnu)
        ifortInit=""
        echo "Using default GNU compiler compiler"
        echo "Warning: The source code is not yet GNU friendly. We welcome help."
        echo -e "\n\n\n\n"
        sleep 3
        ;;

    intel12)
        ifortInit=". /opt/intel/bin/ifortvars.sh $platform"
        #idbInit='. /opt/intel/bin/idbvars.sh'
        echo "Using Intel 12 Fortran ($platform) compiler"
        ;;

    intel11.1)
        if [ -d /opt/intel/Compiler/11.1/072/bin/intel64 ]; then
            ifortInit=". /opt/intel/Compiler/11.1/072/bin/intel64/ifortvars_intel64.sh $platform"
            idbInit=". /opt/intel/Compiler/11.1/072/bin/intel64/idbvars.sh"
            echo "Using Intel 11.1 Fortran ($platform) compiler on 64bit OperatingSystem"
        elif [ -d /opt/intel/Compiler/11.1/072 ]; then
            ifortInit=". /opt/intel/Compiler/11.1/072/bin/ifortvars.sh $platform"
            idbInit=". /opt/intel/Compiler/11.1/072/bin/$platform/idbvars.sh"
            echo "Using Intel 11.1 Fortran ($platform) compiler"
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


#-----  Single precision executables require preparation before hand

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


#-----  Create configure script

log='logs/autoreconf.log'
command="autoreconf -ivf &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Autoreconfig fails!'
    exit 1
fi

#----- To ensure that all libtool the ltmain.sh and m4 macros are updated.

log='logs/libtoolize.log'
command="libtoolize --force &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Libtoolize fails!'
    exit 1
fi


#-----  In DelftOnline:
cd third_party_open/DelftOnline
#----------  Create makefiles

log='logs/configure_DelftOnline.log'
command="./configure --prefix=`pwd` &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Configure DelftOnline fails!'
    exit 1
fi

#----------  Build

log='logs/make_DelftOnline.log'
command="make &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Make DelftOnline fails!'
    exit 1
fi

#----------  and install

log='logs/install_DelftOnline.log'
command="make install &> $log"

log "Running $command"
eval $command

if [ $? -ne 0 ]; then
    log 'Install DelftOnline fails!'
    exit 1
fi



cd ../..



#-----  Create makefiles

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

if [ "$platform" = "intel64" ]; then
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
