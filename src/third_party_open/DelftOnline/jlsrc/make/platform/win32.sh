#-------------------------------------------------------------------------------
#   WL Makefile -- Platform Definitions
#   	Microsoft Windows XP with Cygwin
#
#   Irv.Elshoff@wldelft.nl
#   10 oct 06
#
#   Copyright (C) 2006, WL | Delft Hydraulics
#-------------------------------------------------------------------------------



#-----	Initialize the Java environment

jdk="/cygdrive/c/Program Files/Java/jdk1.5.0_09"

sdk="$jdk"
jre="$jdk/jre"

if [ ! -d "$sdk" ]; then
    echo "Cannot find Java SDK \"$sdk\""
fi

if [ ! -d "$jre" ]; then
    echo "Cannot find Java JRE \"$jre\""
fi

export PATH="$sdk/bin:$PATH"

export LD_LIBRARY_PATH="$MAKE_HOME/lib/$MAKE_PLATFORM:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$jre/lib/i386:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$jre/lib/i386/client:$LD_LIBRARY_PATH"

#export CLASSPATH="$MAKE_HOME/classes/DelftOnline.jar:$CLASSPATH"



