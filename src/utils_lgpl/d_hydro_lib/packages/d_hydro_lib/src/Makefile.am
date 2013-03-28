#-------------------------------------------------------------------------------
#   Makefile for d_hydro_lib
#
#   Irv.Elshoff@deltares.nl
#   2 jun 11
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

noinst_LTLIBRARIES = libDHydroLib.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
        -I../include \
        -I../../../include

#-------------------------------------------------------------------------------
#Sources
libDHydroLib_la_SOURCES = \
    coredump.cpp \
    clock.cpp \
    component.cpp \
    d_hydro_lib_version.cpp \
    exception.cpp \
    log.cpp \
    stringutils.cpp \
    xmltree.cpp

BUILT_SOURCES = ./always \
	../include/d_hydro_lib_version.h

./always:
	rm -rf ../include/d_hydro_lib_version.h

../include/d_hydro_lib_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/d_hydro_lib_version.h $(srcdir) ../include/version_number.ini $(top_builddir)

#-------------------------------------------------------------------------------
#Libraries
