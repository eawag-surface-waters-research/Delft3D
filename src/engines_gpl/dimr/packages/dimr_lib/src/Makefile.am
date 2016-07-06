#-------------------------------------------------------------------------------
#   Makefile for dimr_lib
#
#   Irv.Elshoff@deltares.nl
#   2 jun 11
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

lib_LTLIBRARIES = libdimr.la

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = \
    -I$(STANDARD_C_INCLUDES) \
    -I../include

#-------------------------------------------------------------------------------
#Sources
libdimr_la_SOURCES = \
    coredump.cpp \
    clock.cpp \
    component.cpp \
    dimr.cpp \
    dimr_lib_version.cpp \
    exception.cpp \
    log.cpp \
    stringutils.cpp \
    xmltree.cpp

#-------------------------------------------------------------------------------
#Libraries


#-------------------------------------------------------------------------------

BUILT_SOURCES = \
    ./always \
    ../include/dimr_lib_version.h

./always:
	rm -f ../include/dimr_lib_version.h

../include/dimr_lib_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/dimr_lib_version.h $(srcdir) ../include/version_number.ini $(top_builddir)


