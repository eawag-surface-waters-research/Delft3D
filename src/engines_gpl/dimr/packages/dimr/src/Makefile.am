#-------------------------------------------------------------------------------
#   Makefile for dimr
#
#   Irv.Elshoff@Deltares.NL
#   Adri.Mourits@Deltares.NL
#   29 oct 11
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am


bin_PROGRAMS = dimr.exe

#-------------------------------------------------------------------------------
#Sources
dimr_exe_SOURCES = \
    dimr_exe_version.cpp \
    dimr_exe.cpp

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = \
    -I$(STANDARD_C_INCLUDES) \
    -I../include


#-------------------------------------------------------------------------------
#   Libraries


dimr_exe_LDADD = \
    $(top_builddir)/engines_gpl/dimr/packages/dimr_lib/src/libdimr.la \
    -ldl \
    $(FCLIBS)

# dimr_exe_LDFLAGS = -export-dynamic

#    -lmcheck


#-------------------------------------------------------------------------------

BUILT_SOURCES = \
    ./always \
    ../include/dimr_exe_version.h

./always:
	rm -f ../include/dimr_exe_version.h

../include/dimr_exe_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/dimr_exe_version.h $(srcdir) ../include/version_number.ini $(top_builddir) 