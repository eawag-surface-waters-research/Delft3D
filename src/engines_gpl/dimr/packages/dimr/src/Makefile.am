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
    dimr_version.cpp \
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

