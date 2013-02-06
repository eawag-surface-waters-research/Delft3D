#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libstream.la

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = -I$(top_builddir)/utils_lgpl/stream/include

#-------------------------------------------------------------------------------
#Sources
libstream_la_SOURCES = \
         stream.cpp

#-------------------------------------------------------------------------------
#Libraries
libstream_la_LIBADD = \
	$(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.la
