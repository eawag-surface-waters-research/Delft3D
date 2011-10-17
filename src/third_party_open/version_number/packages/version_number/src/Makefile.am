#-------------------------------------------------------------------------------
#   Makefile for version_number
#
#   Adri.Mourits@deltares.nl
#
#   Copyright (C) 2008, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = version_number.exe

#-------------------------------------------------------------------------------
#Includes

#-------------------------------------------------------------------------------
#Sources
version_number_exe_SOURCES = \
	version_number.f90

install-exec-local:
	cp -fp $(top_srcdir)/third_party_open/version_number/packages/version_number/src/version_number.exe $(top_srcdir)/third_party_open/version_number/bin/linux

