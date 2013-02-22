#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libio_f.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I../../data_f/src \
	-I../../utils_f/src

#-------------------------------------------------------------------------------
#Sources
libio_f_la_SOURCES = \
	../../data_f/src/precision.f90 \
         dlwqbl.f90 \
         getdps.f90 \
         part12.f90 \
         part13.f90 \
         part16.f90 \
         parths.f90 \
         rdccol.f90 \
         rdfnam.f90 \
         rdlgri.f90 \
         rdpart.f90 \
         report_date_time.f90 \
         wrttrk.f90

#-------------------------------------------------------------------------------
#Libraries
