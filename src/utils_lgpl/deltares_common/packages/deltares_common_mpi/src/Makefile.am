#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

noinst_LTLIBRARIES = libDeltaresCommonMpi.la

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src
	
AM_FCFLAGS += \
        -DDFMPI

#-------------------------------------------------------------------------------
#Sources
libDeltaresCommonMpi_la_SOURCES = \
	dfparall.F90 \
	quickin_dep_file.f90 \
	depfil.f90 \
	depfil_double.f90 \
	dfbroadc.F90

#-------------------------------------------------------------------------------
#Libraries

# MPI
FC=$(MPIFC)

