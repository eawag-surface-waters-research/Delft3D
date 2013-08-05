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
# Not sure how to get rid of the warning
# See discussion here. Problem is that we don't always set MPIFC...
# http://stackoverflow.com/q/3968656/386327
if HAVE_MPI
FC=$(MPIFC)
else
# we're not using the mpi compiler but we can't set FC conditionally
FC=$(FC)
endif
