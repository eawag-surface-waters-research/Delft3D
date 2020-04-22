#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   luca.carniato@deltares.nl
#   18 jun 2018
#
#   Copyright (C)  Stichting Deltares, 2018.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libDhydrologyKernel.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
	-I$(top_builddir)/utils_gpl/dhydrology/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libDhydrologyKernel_la_SOURCES = \
         dhydrology_error.f90 \
         interception.f90 \
         horton.f90 \
         wflowSbm.f90
         

#-------------------------------------------------------------------------------
#Libraries

