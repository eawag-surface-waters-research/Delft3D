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
lib_LTLIBRARIES = libmanager.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/esmfsm/include \
	-I$(top_srcdir)/utils_lgpl/precision/include \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/include \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_f/include

FMODULES += \
	-I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_srcdir)/utils_lgpl/delftio_sync/packages/delftio_sync/src \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_srcdir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src \
	-I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_f/src

# fpp is compiler dependent
AM_FCFLAGS += \
        -fpp

#-------------------------------------------------------------------------------
#Sources
libmanager_la_SOURCES = \
         tdatmain.f90 \
         tricom.F90 \
         triend.f90 \
         tripoi.f90 \
         trisim.f90

#-------------------------------------------------------------------------------
#Libraries

