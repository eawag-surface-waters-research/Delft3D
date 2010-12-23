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
lib_LIBRARIES = libkernel_main.a

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/esmfsm/include \
	-I$(top_srcdir)/utils_lgpl/precision/include \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_srcdir)/utils_lgpl/delftio_sync/packages/delftio_sync/src \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src \
	-I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libkernel_main_a_SOURCES = \
         sysini.f90 \
         trisol.f90 \
         z_trisol.f90 \
         z_trisol_nhfull.f90

#-------------------------------------------------------------------------------
#Libraries

