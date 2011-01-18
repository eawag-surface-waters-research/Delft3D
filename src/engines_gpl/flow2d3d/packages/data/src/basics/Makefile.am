#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@wldelft.nl
#   Adri.Mourits@wldelft.nl
#   Fedor.Baart@wldelft.nl
#   11 jan 2007
#
#   Copyright (C) 2007, WL | Delft Hydraulics
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libdata_basics.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src \
	-I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libdata_basics_la_SOURCES = \
         message_module.f90 \
         string_module.f90 \
         bedcomposition_module.f90 \
         globaldata.f90 \
         sp_buffer.f90 \
         flow_tables.f90 \
         timers.f90

#-------------------------------------------------------------------------------
#Libraries

