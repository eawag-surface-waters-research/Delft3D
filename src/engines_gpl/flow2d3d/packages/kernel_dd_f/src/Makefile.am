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
lib_LIBRARIES = libkernel_dd_f_mapping.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src \
	-I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics

#-------------------------------------------------------------------------------
#Sources
libkernel_dd_f_mapping_a_SOURCES = \
         flow_nextstep.f90

#-------------------------------------------------------------------------------
#Libraries

