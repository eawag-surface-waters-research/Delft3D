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
lib_LIBRARIES = libkernel_dd.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src \
	-I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libkernel_dd_a_SOURCES = \
         caldpu_dd.f90 \
         chkdry_dd.f90 \
         chkrefinement.f90 \
         inigeo_dd.f90 \
         trakad_dd.f90 \
         upwhu_dd.f90 \
         wangp1.f90 \
         wangp3.f90 \
         xzyz_dd.f90

#-------------------------------------------------------------------------------
#Libraries

