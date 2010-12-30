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
lib_LIBRARIES = libkernel_compute_roller.a

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src \
	-I$(top_srcdir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libkernel_compute_roller_a_SOURCES = \
         constwave.f90 \
         difuwe.f90 \
         hds.f90 \
         hds_wf.f90 \
         le.f90 \
         massfl.f90 \
         orbvel.f90 \
         qkwbn.f90 \
         qkwcg.f90 \
         radstr.f90 \
         rbsig.f90 \
         rolcor.f90 \
         rollu.f90 \
         snel.f90 \
         tapf.f90 \
         turwav.f90 \
         varcon.f90 \
         waveu.f90 \
         wnr.f90

#-------------------------------------------------------------------------------
#Libraries

