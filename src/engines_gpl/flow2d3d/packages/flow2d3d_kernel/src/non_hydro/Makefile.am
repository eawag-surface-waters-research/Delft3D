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
noinst_LTLIBRARIES = libKernelNonHydro.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/src/basics \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libKernelNonHydro_la_SOURCES = \
         z_hydpres.f90 \
         z_hydpres_nhfull.f90 \
         z_ilu_nhfull.f90 \
         z_initcg.f90 \
         z_initcg_nhfull.f90 \
         z_lu.f90 \
         z_matpro.f90 \
         z_momcor.f90 \
         z_momcor_nhfull.f90 \
         z_precon.f90 \
         z_precon_ilu.f90 \
         z_solbicgstab.f90 \
         z_solcg.f90 \
         z_solcg_nhfull.f90 \
         z_vermom.f90 \
         z_vermom_horadv_iupw.f90 \
         z_vermom_horadv_mdue.f90 \
         z_vermom_nhfull.f90

#-------------------------------------------------------------------------------
#Libraries

