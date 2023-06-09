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
noinst_LTLIBRARIES = libKernelInichk.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/include \
	-I$(top_builddir)/utils_lgpl/parallel_mpi/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/include

FMODULES += \
        -I$(top_builddir)/utils_gpl/morphology/packages/morphology_data/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/src/general \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_io_dol_f/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_kernel/src \
	-I$(top_builddir)/utils_gpl/morphology/packages/morphology_io/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_mpi/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/meteo

#-------------------------------------------------------------------------------
#Sources
libKernelInichk_la_SOURCES = \
         allocadv2d.f90 \
         caldps.f90 \
         caldpu.f90 \
         calpsh.f90 \
         chkbnd.f90 \
         chkcom.f90 \
         chkdis.f90 \
         chkdro.f90 \
         chkdry.f90 \
         chkgeo.f90 \
         chkic.f90 \
         chkiwe.f90 \
         chkkc.f90 \
         chklod.f90 \
         chknum.f90 \
         chkphy.f90 \
         chkset.f90 \
         chksit.f90 \
         chkstr.f90 \
         chktim.f90 \
         chktra.f90 \
         chktrt.f90 \
         chkvic.f90 \
         creategridborder.f90 \
         d3d_in_openda.f90 \
         getdx.f90 \
         getdy.f90 \
         inchki.f90 \
         inchkr.f90 \
         increm.f90 \
         inibcc.f90 \
         inibcparl.f90 \
         inibcq.f90 \
         inibct.f90 \
         inibnd.f90 \
         inicdw.f90 \
         inidis.f90 \
         inieva.f90 \
         inigeo.f90 \
         inigrd.f90 \
         iniphy.f90 \
         inirgl.f90 \
         inised.f90 \
         initau.f90 \
         initem.f90 \
         initgf.f90 \
         initmerge.f90 \
         init_out_heatfluxes.f90 \
         init_mom_output.f90 \
         initur.f90 \
         inivol.f90 \
         iwecof.f90 \
         mirror_bnd.f90 \
         tkecof.f90 \
         xyder.f90 \
         z_chkdry.f90 \
         z_initur.f90 \
         z_inizm.f90 \
         z_kfmnmx.f90

#-------------------------------------------------------------------------------
#Libraries

