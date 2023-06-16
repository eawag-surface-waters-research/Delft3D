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
SUBDIRS = \
         read_xml_discharges 

noinst_LTLIBRARIES = libKernelComputeNearfar.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d_kernel/src/compute_nearfar/read_xml_discharges \ 
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libKernelComputeNearfar_la_SOURCES = \
         cor2world.f90 \
         corinp_gen.f90 \
         corinp_gen2.f90 \
         corjet2flow.f90 \
         cormix2flow.f90 \
         cortim2flow.f90 \
         cortim_no_modules.f90 \
         cortim_xyzs.f90 \
         coupled.f90 \
         ct2jettraj.f90 \
         desa.f90 \
         desa_kepco.f90 \
         determine_densprof.f90 \
         det_num_dis.f90 \
         discha_nf.f90 \
         dis_qjet.f90 \
         findnmk.f90 \
         init_nfl.f90 \
         interp_tk.f90 \
         discha_nf.f90 \
         jet3d2flow.f90 \
         near_field.f90 \
         nf_2_flow.f90 \
         tables.f90 \
         wait_a_sec.f90 \
         wait_until_finished.f90 \
         write_to_matlab.f90 \
         wri_cormix.f90 \
         wri_cortim.f90 \
         wri_FF2NF.f90 \
         wri_jet3d.f90 \
         wri_tek.f90


#-------------------------------------------------------------------------------
#Libraries

