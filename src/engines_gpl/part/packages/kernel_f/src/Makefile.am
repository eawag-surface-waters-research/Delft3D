#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libkernel_f.la

AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
    -I$(NETCDFROOT)/include \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src

FMODULES += \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_lgpl/gridgeom/packages/gridgeom/src \
        -I$(top_builddir)/third_party_open/kdtree2/src-f90 \
        -I$(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src \
        -I$(NETCDF_FORTRAN_LIBS) \
        -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
        -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
        -I$(top_builddir)/engines_gpl/part/packages/data_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/io_f/src \
        -I$(top_builddir)/engines_gpl/part/packages/utils_f/src

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)
AM_FFLAGS  += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libkernel_f_la_SOURCES = \
         vdisp.f90 \
         mudrel.f90 \
         extout.f90 \
         partfl.f90 \
         oildsp.f90 \
         oildspfm.f90 \
         umagi.f90 \
         p10cor.f90 \
         inipart_asc.f90 \
         inipart.f90 \
         densty.f90 \
         part01.f90 \
         part10.f90 \
         part10fm.f90 \
         plotgr.f90 \
         part03.f90 \
         part21.f90 \
         part14.f90 \
         part14fm.f90 \
         part06.f90 \
         part15.f90 \
         partur.f90 \
         part08.f90 \
         part17.f90 \
         partwq.f90 \
         part09.f90 \
         part09fm.f90 \
         part18.f90 \
         partvs.f90 \
         partzp.f90 \
         p10ddb.f90 \
         diffus.f90 \
         partfm_decay.f90 \
         partfm.f90 \
         partini.f90\
         s_forcing.f90\
         t_forcing.f90\
         tidal_state.f90\
         random_surface_position.F90\
         orien_bathymetry.F90\
         orien_salinity.F90\
         orien_temperature.F90\
         re_orien_sal_temp.F90\
         intpltd_funtion.f90\
         intpltd_stagedev.f90\
         intpltd_motility.f90\
         intpltd_divelimit.f90\
         intpltd_diurnal.f90\
         vert_swimm_diurnal.f90\
         vert_swimm_tidal.f90\
         larvm2.f90\
         larvae.f90\
         behv_mangrove.F90\
         behv_horseshoecrab.F90\
         behv_european_eel.F90\
         behv_atlantic_salmon.F90\
         behv_asiancarpeggs.F90\
         ibm.f90\
         delpar.F90

#-------------------------------------------------------------------------------
#Libraries
