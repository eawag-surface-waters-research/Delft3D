#-------------------------------------------------------------------------------
#   Makefile for Delft3D-AGRHYD:
#
#   Copyright (C) Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = agrhyd

#-------------------------------------------------------------------------------
#Includes

FINCLUDES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \ 
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(NETCDFROOT)/include

FMODULES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \ 
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(NETCDF_FORTRAN_LIBS)

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)
AM_FFLAGS  += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}  

agrhyd_SOURCES = \
   set_poi.f \
   gkwini.f \
   dhaggr.f \
   agr_poi.f \
   agrhyd_version.F90 \
   sing_dis.f \
   merge_atr.f \
   agr_len.f \
   agr_hyd_step.f \
   agr_reg.f \
   agr_hyd_init.f \
   set_aggr_reg.f \
   agr_poi_reg.f \
   agr_exp.f \
   write_sing_dis.f \
   set_aggr_pnts.f \
   agr_poi_exp.f \
   agr_atr.f \
   agrhyd.f90
#-------------------------------------------------------------------------------
#Libraries

agrhyd_LDADD   = \
    $(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src/libIOHyd.la \
    $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
    $(top_srcdir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) -lnetcdff \
    $(FCLIBS) \
	$(X_PRE_LIBS) $(XPLIB) $(X_LIBS) $(X_EXTRA_LIBS)

BUILT_SOURCES = ./always \
	./agrhyd_version.F90

./always:
	rm -rf ./agrhyd_version.F90

./agrhyd_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/agrhyd_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir) 
