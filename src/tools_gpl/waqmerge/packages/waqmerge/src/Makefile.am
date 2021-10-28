#-------------------------------------------------------------------------------
#   Makefile for waqmerge
#
#   michelle.jeuken@deltares.nl
#
#   Copyright (C) Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = waqmerge
FMODULES += \
        -I${NETCDFROOT}/include \
        -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Includes
#-------------------------------------------------------------------------------
#Sources
    waqmerge_SOURCES = \
    renum_bnd.f90 \
    overall_hyd.F90 \
    merge_step_unstruc.f90 \
    read_waqoutput_dir.f90 \
    waqmerge.f90


#-------------------------------------------------------------------------------
#Libraries

waqmerge_LDADD   = \
    $(top_builddir)/tools/waqmerge/packages/waqmerge_version_number/src/libwaqmerge_version_number.a \
    $(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src/libIOHyd.la \
    $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) -lnetcdff \
    $(FCLIBS) -lnetcdff \
    $(X_PRE_LIBS) $(XPLIB) $(X_LIBS) $(X_EXTRA_LIBS)
