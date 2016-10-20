#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libDelwaq.la
 
#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/waq/packages/delftio/src/diof90 \
        -I$(top_builddir)/third_party_open/openda/core/native/include \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

MPILIBS += $(MPILIBS_ADDITIONAL)

#-------------------------------------------------------------------------------
#Sources
libDelwaq_la_SOURCES = \
       delwaq2_global_data.f90 \
       block.f \
       delwaq_version.F90 \
       getidentification.f \
       delwaq1.f \
       delwaq2.f \
       waq_omi_lib.f90 \
       waq_omi_api.f90 \
       waq_omi_substances.f90 \
       waq_bmi.F90 \
       waq_mt3d_api.f90

#-------------------------------------------------------------------------------
# Libraries

libDelwaq_la_LIBADD = \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel/libWaqKernel.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_process/libwaq_process.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/bloom/libbloom.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/charon/libcharon.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_data/src/proc_data/libproc_data.la \
    $(top_builddir)/engines_gpl/part/packages/kernel_f/src/libkernel_f.la \
    $(top_builddir)/engines_gpl/part/packages/io_f/src/libio_f.la \
    $(top_builddir)/engines_gpl/part/packages/utils_f/src/libutils_f.la \
    $(top_builddir)/engines_gpl/part/packages/data_f/src/libdata_f.la \
    $(top_builddir)/engines_gpl/part/packages/version_number/src/libpart_version_number.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/dlwq_netcdf/libdlwq_netcdf.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_couplib/libmod_couplib.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings/libmod_timings.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90/libDIOF90.la \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libDelftIOSHM.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/ods/libods.la \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/interact/libinteract.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_c/src/libwaq_utils_c.la \
    $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) -lnetcdff \ 
    -lpthread

# To be added when using Intel 11:
#    $(FCLIBS)

BUILT_SOURCES = ./always \
	./delwaq_version.F90

./always:
	rm -rf ./delwaq_version.F90

./delwaq_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/delwaq_version.F90 $(srcdir) $(top_srcdir)/engines_gpl/waq/version/version_number.ini $(top_builddir)
