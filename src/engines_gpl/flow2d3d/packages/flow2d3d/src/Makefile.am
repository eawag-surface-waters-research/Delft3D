#-------------------------------------------------------------------------------
#   Makefile for flow2d3d
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libflow2d3d.la

FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Sources
libflow2d3d_la_SOURCES = \
         flow2d3d_version.F90 \
         runme.f90

#-------------------------------------------------------------------------------
#Libraries
libflow2d3d_la_LIBADD = \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/src/ddexec/.libs/libmanager_dd_ddexec.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/src/hydra/.libs/libmanager_dd_hydra.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/manager/src/.libs/libmanager.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/main/.libs/libkernel_main.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/non_hydro/.libs/libkernel_non_hydro.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/taylor/.libs/libkernel_taylor.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/timedep/.libs/libkernel_timedep.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute/.libs/libkernel_compute.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/.libs/libkernel_compute_nearfar.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/.libs/libkernel_compute_roller.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/.libs/libkernel_compute_sediment.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel_dd_f/src/.libs/libkernel_dd_f_mapping.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel_dd_c/src/mapper/.libs/libkernel_dd_c_mapping.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io/src/preprocessor/.libs/libio_preprocessor.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io/src/output/.libs/libio_output.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/inichk/.libs/libkernel_inichk.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/dd/.libs/libkernel_dd.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io/src/input/.libs/libio_input.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/general/.libs/libkernel_general.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_f/src/.libs/libio_dol_f.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_c/src/.libs/libio_dol_c.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/plugin_user/src/.libs/libplugin_user.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/.libs/libplugin_3dmortrafrm_c.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/allocation/.libs/libdata_allocation.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/general/.libs/libdata_general.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/gdp/.libs/libdata_gdp.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/.libs/libparallel.la \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics/.libs/libdata_basics.la \
    $(top_srcdir)/utils_lgpl/meteo/packages/meteo/src/libmeteo.a \
    $(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src/libec_module.a \
    $(top_srcdir)/utils_lgpl/delftio_sync/packages/delftio_sync/src/libdelftio_sync.a \
    $(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90/libdelftio.a \
    $(top_srcdir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libdelftio_shm.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
    $(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src/libinifiles.a \
    $(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
    $(top_srcdir)/utils_lgpl/constants/packages/mathconsts/src/libmathconsts.a \
    $(top_srcdir)/utils_lgpl/precision/packages/precision/src/libprecision.a \
    $(top_srcdir)/utils_lgpl/semaphore/packages/semaphore/src/libsemaphore.a \
    $(top_srcdir)/utils_lgpl/stream/packages/stream/src/libstream.a \
    $(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src/libutils_flow.a \
    $(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow_c/src/libutils_flow_c.a \
    $(top_srcdir)/utils_lgpl/delftio_sync/packages/delftio_sync_version_number/src/libdelftio_sync_version_number.a \
    $(top_srcdir)/utils_lgpl/delftio/packages/delftio_version_number/src/libdelftio_version_number.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a \
    $(top_srcdir)/utils_lgpl/meteo/packages/meteo_version_number/src/libmeteo_version_number.a \
    $(top_srcdir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
    $(top_srcdir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.a \
    $(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow_version_number/src/libutils_flow_version_number.a \
    -L$(top_srcdir)/third_party_open/DelftOnline/lib -lDelftOnline -lDelftOnlineJNI -lJavaLaunch \
    $(EXTERNALSOURCES) \
    $(MPILIBS) \
    $(FCLIBS) \
    $(X_PRE_LIBS) -lXt $(XPLIB) $(X_LIBS) -lX11 $(X_EXTRA_LIBS)


BUILT_SOURCES = ./always \
	./flow2d3d_version.F90 

./always:
	rm -rf ./flow2d3d_version.F90 

./flow2d3d_version.F90:
	$(top_srcdir)/scripts/linux/update_version.sh $(srcdir)/flow2d3d_version.F90 $(srcdir) ../include/version_number.ini $(top_srcdir)

