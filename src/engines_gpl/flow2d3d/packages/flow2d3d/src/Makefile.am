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
lib_LTLIBRARIES = libflow2d3d_sp.la

FINCLUDES += \
	-I$(top_srcdir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Sources
libflow2d3d_sp_la_SOURCES = \
         flow2d3d_version.F90 \
         runme.f90

#-------------------------------------------------------------------------------
#Libraries
libflow2d3d_sp_la_LIBADD = \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/src/ddexec/libmanager_dd_ddexec.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/src/hydra/libmanager_dd_hydra.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/manager/src/libmanager.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/main/libkernel_main.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/non_hydro/libkernel_non_hydro.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/taylor/libkernel_taylor.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/timedep/libkernel_timedep.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute/libkernel_compute.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/libkernel_compute_nearfar.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/libkernel_compute_roller.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/libkernel_compute_sediment.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel_dd_f/src/libkernel_dd_f_mapping.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel_dd_c/src/mapper/libkernel_dd_c_mapping.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io/src/preprocessor/libio_preprocessor.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io/src/output/libio_output.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/inichk/libkernel_inichk.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/dd/libkernel_dd.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io/src/input/libio_input.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/kernel/src/general/libkernel_general.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_f/src/libio_dol_f.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_c/src/libio_dol_c.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/plugin_user/src/libplugin_user.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/libplugin_3dmortrafrm_c.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/allocation/libdata_allocation.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/general/libdata_general.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/gdp/libdata_gdp.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/libparallel.a \
    $(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics/libdata_basics.a \
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

