#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Irv.Elshoff@Deltares.NL
#   25 may 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

SUBDIRS = \
    dd

lib_LTLIBRARIES = \
    libflow2d3d.la

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../include

MPILIBS += $(MPILIBS_ADDITIONAL)

#-------------------------------------------------------------------------------
#Sources
libflow2d3d_la_SOURCES = \
    flow2d3d.cpp \
    flowol.cpp \
    flow2d3d_version.cpp

#-------------------------------------------------------------------------------
#Libraries
libflow2d3d_la_LIBADD = \
    $(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/manager/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/main/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/non_hydro/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/taylor/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/timedep/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel_dd_f/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/preprocessor/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/output/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/dd/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/input/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/general/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/plugin_user/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/allocation/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/gdp/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics/*.lo \
    $(top_builddir)/lib/libmeteo.a \
    $(top_builddir)/lib/libec_module.a \
    $(top_builddir)/lib/libdelftio_sync.a \
    $(top_builddir)/lib/libdelftio.a \
    $(top_builddir)/lib/libdelftio_shm.a \
    $(top_builddir)/lib/libD_hydro.a \
    $(top_builddir)/lib/libfsm.a \
    $(top_builddir)/lib/libfsm_c.a \
    $(top_builddir)/lib/libesm_c.a \
    $(top_builddir)/lib/libinifiles.a \
    $(top_builddir)/lib/libnefis.a \
    $(top_builddir)/lib/libmathconsts.a \
    $(top_builddir)/lib/libprecision.a \
    $(top_builddir)/lib/libsemaphore.a \
    $(top_builddir)/lib/libstream.a \
    $(top_builddir)/lib/libdeltares_common.a \
    $(top_builddir)/lib/libdeltares_common_c.a \
    $(top_builddir)/lib/libdelftio_sync_version_number.a \
    $(top_builddir)/lib/libdelftio_version_number.a \
    $(top_builddir)/lib/libesmfsm_version_number.a \
    $(top_builddir)/lib/libmeteo_version_number.a \
    $(top_builddir)/lib/libnefis_version_number.a \
    $(top_builddir)/lib/libstream_version_number.a \
    -L$(top_builddir)/lib -lDelftOnline \
    $(MPILIBS) \
    $(FCLIBS)


BUILT_SOURCES = \
	./always \
	$(srcdir)/../include/flow2d3d_version.h 

./always:
	rm -f $(srcdir)/../include/flow2d3d_version.h 

../include/flow2d3d_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/../include/flow2d3d_version.h $(srcdir) $(srcdir)/../include/version_number.ini $(top_builddir)

