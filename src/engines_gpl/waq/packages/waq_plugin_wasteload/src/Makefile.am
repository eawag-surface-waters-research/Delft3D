#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libwaq_plugin_wasteload.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libwaq_plugin_wasteload_la_SOURCES = \
       waq_plugin_wasteload_version.F90 \
       delwaq_user_wasteloads.f \
       zoekns.f

#-------------------------------------------------------------------------------
# Libraries
libwaq_plugin_wasteload_la_LIBADD = \
	$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
	$(top_builddir)/engines_gpl/waq/packages/wq_processes/src/libwqprocesses.la



BUILT_SOURCES = ./always \
	./waq_plugin_wasteload_version.F90

./always:
	rm -rf ./waq_plugin_wasteload_version.F90

./waq_plugin_wasteload_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/waq_plugin_wasteload_version.F90 $(srcdir) $(top_srcdir)/engines_gpl/waq/version/version_number.ini $(top_builddir)
