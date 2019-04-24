include $(top_srcdir)/common.am
bin_PROGRAMS = test_dflowfm_kernel

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
	-I$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src

#-------------------------------------------------------------------------------
#Sources 
test_dflowfm_kernel_SOURCES = \
	test_read.f90 \
	test_dflowfm_kernel_f.f90

#-------------------------------------------------------------------------------
#Libraries

test_dflowfm_kernel_LDADD = \
	$(top_builddir)/engines_gpl/dflowfm/packages/dflowfm_kernel/src/libdflowfm.la \
	$(top_builddir)/utils_lgpl/ftnunit/packages/ftnunit/src/libFtnUnit.la
