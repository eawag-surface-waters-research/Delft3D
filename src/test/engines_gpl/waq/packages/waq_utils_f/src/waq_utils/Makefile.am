include $(top_srcdir)/common.am
bin_PROGRAMS = test_deltares_common

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources 
test_deltares_common_SOURCES = \
	basic_tests.f90 \
	test_waq_utils_f.f90

#-------------------------------------------------------------------------------
#Libraries

test_deltares_common_LDADD = \
	$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la
