#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq1

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/delwaq1_lib/src

#-------------------------------------------------------------------------------
#Sources
delwaq1_SOURCES = block.f \
       getidentification.f \
       dlwq00.f

#-------------------------------------------------------------------------------
# Libraries

delwaq1_LDADD = \
       $(top_builddir)/engines_gpl/waq/packages/delwaq1_version_number/src/libdelwaq1_version_number.la \
       $(top_builddir)/engines_gpl/waq/packages/delwaq1_lib/src/libDelwaq1.la

