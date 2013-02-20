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


#-------------------------------------------------------------------------------
#Sources
delwaq1_SOURCES = block.f \
       dlwq00.f \
       getidentification.f

#-------------------------------------------------------------------------------
# Libraries

delwaq1_LDADD = \
    $(top_builddir)/engines_gpl/waq/packages/delwaq1_lib/src/libDelwaq1.la
