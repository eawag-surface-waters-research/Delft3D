#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq2

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include 


#-------------------------------------------------------------------------------
#Sources
delwaq2_SOURCES = block.f \
       getidentification.f \
       dlwq2.f

#-------------------------------------------------------------------------------
# Libraries
delwaq2_LDADD = \
       $(top_builddir)/engines_gpl/waq/packages/delwaq2_version_number/src/libdelwaq2_version_number.a \
       $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/waq_kernel/libWaqKernel.la \
       $(top_builddir)/engines_gpl/waq/packages/delwaq1_lib/src/libDelwaq2.la


# To be added when using Intel 11:
#    $(FCLIBS)
