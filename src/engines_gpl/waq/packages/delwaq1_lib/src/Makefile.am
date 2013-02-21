#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libDelwaq1.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src


#-------------------------------------------------------------------------------
#Sources
libDelwaq1_la_SOURCES = block.f \
       delwaq1.f \
       getidentification.f

#-------------------------------------------------------------------------------
# Libraries

libDelwaq1_la_LIBADD = \
    $(top_builddir)/engines_gpl/waq/packages/delwaq1_version_number/src/libdelwaq1_version_number.a \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.a \
    $(top_builddir)/engines_gpl/waq/packages/waq_kernel/src/charon/libcharon.a \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/waq_io/libwaq_io.a \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.a \
    $(top_builddir)/engines_gpl/waq/packages/waq_io/src/ods/libods.a \
    $(top_builddir)/engines_gpl/part/packages/kernel_f/src/libkernel_f.a \
    $(top_builddir)/engines_gpl/part/packages/io_f/src/libio_f.a \
    $(top_builddir)/engines_gpl/part/packages/utils_f/src/libutils_f.a \
    $(top_builddir)/engines_gpl/part/packages/data_f/src/libdata_f.a \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/interact/libinteract.a \
    $(top_builddir)/engines_gpl/waq/packages/delwaq1_version_number/src/libdelwaq1_version_number.a \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libESMFSM.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la 
 
#   -lpthread
