#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
FMODULES += \
        -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src

SUBDIRS = proc_preprocess \
          ods \
          waq_io \
          mpidum
