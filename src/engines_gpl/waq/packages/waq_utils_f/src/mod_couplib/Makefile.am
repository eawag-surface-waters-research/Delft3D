#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libmod_couplib.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I/opt/mpich2/include \
        -I$(top_builddir)/engines_gpl/waq/include \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/mod_timings

#-------------------------------------------------------------------------------
#Sources
libmod_couplib_la_SOURCES = \
       m_coupcns.f90 \
       m_ixset.f90 \
       m_prcgrp.f90 \
       m_intfc.f90 \
       m_sndrcv.f90  \
       m_globcomm.f90 \
       reshape_idata.f90 \
       reshape_rdata.f90 \
       m_couplib.f90 \
       reshape_ddata.f90

#-------------------------------------------------------------------------------
# Libraries
