#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = testesm

#-------------------------------------------------------------------------------
#Sources
testesm_SOURCES = \
         testesm.cpp

#-------------------------------------------------------------------------------
#Libraries

testesm_LDADD = \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/libESMFSM.la
