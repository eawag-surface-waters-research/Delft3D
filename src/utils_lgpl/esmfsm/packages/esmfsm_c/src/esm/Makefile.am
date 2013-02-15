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
lib_LTLIBRARIES = libESMC.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_builddir)/utils_lgpl/esmfsm/include \
           -I$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/include

#-------------------------------------------------------------------------------
#Sources
libESMC_la_SOURCES = \
         esm.cpp \
         esm_f.cpp \
         local.cpp \
         shared.cpp

#-------------------------------------------------------------------------------
#Libraries
