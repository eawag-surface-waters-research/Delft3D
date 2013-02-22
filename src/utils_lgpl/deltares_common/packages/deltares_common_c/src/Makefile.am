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
noinst_LTLIBRARIES = libDeltaresCommonC.la

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/include

#-------------------------------------------------------------------------------
#Sources
libDeltaresCommonC_la_SOURCES = \
         cpuinfo.cpp \
         cutil.c \
         getopt.cpp \
         getputarray.cpp \
         meminfo.cpp \
         sems.cpp \
         stream.cpp \
         throwexception.cpp


#-------------------------------------------------------------------------------
#Libraries

