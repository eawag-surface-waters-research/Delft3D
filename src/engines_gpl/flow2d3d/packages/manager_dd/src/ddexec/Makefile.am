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
lib_LTLIBRARIES = libmanager_dd_ddexec.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_srcdir)/utils_lgpl/precision/include \
	-I$(top_srcdir)/utils_lgpl/stream/include \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/include

#-------------------------------------------------------------------------------
#Sources
libmanager_dd_ddexec_la_SOURCES = \
         barrier.cpp \
         config.cpp \
         ddexec.cpp \
         dredge_merge.cpp \
         dummy.cpp \
         final.cpp \
         init_barrier.cpp \
         read.cpp \
         rtc_comm.cpp 

#-------------------------------------------------------------------------------
#Libraries

