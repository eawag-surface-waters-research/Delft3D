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
lib_LTLIBRARIES = libdeltares_common_c.la

#-------------------------------------------------------------------------------
#Includes
AM_CPPFLAGS = -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/include

#-------------------------------------------------------------------------------
#Sources
libdeltares_common_c_la_SOURCES = \
         cpuinfo.cpp \
         cutil.c \
         getopt.cpp \
         getputarray.cpp \
         meminfo.cpp \
         sems.cpp \
         stream-mpi.cpp \
         stream.cpp \
         throwexception.cpp


#-------------------------------------------------------------------------------
#Libraries

libdeltares_common_c_la_LIBADD =\
	$(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.la \
	$(top_builddir)/utils_lgpl/semaphore/packages/semaphore/src/libsemaphore.la
