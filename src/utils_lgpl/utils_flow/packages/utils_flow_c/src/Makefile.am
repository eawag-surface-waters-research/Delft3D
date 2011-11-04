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
lib_LIBRARIES = libutils_flow_c.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_builddir)/utils_lgpl/utils_flow/packages/utils_flow_c/include

#-------------------------------------------------------------------------------
#Sources
libutils_flow_c_a_SOURCES = \
         cutil.c \
         getopt.cpp \
         getputarray.cpp \
         throwexception.cpp

#-------------------------------------------------------------------------------
#Libraries

