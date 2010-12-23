#-------------------------------------------------------------------------------
#   Makefile for DelftFlow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   22 aug 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am
lib_LIBRARIES = libio_dol_c.a


INCLUDES = \
    -I$(top_srcdir)/utils_lgpl/stream/include \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_c/include \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/include \
    -I$(top_srcdir)/third_party_lgpl/DelftOnline/include \
    -I$(top_srcdir)/third_party_lgpl/DelftOnline/include/linux

libio_dol_c_a_SOURCES = \
     d3dol.cpp \
     fortran.cpp
