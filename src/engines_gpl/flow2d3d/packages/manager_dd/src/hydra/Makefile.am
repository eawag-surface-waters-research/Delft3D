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
lib_LIBRARIES = libmanager_dd_hydra.a


INCLUDES = \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_c/include \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/include \
    -I$(top_srcdir)/utils_lgpl/esmfsm/include \
    -I$(top_srcdir)/utils_lgpl/instrumentation/include \
    -I$(top_srcdir)/utils_lgpl/stream/include \
    -I$(top_srcdir)/third_party_open/DelftOnline/include \
    -I$(top_srcdir)/third_party_open/DelftOnline/include/linux


libmanager_dd_hydra_a_SOURCES = \
     category.cpp \
     clock.cpp \
     cluster.cpp \
     common.cpp \
     dictionary.cpp \
     get_num_domains.c \
     hydra.cpp \
     instrumentation.cpp \
     iterator.cpp \
     list.cpp \
     master.cpp \
     semaphore.cpp \
     slave.cpp \
     util.cpp

