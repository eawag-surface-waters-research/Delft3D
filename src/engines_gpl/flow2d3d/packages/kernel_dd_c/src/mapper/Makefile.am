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
lib_LIBRARIES = libkernel_dd_c_mapping.a


INCLUDES = \
    -I$(top_srcdir)/utils_lgpl/esmfsm/include \
    -I$(top_srcdir)/utils_lgpl/instrumentation/include \
    -I$(top_srcdir)/utils_lgpl/precision/include \
    -I$(top_srcdir)/utils_lgpl/stream/include \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/io_dol_c/include \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/kernel_dd_c/include/mapper \
    -I$(top_srcdir)/engines_gpl/flow2d3d/packages/manager_dd/include \
    -I$(top_srcdir)/third_party_lgpl/DelftOnline/include \
    -I$(top_srcdir)/third_party_lgpl/DelftOnline/include/linux

libkernel_dd_c_mapping_a_SOURCES = \
     context-flowside.cpp \
     context-gawsside.cpp \
     context-mapside.cpp \
     context_distrib_functs.cpp \
     context_general_functs.cpp \
     flow_nxtstp.cpp \
     gaws.cpp \
     gaws_wang_eq.cpp \
     gawsbarr.cpp \
     hymapper.cpp \
     map_debug.cpp \
     map_messages.cpp \
     mapper_config.cpp \
     mapper_general.cpp \
     mapper_statemachine.cpp \
     mapper_uvz.cpp \
     varinfocoll.cpp
