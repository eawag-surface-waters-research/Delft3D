#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@wldelft.nl
#   Adri.Mourits@wldelft.nl
#   Fedor.Baart@wldelft.nl
#   26 aug 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libparallel.la

#-------------------------------------------------------------------------------
#Includes
# TODO:/opt/mpich2/include should be configured....
FINCLUDES += \
        -I/opt/mpich2/include \
        -I$(top_srcdir)/utils_lgpl/parallel_mpi/include \
        -I$(top_srcdir)/utils_lgpl/precision/include \
        -I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
        -I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_srcdir)/engines_gpl/flow2d3d/packages/data/src/basics \
        -I$(top_srcdir)/utils_lgpl/ec_module/packages/ec_module/src \
        -I$(top_srcdir)/utils_lgpl/inifiles/packages/inifiles/src \
        -I$(top_srcdir)/utils_lgpl/utils_flow/packages/utils_flow/src

# fpp is compiler dependent (convention is to use .F90 files for preprocessing)
AM_FCFLAGS += \
        -DDFMPI \
        -fpp

#-------------------------------------------------------------------------------
#Sources
libparallel_la_SOURCES = \
         dfparall.f90 \
         initdfparall.f90 \
         dfinitmpi.f90 \
         dfsync.f90 \
         dfexitmpi.f90 \
         dfbroadc.f90 \
         dfwaiti.f90 \
         dfwaitr.f90 \
         dfwaitd.f90 \
         dfsendnb.f90 \
         dfrecvnb.f90 \
         dfsendi.f90 \
         dfsendr.f90 \
         dfsendd.f90 \
         dfreduce.f90 \
         cparri.f90 \
         cparrd.f90 \
         cparrr.f90 \
         dfupdr.f90 \
         dfupdd.f90 \
         dfupdi.f90 \
         dfexchg.f90 \
         dfdecomp.f90 \
         dfpartit.f90 \
         dfbladm.f90 \
         dforb.f90 \
         dfupdgeo1.f90 \
         dfupdgeo2.f90 \
         dfstrip.f90 \
         dfmassc.f90 \
         dffunctionals.f90 \
         dfgather.f90 \
         dfgather_grddim.f90 \
         dfwrirst.f90 \
         dfwrmorm1.f90 \
         dfwrmorm2.f90 \
         dfwrsedm.f90 \
         dfwrsedmavg.f90 \
         dfwrsedh.f90 \
         adjlin.f90 \
         rdencl.f90 \
         strgrd.f90

#-------------------------------------------------------------------------------
#Libraries

