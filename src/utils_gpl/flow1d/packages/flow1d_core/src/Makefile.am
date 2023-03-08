#-------------------------------------------------------------------------------
#
#   Copyright (C) 2007-2018, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libflow1d_core.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
    -I$(top_builddir)/utils_gpl/flow1d/include

FMODULES += \
  -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------

#Sources
libflow1d_core_la_SOURCES = \
         nodes.f90 \
         GlobalParameters.f90 \
         forcinglist.f90 \
         branches.f90 \
         spatial_data.f90 \
         Roughness.f90 \
         Qsort.f90 \
         wetcrs_modules.F90 \
         CrossSections.f90 \
         struc_helper.f90 \
         universal_weir.f90 \
         Pump.f90 \
         Dambreak.f90 \
         general_structure.f90 \
         culvert.f90 \
         bridge.f90 \
         structures.f90 \
         compound.f90 \
         Storage.f90 \
         ObservationPoints.f90 \
         ObservCrossSections.f90 \
         Network.f90 \
         c_structures.f90 \
         callback_functions.F90 \
         cross_helper.f90 \
         flow1d_core.F90 

# libflow1d_core is a convenience library, which will be linked into toplevel flow1d library.
libflow1d_core_la_LIBADD=
