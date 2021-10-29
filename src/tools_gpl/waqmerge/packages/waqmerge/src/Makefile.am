#  Copyright (C)  Stichting Deltares, 2021-2021.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License version 3,
#  as published by the Free Software Foundation.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program. If not, see <http://www.gnu.org/licenses/>.
#
#  contact: delft3d.support@deltares.nl
#  Stichting Deltares
#  P.O. Box 177
#  2600 MH Delft, The Netherlands
#
#  All indications and logos of, and references to registered trademarks
#  of Stichting Deltares remain the property of Stichting Deltares. All
#  rights reserved.

include $(top_srcdir)/common.am

bin_PROGRAMS = waqmerge
FMODULES += \
        -I${NETCDFROOT}/include \
        -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
        -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Includes
#-------------------------------------------------------------------------------
#Sources
    waqmerge_SOURCES = \
    renum_bnd.f90 \
    overall_hyd.F90 \
    merge_step_unstruc.f90 \
    read_waqoutput_dir.f90 \
    waqmerge.f90


#-------------------------------------------------------------------------------
#Libraries

waqmerge_LDADD   = \
    $(top_builddir)/tools/waqmerge/packages/waqmerge_version_number/src/libwaqmerge_version_number.a \
    $(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src/libIOHyd.la \
    $(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
    $(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
    $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) -lnetcdff \
    $(FCLIBS) -lnetcdff \
    $(X_PRE_LIBS) $(XPLIB) $(X_LIBS) $(X_EXTRA_LIBS)
