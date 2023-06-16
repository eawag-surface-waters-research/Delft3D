#-------------------------------------------------------------------------------
#   Makefile for Swan
#
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   Bert.Jagers@deltares.nl
#   4 mar 21
#
#   Copyright (C)  Stichting Deltares, 2011-2021.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libSwan.la

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS = ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS} -DUSE_MPI
FFLAGS  = ${SWAN_FFLAGS} -O2 ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS} 

FINCLUDES += \
	-I$(builddir)/../include

FMODULES += \
	-I$(top_builddir)/third_party_open/swan/src

#-------------------------------------------------------------------------------
#Sources
libSwan_la_SOURCES = \
         swmod1.for \
         SwanGriddata.f90 \
         swmod2.F \
         nctablemd.f90 \
         netcdf_tools.f90 \
         agioncmd.f90 \
         swn_outnc.f90 \
         HRextensions.f90 \
         SdsBabanin.f90 \
         SwanBndStruc.f90 \
         SwanGridobjects.f90 \
         SwanCompdata.f90 \
         SwanBpntlist.f90 \
         SwanCheckGrid.f90 \
         SwanCompUnstruc.f90 \
         SwanComputeForce.f90 \
         SwanConvAccur.f90 \
         SwanConvStopc.f90 \
         SwanCreateEdges.f90 \
         SwanCrossObstacle.f90 \
         SwanDiffPar.f90 \
         SwanDispParm.f90 \
         SwanFindObstacles.f90 \
         SwanFindPoint.f90 \
         SwanGSECorr.f90 \
         SwanGradDepthorK.f90 \
         SwanGradVel.f90 \
         SwanGridCell.f90 \
         SwanGridFace.f90 \
         SwanGridTopology.f90 \
         SwanGridVert.f90 \
         SwanInitCompGrid.f90 \
         SwanInterpolateAc.f90 \
         SwanInterpolateOutput.f90 \
         SwanInterpolatePoint.f90 \
         SwanIntgratSpc.f90 \
         SwanMaxOverNodes.f90 \
         SwanMinOverNodes.f90 \
         SwanPointinMesh.f90 \
         SwanPrepComp.f90 \
         SwanPrintGridInfo.f90 \
         SwanPropvelS.f90 \
         SwanPropvelX.f90 \
         SwanPunCollect.f90 \
         SwanReadADCGrid.f90 \
         SwanReadEasymeshGrid.f90 \
         SwanReadGrid.f90 \
         SwanReadTriangleGrid.f90 \
         SwanReadfort18.f90 \
         SwanSpectPart.for \
         SwanSumOverNodes.f90 \
         SwanSweepSel.f90 \
         SwanThreadBounds.f90 \
         SwanTranspAc.f90 \
         SwanTranspX.f90 \
         SwanVertlist.f90 \
         couple2adcirc.f90 \
         m_constants.f90 \
         m_fileio.f90 \
         serv_xnl4v5.f90 \
         mod_xnl4v5.f90 \
         ocpcre.for \
         ocpids.F \
         ocpmix.for \
         swan2coh.f90 \
         swancom1.F \
         swancom2.for \
         swancom3.for \
         swancom4.for \
         swancom5.for \
         swanmain.F \
         swanout1.for \
         swanout2.for \
         swanparll.F \
         swanpre1.for \
         swanpre2.for \
         swanser.for
#-------------------------------------------------------------------------------
#Libraries

