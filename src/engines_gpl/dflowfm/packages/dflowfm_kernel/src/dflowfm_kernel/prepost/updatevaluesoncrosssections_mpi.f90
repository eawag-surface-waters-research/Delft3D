!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

!> Sums all monitored data on all cross sections, including time-integrated values.
!! for sequential/non-MPI models: stored in crs()%sumvalcur/sumvalcum
!! for parallel/MPI models: stored in sumvalcur_tmp, and needs later mpi_allreduce:
!! @see updateValuesOnCrossSections_mpi
subroutine updateValuesOnCrossSections_mpi(tim1)
   use m_monitoring_crosssections
   use m_partitioninfo
   use m_timer
   use m_transport , only: NUMCONST_MDU
   use m_sediment, only: jased, stmpar
   use m_alloc
   use mpi
   use m_flowtimes, only: tstart_user, ti_his
   implicit none
   double precision                 :: tim1, timtot
   integer                          :: iv, icrs, numvals, ierror

   ! This routine can now be called any time, but will only do the update
   ! of sumval* when necessary:
   
   !This method needs only be called on rank 0, but for some reason MPI processes desync and get deadlocked if we don't use allreduce with all processes
   !It should be checked if this is due to testing with Intel MPI or a design choice. If the first, change mpi_allreduce below to mpi_reduce.
   if (tlastupd_sumval == tim1 )then 
     return
   end if
   
   tlastupd_sumval = tim1

   ! This is done every time, why not save this in a module?
   numvals  = 5 + NUMCONST_MDU 

   if( jased == 4 .and. stmpar%lsedtot > 0 ) then
      numvals = numvals + stmpar%lsedtot + 1      
      if( stmpar%lsedsus > 0 ) then
         numvals = numvals + 1
      endif
   endif

   timtot = tim1 - tstart_user
   if (timtot == 0) then
   timtot = 1 ! So that the first time we don't divide by zero, avoids if condition in loop.
   endif
   
   ! Allocate separate arrays to store sum
    if (.not. allocated(sumvalcur_global)) then
       allocate(sumvalcur_global(numvals,ncrs))
       sumvalcur_global = 0d0      
    endif
    if (.not. allocated(sumvalcum_global)) then
       allocate(sumvalcum_global(numvals,ncrs))
       sumvalcum_global = 0d0      
    endif
    
   ! Sum current and cumulative values across MPI partitions
   if ( jatimer.eq.1 ) call starttimer(IOUTPUTMPI)  
   ! these two calls should happen asynchronously, currently they are blocking
    call mpi_allreduce(sumvalcum_local, sumvalcum_global,numvals*ncrs,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
    call mpi_allreduce(sumvalcur_local, sumvalcur_global,numvals*ncrs,mpi_double_precision,mpi_sum,DFM_COMM_DFMWORLD,ierror)
   if ( jatimer.eq.1 ) call stoptimer(IOUTPUTMPI)

   ! Update values of crs object
   do icrs=1,ncrs
   
      crs(icrs)%sumvalcur(1:numvals) = sumvalcur_global(1:numvals,icrs)
      crs(icrs)%sumvalcum(1:numvals) = sumvalcum_global(1:numvals,icrs)
      crs(icrs)%sumvalavg(1:numvals) = sumvalcum_global(1:numvals,icrs) / timtot / max(sumvalcum_timescale(1:numvals),1d0) 
      
   enddo
   
   return 
   
end subroutine updateValuesOnCrossSections_mpi
