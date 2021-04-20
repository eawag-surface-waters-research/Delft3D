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
   integer                          :: icrs, iv, numvals, ierror

   ! This routine can now be called any time, but will only do the update
   ! of sumval* when necessary:
   if (tlastupd_sumval == tim1) then
      return
   end if
   tlastupd_sumval = tim1 ! When jampi==1 the sumval arrays are only correct after the reductions below.

   numvals  = 5 + NUMCONST_MDU

   if( jased == 4 .and. stmpar%lsedtot > 0 ) then
      numvals = numvals + 1
      if( stmpar%lsedsus > 0 ) then
         numvals = numvals + 1
      endif
   endif

   timtot = tim1 - tstart_user

   ! MPI communication between subdomains
   if ( jatimer.eq.1 ) call starttimer(IOUTPUTMPI)
   call reduce_crs(sumvalcur_tmp,ncrs,numvals)
   call reduce_crs(sumvalcumQ_mpi, ncrs, 1)
   if ( jatimer.eq.1 ) call stoptimer(IOUTPUTMPI)

   ! Update values
   do icrs=1,ncrs
      if (sumvalcur_tmp(IPNT_AUC, icrs) > 0) then
         sumvalcur_tmp(IPNT_U1A, icrs) = sumvalcur_tmp(IPNT_Q1C, icrs) / sumvalcur_tmp(IPNT_AUC, icrs)
         sumvalcur_tmp(IPNT_S1A, icrs) = sumvalcur_tmp(IPNT_S1A, icrs) / sumvalcur_tmp(IPNT_AUC, icrs)
         sumvalcur_tmp(IPNT_HUA, icrs) = sumvalcur_tmp(IPNT_HUA, icrs) / sumvalcur_tmp(IPNT_AUC, icrs)
      endif
      crs(icrs)%sumvalcur(1:numvals) = sumvalcur_tmp(1:numvals,icrs)
   enddo

   do icrs=1,ncrs
      do iv = 1, numvals ! Nu nog "5+ Numconst" standaard grootheden, in buitenlus
         if (iv == IPNT_Q1C) then
            crs(icrs)%sumvalcum(iv) = crs(icrs)%sumvalcum(iv) + sumvalcumQ_mpi(icrs)
         else
            ! TODO: AvD/JZ: UNST-1281: cumulative Q fort MPI runs is now correct, but:
            ! * jampi==1 code is quite different from jampi==0 for the sumvalcum.
            ! * And: sumvalcum for all other quantities than Q1C are wrong:
            crs(icrs)%sumvalcum(iv) = crs(icrs)%sumvalcum(iv) + max(sumvalcum_timescale(iv),1d0)*ti_his*sumvalcur_tmp(iv, icrs)
         end if
         if (timtot > 0d0) then
             crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcum(iv)/timtot/max(sumvalcum_timescale(iv),1d0)
         else
             crs(icrs)%sumvalavg(iv) = crs(icrs)%sumvalcur(iv)
         endif
       end do
   end do

   ! Total sums are now correctly in crs(:)%sumval*. Prepare for a new ti_his time interval with partial sums:
   sumvalcur_tmp = 0d0
   sumvalcumQ_mpi= 0d0

end subroutine updateValuesOnCrossSections_mpi
