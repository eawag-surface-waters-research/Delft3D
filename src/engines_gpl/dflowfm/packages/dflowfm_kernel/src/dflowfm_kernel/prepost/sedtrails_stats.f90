!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

module m_sedtrails_stats
   use m_sedtrails_data
   
   implicit none
   
   integer, parameter                       :: IDX_BL         = 1   !< Index for avg bottom level
   integer, parameter                       :: IDX_HS         = 2   !< Index for avg water depth
   integer, parameter                       :: IDX_UCX        = 3   !< Index for avg x velocity
   integer, parameter                       :: IDX_UCY        = 4   !< Index for avg y velocity
   integer, parameter                       :: IDX_TAUS       = 5   !< Index for avg x bed shear stress
   integer, parameter                       :: IDX_TAUSMAX    = 6   !< Index for avg y shear stress
   integer, parameter                       :: IDX_SBX        = 7
   integer, parameter                       :: IDX_SBY        = 8
   integer, parameter                       :: IDX_SSX        = 9
   integer, parameter                       :: IDX_SSY        = 10
   integer, parameter                       :: IDX_SSC        = 11  !< Index for avg sediment concentration
   !
   integer, parameter                       :: is_numndvals   = 11  !< Number of variables on flow nodes for which statistics are recorded.

   
   double precision, allocatable, target    :: is_sumvalsnd(:,:,:)
   
   character(len=100)                       :: is_valnamesnd(is_numndvals)
   double precision, target                 :: is_dtint             !< [s] total time interval since last statistics reset.
   
   contains
   
   !> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_sedtrails_stats()
   
      implicit none
      
      is_valnamesnd(:)  = ''
      is_valnamesnd(1)  = 'bl'
      is_valnamesnd(2)  = 'hs'
      is_valnamesnd(3)  = 'ucx'
      is_valnamesnd(4)  = 'ucy'
      is_valnamesnd(5)  = 'taus'      ! change to vector comps after sedmor merge
      is_valnamesnd(6)  = 'tausmax'
      is_valnamesnd(7)  = 'sbx'
      is_valnamesnd(8)  = 'sby'
      is_valnamesnd(9)  = 'ssx'
      is_valnamesnd(10) = 'ssy'
      is_valnamesnd(11) = 'ssc'
   
      ! Remaining of variables is handled in reset_integralstats()
      call reset_sedtrails_stats()
   end subroutine default_sedtrails_stats
   
   !> Resets only stats variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_sedtrails_stats() instead.
   subroutine reset_sedtrails_stats()

      implicit none

       is_sumvalsnd(1:is_numndvals,:,:) = 0d0
       is_dtint = 0d0
   end subroutine reset_sedtrails_stats
   
   subroutine alloc_sedtrails_stats()
      use m_alloc
      use m_fm_erosed, only: lsedtot
      use m_sediment, only: stm_included
      use m_flowgeom, only: ndx
      
      implicit none
      
      if (is_numndvals > 0) then
         if (stm_included) then 
            call realloc(is_sumvalsnd, (/ is_numndvals, ndx, lsedtot /), keepExisting = .false., fill = 0d0)
         else
            call realloc(is_sumvalsnd, (/ is_numndvals, ndx, 1 /), keepExisting = .false., fill = 0d0)
         end if
      end if
   
   end subroutine alloc_sedtrails_stats
   
   !> Update the (time-)integral statistics for all flow nodes, typically after each time step.
   subroutine update_sedtrails_stats()
      use m_flowtimes, only: dts
      use m_flow, only: hs, ucx, ucy, taus, kmx, ucxq, ucyq, hs, vol1
      use m_flowgeom, only: ndx, bl, ba
      use m_fm_erosed
      use m_transport, only: constituents, ISED1
      use m_sediment, only: sedtot2sedsus, stm_included, sedtra
      use m_flowparameters, only: jawave, flowWithoutWaves, jawaveswartdelwaq,epshu

      implicit none

      integer          :: k, l
      integer          :: kk, kbot, ktop
      double precision :: ssc

      if (is_numndvals <= 0) then
         return
      end if
      
      if (jawave==0 .or. flowWithoutWaves) then      ! do not overwrite current+wave induced bed shear stresses from tauwave
         call gettaus(1, 1)                           
      else
         call gettauswave(jawaveSwartdelwaq)
      endif
   
      do k=1,ndx
         is_sumvalsnd(IDX_BL  , k, 1) = is_sumvalsnd(IDX_BL      ,k, 1) + dts * bl(k)
         is_sumvalsnd(IDX_HS  , k, 1) = is_sumvalsnd(IDX_HS      ,k, 1) + dts * hs(k)
         is_sumvalsnd(IDX_TAUS, k, 1) = is_sumvalsnd(IDX_TAUS    ,k, 1) + dts * taus(k)
         
         is_sumvalsnd(IDX_UCX , k, 1) = is_sumvalsnd(IDX_UCX     ,k, 1) + dts * ucx(k)     ! assumes depth-averaged value in base node index
         is_sumvalsnd(IDX_UCY , k, 1) = is_sumvalsnd(IDX_UCY     ,k, 1) + dts * ucy(k)
      enddo

      
      if (stm_included) then
         do k=1,ndx
             is_sumvalsnd(IDX_TAUSMAX, k, 1) = is_sumvalsnd(IDX_TAUSMAX ,k, 1) + dts * sedtra%taub(k)  
         enddo
         
         do l=1,lsedtot 
            do k=1, ndx
               is_sumvalsnd(IDX_SBX , k, l) = is_sumvalsnd(IDX_SBX  ,k, l) + dts * (sbcx(k,l) + sbwx(k,l))
               is_sumvalsnd(IDX_SBY , k, l) = is_sumvalsnd(IDX_SBY  ,k, l) + dts * (sbcy(k,l) + sbwy(k,l))
               is_sumvalsnd(IDX_SSX , k, l) = is_sumvalsnd(IDX_SSX  ,k, l) + dts * (sscx(k,l) + sswx(k,l))
               is_sumvalsnd(IDX_SSY , k, l) = is_sumvalsnd(IDX_SSY  ,k, l) + dts * (sscy(k,l) + sswy(k,l))
            enddo
         end do 
         
         if (kmx==0) then
            do l=1, lsed
               do k=1,ndx
                  is_sumvalsnd(IDX_SSC , k, sedtot2sedsus(l)) = is_sumvalsnd(IDX_SSC  ,k, sedtot2sedsus(l)) + dts * constituents(ISED1+l-1,k)   ! this works just for 2D now
               enddo   
            enddo   
         else
            do l=1, lsed
               do k=1,ndx
                  if (hs(k)<=epshu) cycle
                  call getkbotktop(k,kbot,ktop)
                  ssc=0d0
                  do kk=kbot,ktop
                     ssc=ssc+constituents(ISED1+l-1,kk)*vol1(kk)
                  enddo
                  is_sumvalsnd(IDX_SSC , k, sedtot2sedsus(l)) = is_sumvalsnd(IDX_SSC  ,k, sedtot2sedsus(l)) + dts * ssc / ba(k) / hs(k) 
               enddo   
            enddo              
         endif
      endif
      
      is_dtint = is_dtint + dts

   end subroutine update_sedtrails_stats

end module m_sedtrails_stats
