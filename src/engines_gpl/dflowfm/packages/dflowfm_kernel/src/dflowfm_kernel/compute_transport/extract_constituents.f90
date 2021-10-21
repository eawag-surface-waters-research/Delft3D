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

!> extract constituent array
subroutine extract_constituents()
   use m_transport
   use m_flow
   use m_flowgeom
   use m_sediment
   use m_transport
   use messageHandling
   use m_missing
   use m_plotdots
   use timers

   implicit none

   integer :: i, iconst, k, kk, limmin, limmax
   double precision :: dmin

   integer(4) ithndl /0/
   if (timon) call timstrt ( "extract_constituents", ithndl )

   do k=1,Ndkx
      if ( ISALT.ne.0 ) then
         sa1(k) = constituents(ISALT,k)
      end if

      !if ( ITEMP.ne.0 ) then
         ! tem1(k) = constituents(ITEMP,k)
      !end if

      if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         spirint(k) = constituents(ISPIR,k)
      endif

      if ( ISED1.ne.0 ) then
         do i=1,mxgr
            iconst = ISED1+i-1
            sed(i,k) = constituents(iconst,k)
         end do
      end if
   end do

   if (jatem .ne. 0) then
      if (tempmax .ne. dmiss) then ! tem is now positive
         limmax = 0
         do k = 1, Ndkx
            if (constituents(itemp,k) > tempmax) then
                constituents(itemp,k) = tempmax
                limmax   = limmax + 1
            endif
         enddo
         if (limmax .ne. 0) then
            write(msgbuf , *) 'Max. temperature limited, number of cells Limmax = ' , limmax  ; call msg_flush()
         endif
      endif
      if (tempmin .ne. dmiss) then ! tem is now positive
         limmin = 0
         do k = 1, Ndkx
            if (constituents(itemp,k) < tempmin) then
                constituents(itemp,k) = tempmin
                limmin   = limmin + 1
            endif
         enddo
         if (limmin .ne. 0) then
            write(msgbuf , *) 'Min. temperature limited, number of cells Limmin = ' , limmin  ; call msg_flush()
         endif
      endif

   endif

   if (jasal .ne. 0) then
      limmax = 0 ; limmin = 0 ; numdots = 0
      dmin = huge(1d0)
      do kk = 1, Ndxi
         if (salimax .ne. dmiss) then
            do k = kbot(kk),ktop(kk)
               if (sa1(k) > salimax) then
                  sa1(k)  = salimax
                  limmax  = limmax + 1
               endif
            enddo
         endif

         do k = kbot(kk),ktop(kk)
            if (sa1(k) < salimin) then
               !if (sa1(k) < -1d-4) then
               !   call adddot( xz(kk) , yz(kk), sa1(k) )
               !endif
               dmin    = min(dmin,sa1(k))
               sa1(k)  = salimin
               limmin  = limmin + 1
            endif
         enddo
      enddo

      if (limmax .ne. 0) then
         write(msgbuf , *) 'Max. salinity limited, number of cells Limmax = ' , limmax  ; call msg_flush()
      endif
      if (limmin .ne. 0) then
         write(msgbuf , *) 'Min. salinity limited, number of cells Limmin = ' , limmin  ; call msg_flush()
         write(msgbuf , *) 'Min. salinity limited, min = ' , dmin  ; call msg_flush()
      endif
  endif

  if (jasal > 0 .and. maxitverticalforestersal > 0 .or. jatem > 0 .and. maxitverticalforestertem > 0) then
     call doforester()
  endif

  if (timon) call timstop( ithndl )
  return
end subroutine extract_constituents
