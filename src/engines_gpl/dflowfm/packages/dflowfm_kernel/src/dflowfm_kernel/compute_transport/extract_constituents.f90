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

! 
! 

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
   use m_flowtimes

   implicit none

   integer :: i, iconst, k, kk, limmin, limmax,  ll, kb, k1, kt, ii
   double precision :: dmin, tempmi

   integer(4) ithndl /0/
   if (timon) call timstrt ( "extract_constituents", ithndl )
   limmax = 0
   limmin = 0
   
   do k=1,Ndkx
 
      if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         spirint(k) = constituents(ISPIR,k)
      endif

      if ( ISED1.ne.0 ) then
         do i=1,mxgr
            iconst = ISED1+i-1
            if (constituents(iconst,k)<0d0) then
               limmin = limmin + 1
               constituents(iconst,k) = 0d0
            endif
            !
            ! keep track of mass error because of concentration limitation
            if (constituents(iconst,k)>upperlimitssc) then
               limmax = limmax + 1
               maserrsed = maserrsed + vol1(k)*(constituents(iconst,k)-upperlimitssc)
               constituents(iconst,k) = upperlimitssc
            endif
            sed(i,k) = constituents(iconst,k)
         end do
      end if
   end do
   
   if (ISED1.ne.0 .and. jalogtransportsolverlimiting>0) then
      if (limmin>0) then
         write(msgbuf , *) 'Negative ssc encountered and limited, number of cells = ' , limmin  ; call msg_flush()
      endif
      !
      if (limmax>0) then
         write(msgbuf , *) 'SSC overshoots encountered and limited to ', upperlimitssc, ', number of cells = ',  limmax  ; call msg_flush()
      endif   
   endif

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
         limmin = 0
     
      if (tempmin .ne. dmiss) then 
         k1 = 1 ; if (kmx > 0) k1 = Ndx + 1
         do k = k1, Ndkx
            if (constituents(itemp,k) < tempmin) then
                constituents(itemp,k) = tempmin
                limmin   = limmin + 1
            endif
         enddo
      else if (isalt > 0) then ! only at surface limit to freezing point
         !a = -0.0575d0 ; b =� 1.710523d-3 ; c = -2.154996d-4 ; d = -7.53d-3 ; P = 0 ; 
         !fp = (a + b.*sqrt(S) + c.*S) .* S + d*P;
         do k = 1, Ndx
            kt = ktop(k)
            tempmi = ( -0.0575d0 - 2.154996d-4*constituents(isalt,kt) ) * constituents(isalt,kt)  
            if (constituents(itemp,kt) < tempmi) then
                constituents(itemp,kt) = tempmi
                limmin   = limmin + 1
            endif
         enddo
      endif
         if (limmin .ne. 0) then
            write(msgbuf , *) 'Min. temperature limited, number of cells Limmin = ' , limmin  ; call msg_flush()
      endif

   endif

   if (jasal .ne. 0) then
      limmax = 0 ; limmin = 0 ; numdots = 0
      dmin = huge(1d0)
      do kk = 1, Ndxi
         if (salimax .ne. dmiss) then
            do k = kbot(kk),ktop(kk)
               if (constituents(isalt,k) > salimax) then
                   constituents(isalt,k) = salimax
                  limmax  = limmax + 1
               endif
            enddo
         endif

         do k = kbot(kk),ktop(kk)
            if (constituents(isalt,k) < salimin) then
               dmin    = min(dmin,constituents(isalt,k))
               constituents(isalt,k) = salimin
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
  !
  ! When a cell become dries, keep track of the mass in the water column in sscum array. This will be accounted
  ! for in the bottom update when the cell becomes wet again. This prevents large concentration gradients and exploding bed levels.
  if (stm_included) then
     if (stmpar%morpar%bedupd .and. time1 >= tstart_user + stmpar%morpar%tmor*tfac) then
        if (ISED1>0) then
          do ll=1,mxgr
             do k=1,ndx
                if (hs(k)<stmpar%morpar%sedthr) then
                   call getkbotktop(k,kb,kt)
                   ssccum(ll,k) = ssccum(ll,k)+sum(constituents(ISED1+ll-1,kb:kt))/dts*bai_mor(k)*vol1(k)
                   constituents(ISED1+ll-1,kb:kt) = 0d0
                endif
             enddo
           enddo
        endif
     endif
  endif

  if (timon) call timstop( ithndl )
  return
end subroutine extract_constituents
