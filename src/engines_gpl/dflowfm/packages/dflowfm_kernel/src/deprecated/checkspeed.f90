!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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

 subroutine checkspeed(rr)
 use unstruc_messages
 implicit none
 double precision :: mult0, mult1, mult, divt0, divt1, divt
 double precision :: t, ti, r, rr, rrm, rrd
 integer :: k, key

 call klok(mult0)


 do k   = 1,10000
    t   = 1d0*k - 1d0*k + 1.5155155d0
    ti  = 1d0/t
    r   = 0
    rrm = 0
    do key = 1,1000000
       r   = r + 1d0
       rr  = r*ti
       rrm = rrm + rr            ! remove this line and both loops will have identical perf on compaq visual
    enddo
 enddo

 call klok(mult1)

 call klok(divt0)

 do k   = 1,10000
    t   = 1d0*k - 1d0*k + 1.5155155d0
    ti  = 1d0/t
    r   = 0
    rrd = 0
    do key = 1,1000000
       r   = r + 1d0
       rr  = r/t
       rrd = rrd + rr
    enddo
 enddo

 call klok(divt1)

 mult = mult1-mult0
 divt = divt1-divt0

 write(msgbuf,*) 'mult ', mult
 call msg_flush()
 write(msgbuf,*) 'divt ', divt
 call msg_flush()
 write(msgbuf,*) 'divt/mult ', divt/mult
 call msg_flush()
 write(msgbuf,*) 'rrm, rrd, rrd-rrm ', rrm, rrd, rrd-rrm
 call msg_flush()

 end subroutine checkspeed
