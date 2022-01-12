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

 subroutine checkcellfile(mout)                      ! check first two lines for consistency
 use m_netw
 use m_flowgeom

 implicit none
 integer             :: mout, numkr, numlr, L1
 character (len=256) :: rec

 read(mout,'(A)',end = 999) rec
 L1 = index(rec,'=') + 1
 read (rec(L1:), *, err = 888) numkr
 if (numkr .ne. numk) then
    call doclose(mout) ; mout = 0 ; return
 endif

 read(mout,'(A)',end = 999) rec
 L1 = index(rec,'=') + 1
 read (rec(L1:), *, err = 777) numlr
 if (numLr .ne. numL) then
    call doclose(mout) ; mout = 0 ; return
 endif
 return

 999 call    eoferror(mout)
 888 call qnreaderror('trying to read nr of net nodes but getting',rec,mout)
 777 call qnreaderror('trying to read nr of net links but getting',rec,mout)

 end subroutine checkcellfile
