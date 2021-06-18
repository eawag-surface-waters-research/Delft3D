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

 subroutine correctiefile(a)
 implicit none
 character*(*) a
 double precision :: am, ph
 character*8   cmp

 read (a,'(a)') cmp
 read (a(8:),*) am, ph

 if ( index(cmp,'O1')        .ne. 0 ) then
     am = am*1.100d0  ; ph = ph -  10d0
 else if ( index(cmp,'K1')   .ne. 0 ) then
     am = am*1.050d0  ; ph = ph -   5d0
 else if ( index(cmp,'P1')   .ne. 0 ) then
     am = am*1.050d0  ; ph = ph -   0d0
 else if ( index(cmp,'N2')   .ne. 0 ) then
     am = am*1.000d0  ; ph = ph -   5d0
 else if ( index(cmp,'M2')   .ne. 0 ) then
     am = am*1.150d0  ; ph = ph -   5d0
 else if ( index(cmp,'S2')   .ne. 0 ) then
     am = am*1.100d0  ; ph = ph -   0d0
 else if ( index(cmp,'L2')   .ne. 0 ) then
     am = am*1.000d0  ; ph = ph -  20d0
 else if ( index(cmp,'K2')   .ne. 0 ) then
     am = am*1.100d0  ; ph = ph - 0d0
 endif

 a =  ' '
 write(a,*) cmp, am, ph

 end subroutine correctiefile
