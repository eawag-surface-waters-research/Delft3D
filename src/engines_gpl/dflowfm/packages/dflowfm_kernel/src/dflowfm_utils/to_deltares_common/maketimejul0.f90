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

 SUBROUTINE MAKETIMEjul0(TEX,TNr) ! maketime with jul0 already in module
 use m_flowtimes
 implicit none
 double precision :: tnr          ! time in hours
 CHARACTER TEX*(*)

 double precision :: Tuur, Tmin
 integer          :: nuur, nmin, nsec, iyyy,mm,id,ndag, jul0

 TEX  = '20010101 000000'
 JUL0 = julrefdat
 NDAG = TNR / 24.0
 CALL CALDAT(JUL0+NDAG,MM,ID,IYYY)

 TUUR = TNR - NDAG*24
 NUUR = TUUR
 TMIN = (TUUR - NUUR)*60
 NMIN = TMIN
 NSEC = (TMIN - NMIN)*60

 WRITE(TEX(1:4),'(I4.4)') IYYY
 WRITE(TEX(5:6),'(I2.2)') MM
 WRITE(TEX(7:8),'(I2.2)') ID

 WRITE(TEX(10:11),'(I2.2)') NUUR
 WRITE(TEX(12:13),'(I2.2)') NMIN
 WRITE(TEX(14:15),'(I2.2)') NSEC
 END  SUBROUTINE MAKETIMEjul0
