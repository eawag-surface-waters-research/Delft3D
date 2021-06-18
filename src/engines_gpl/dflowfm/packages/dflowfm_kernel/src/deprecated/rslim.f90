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

 double precision function rslim(d1,d2,limtyp)
 implicit none
 double precision :: d1, d2
 double precision :: rminmod,rvanleer,rkoren,rcentral
 integer :: limtyp

 if (limtyp .eq. 0) then
    rslim = 0
 else if (limtyp .eq. 1) then                        ! codering guus, met voorloper
    rslim = d1*rminmod(d1,d2)
 else if (limtyp .eq. 2) then                        ! codering guus, met voorloper
    rslim = d1*rvanleer(d1,d2)
 else if (limtyp .eq. 3) then                        ! codering guus, met voorloper
    rslim = d1*rkoren(d1,d2)
 else if (limtyp .eq. 4) then                        ! monotonized central
    rslim = rcentral(d1,d2)
 endif
 return
 end function rslim
