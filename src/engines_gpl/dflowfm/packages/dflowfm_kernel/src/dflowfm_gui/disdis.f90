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

      SUBROUTINE DISDIS()

      use m_devices
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      double precision :: dis
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
!     -------------------------------
!     write distance
!     -------------------------------
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      common /dispfor/ xyform, zform, disform
      character*7      xyform, zform, disform
      CHARACTER DISTAN*25

      DISTAN = 'DIS:'
      DIS    = dbdistance(xa,ya,xlc,ylc, jsferic, jasfer3D, dmiss)
      WRITE(DISTAN(6:),'(F17.5)') min(DIS,1d9)
      CALL KTEXT(DISTAN,IWS-24,3,15)

  !   checkdislin()

      RETURN
      END
