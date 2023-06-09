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

      SUBROUTINE SETWINDOW(NSC,X1,Y1,X2,Y2,DXH,DYH)
      implicit none
      double precision :: dx
      double precision :: dxh
      double precision :: dy
      double precision :: dyh
      integer :: nsc
      integer :: numsc
      double precision :: x1
      double precision :: x1sc
      double precision :: x2
      double precision :: x2sc
      double precision :: y1
      double precision :: y1sc
      double precision :: y2
      double precision :: y2sc
      COMMON /GSCREENS/ X1SC(100),Y1SC(100),X2SC(100),Y2SC(100),NUMSC

      CALL viewport ( real(X1SC(NSC)),real(Y1SC(NSC)),real(X2SC(NSC)),real(Y2SC(NSC)) )
      DX  = (X2-X1)*0.1d0
      DY  = (Y2-Y1)*0.1d0
      DXH = DX/2d0
      DYH = DY/2d0
  !    CALL IGRUNITS( real(X1-DX),real(Y1-DY),real(X2+DX),real(Y2+DY) )
      CALL setwor( X1-DX,Y1-DY,X2+DX,Y2+DY )

      RETURN
      END
