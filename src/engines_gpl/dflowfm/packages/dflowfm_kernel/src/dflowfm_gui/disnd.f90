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

 SUBROUTINE DISND(NN)   ! print node values
 use m_devices
 use m_flowgeom
 implicit none
 integer :: nn

 CHARACTER TEX*23
 DOUBLE PRECISION :: ZNOD

 IF (NN .LE. 0) THEN
    TEX = 'NO FLOW NODE FOUND    '
    CALL KTEXT(TEX,IWS-22,4,15)
 ELSE
    TEX = 'FLOW NODE NR:         '
    WRITE(TEX (14:),'(I10)') NN
    CALL KTEXT(TEX,IWS-22,4,15)
    TEX = 'VAL=                  '
    WRITE(TEX(6:), '(E18.11)') ZNOD(NN)
    CALL KTEXT(TEX,IWS-22,5,15)
    TEX = 'XZ =                  '
    WRITE(TEX(6:), '(E18.11)') XZ(NN)
    CALL KTEXT(TEX,IWS-22,6,15)
    TEX = 'yZ =                  '
    WRITE(TEX(6:), '(E18.11)') YZ(NN)
    CALL KTEXT(TEX,IWS-22,7,15)
 ENDIF

 RETURN
 END SUBROUTINE DISND
