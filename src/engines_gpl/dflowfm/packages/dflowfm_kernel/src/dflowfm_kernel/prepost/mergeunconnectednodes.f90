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

  SUBROUTINE MERGEUNCONNECTEDNODES(K1,K2,JA)  ! KNOOP 1 WORDT OPGENOMEN IN KNOOP 2
  use m_netw
  use m_missing
  implicit none
  integer :: K1,K2,JA

  integer :: l
  integer :: n
  integer :: nm
  integer :: nm1
  integer :: nm2
  INTEGER :: NODLIN(200)

  JA  = 0
  NM1 = NMK(K1)
  NM2 = NMK(K2)

  N = 0
  DO NM = 1,NM2
     L  = NOD(K2)%LIN(NM)
     IF (KN(1,L) .NE. 0) THEN
        N = N + 1 ; NODLIN(N) = L
     ENDIF
  ENDDO

  DO NM = 1,NM1
     L = NOD(K1)%LIN(NM)
     IF (KN(1,L) .NE. 0) THEN
        N = N + 1 ; NODLIN(N) = L
        IF (KN(1,L) == K1) KN(1,L) = K2
        IF (KN(2,L) == K1) KN(2,L) = K2
     ENDIF
  ENDDO
  NMK(K2) = N


  if (allocated(nod(k2)%lin)) deallocate(NOD(K2)%LIN)
  ALLOCATE ( NOD(K2)%LIN(NMK(K2)) )
  NOD(K2)%LIN(1:NMK(K2)) = NODLIN(1:NMK(K2))

  if ( allocated(nod(k1)%lin) ) deallocate (NOD(K1)%lin) ! %LIN = 0  ! SPvdP: added check
  KC (K1)     = 0
  NMK(K1)     = 0
  XK (K1)     = dxymis
  ja = 1 ! nepcheck

  RETURN
  END SUBROUTINE MERGEUNCONNECTEDNODES
