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

  SUBROUTINE MERGENODES(K1,K2,JA)  ! KNOOP 1 WORDT OPGENOMEN IN KNOOP 2

  use m_netw
  use m_missing
  use gridoperations

  implicit none
  integer :: K1,K2,JA, L2, NN, K22, NM22, L2A, K22A, N1

  integer :: l
  integer :: l12
  integer :: n
  integer :: nm
  integer :: nm1
  integer :: nm2
  INTEGER :: NODLIN(200)

  JA  = 0
  NM1 = NMK(K1)
  NM2 = NMK(K2)
  CALL GIVELINKNUM(K1,K2,L12)
  if (L12 .ne. 0) then
     kn(1,L12) = 0 ; kn(2,L12) = 0; kn(3,L12) = 0
  endif

  DO NM = 1,NM1               ! CHECK OF JE NIET VIA EEN ANDER PAD OOK BIJ K2 UIT KAN KOEMN. ZO JA, VERWIJDER LINK
     L2 = NOD(K1)%LIN(NM)
     CALL OTHERNODE(K1,L2,K22)
     IF (K22 .NE. 0 .AND. K22 .NE. K2) THEN
        NM22 = NMK(K22)
        DO NN  = 1,NM22
           L2A  = NOD(K22)%LIN(NN)
           CALL OTHERNODE(K22,L2A,K22A)
           IF (K22A == K2) THEN
              kn(1,L2A) = 0 ; kn(2,L2A) = 0; kn(3,L2A) = 0
           ENDIF
        ENDDO
     ENDIF
  ENDDO

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

 ! call setnodadm(0); return


  if ( allocated(nod(k2)%lin) ) DEALLOCATE(NOD(K2)%LIN) ! hk did same as sp few lines below
  ALLOCATE ( NOD(K2)%LIN(NMK(K2)) )

  NOD(K2)%LIN(1:NMK(K2)) = NODLIN(1:NMK(K2))

  if ( allocated(nod(k1)%lin) ) deallocate (NOD(K1)%lin) ! %LIN = 0  ! SPvdP: added check
  KC (K1)     = 0
  NMK(K1)     = 0
  XK (K1)     = dxymis
  ja = 1 ! nepcheck

  RETURN
  END SUBROUTINE MERGENODES
