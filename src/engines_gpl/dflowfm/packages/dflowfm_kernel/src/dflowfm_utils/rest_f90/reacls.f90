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

      SUBROUTINE REACLS (MLAN)      ! DV
      implicit none
      double precision :: dv
      integer :: i
      integer :: jaauto
      integer :: mlan
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: nrow
      integer :: nv
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: x
!     ------------------------------------------------------------------
!     LEZEN FILE MET CLASSES
!    ------------------------------------------------------------------
      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      CHARACTER MATR*4

      CALL READYY ('READING CLS-FILE',0d0)

!     ------------------------------------------------------------------
!     EERST LEZEN ALS DUMMY WAARDEN OM TE TESTEN OF ER FOUTEN OPTREDEN
!     ------------------------------------------------------------------
   10 READ(MLAN,'(A)',END=999) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 10

      READ (MLAN,*,ERR=999) NROW

      NROW = MIN(NROW,30)

      DO 20 I = 1,NROW
         READ (MLAN,*,ERR=999,END=999) X
   20 CONTINUE

!     -------------------------------------
!     NO ERRORS, SO READ THE VALUES
!     -------------------------------------
      REWIND (MLAN)

  110 READ(MLAN,'(A)',END=999) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 110

      READ (MLAN,*,ERR=999) NROW

      NROW = MIN(NROW,30)

      DO 120 I = 1,NROW
         READ (MLAN,*) VAL(I)
         VMIN = MIN(VMIN,VAL(I))
         VMAX = MAX(VMAX,VAL(I))
  120 CONTINUE

      JAAUTO = 2
      NV = NROW

  999 CONTINUE

      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN
      END
