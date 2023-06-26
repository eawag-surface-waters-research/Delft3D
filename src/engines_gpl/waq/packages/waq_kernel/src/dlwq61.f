!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dlwq61

      implicit none

      contains


      SUBROUTINE DLWQ61 ( CONC   , DERIV  , AMASS  , AMAT   , NOSEG  ,
     *                             NOTOT  , ISYS   , NSYS   , JTRACK )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june 1988 by L.Postma
!
!     FUNCTION            : zeros the matrix,
!                           updates first order term on the diagonal
!                           compresses DERIV for use in DELMAT
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
!     ----    -----      ------       ------- -----------
!     CONC    REAL     NOTOT*NOSEG    INPUT   first order term
!     DERIV   REAL     NOTOT*NOSEG    IN/OUT  right hand side matrix
!     AMASS   REAL        NOSEG       INPUT   closure error correction
!     AMAT    REAL (JTRACK*2+1)*NOSEG IN/OUT  matrix to invert
!     NOSEG   INTEGER       1         INPUT   number of segments
!     NOTOT   INTEGER       1         INPUT   total number of systems
!     ISYS    INTEGER       1         INPUT   system considered
!     NSYS    INTEGER       1         INPUT   number of systems to take
!     JTRACK  INTEGER       1         INPUT   number of codiagonals
!
      use timers

      real        CONC (NOTOT,*)  ,  DERIV(*)  ,  AMAT(*)  ,
     *            AMASS(      *)

      integer ntot, nsys, notot, noseg
      integer i, isys, istep, iset, iseg, ioff, jtrack

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq61", ithandl )
!
!         zero the matrix
!
      ISTEP = JTRACK*2 + 1
      NTOT  = NOSEG*ISTEP
      DO 10 I = 1 , NTOT
      AMAT(I) = 0.0
   10 CONTINUE
!
!         set the diagonal
!
      ISET = JTRACK + 1
      DO 20 ISEG = 1 , NOSEG
      AMAT(ISET) = -CONC(ISYS,ISEG)+AMASS(ISEG)
   20 ISET = ISET+ISTEP
!
!         set the right hand side
!
      ISET = 1
      IOFF = 0
      DO 40 ISEG = 1 , NOSEG
      DO 30 I = ISYS , ISYS+NSYS-1
      DERIV(ISET) = DERIV(IOFF+I)
   30 ISET = ISET+1
   40 IOFF = IOFF+NOTOT
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq61
