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
      module m_fioraa

      implicit none

      contains


      SUBROUTINE FIORAA (OUTVAL, NRVAR , TRRAAI, NORAAI, NOSYS )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : september 1995 by Jan van Beek
!
!     FUNCTION            : Fills output buffer OUTVAL for raaien
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : -
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     OUTVAL  REAL    NRVAR,*     OUTPUT  Values for vars on output grid
!     NRVAR   INTEGER       1     INPUT   Number of output vars
!     TRRAAI  REAL    NOSYS,*     INPUT   Tranport over raai for active substanc
!     NORAAI  INTEGER       1     INPUT   Number of raaien
!     NOSYS   INTEGER       1     INPUT   Number of parameters in TRRAAI
!
!     Declaration of arguments
!
      use timers

      INTEGER    NRVAR , NORAAI, NOSYS
      REAL       OUTVAL(NRVAR,*), TRRAAI(NOSYS,*)
!
!     Local
!
      integer   iraai, isys
      real, PARAMETER  :: RMISS = -999.
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "fioraa", ithandl )
!
!     Copy values into output buffer
!
      DO 30 IRAAI = 1 , NORAAI
         DO 10 ISYS = 1 , NOSYS
            OUTVAL(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)
   10    CONTINUE
         DO 20 ISYS = NOSYS + 1 , NRVAR
            OUTVAL(ISYS,IRAAI) = RMISS
   20    CONTINUE
   30 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_fioraa
