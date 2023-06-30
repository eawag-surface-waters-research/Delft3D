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
      module m_raatra

      implicit none

      contains


      SUBROUTINE RAATRA (NOSYS , NDMPQ , NORAAI, NTRAAQ, IORAAI,
     +                   NQRAAI, IQRAAI, IQDMP , DMPQ  , TRRAAI)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : march 1995 by Jan van Beek
!
!     FUNCTION            : Fills transport terms for raaien
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : -
!
!     COMMON BLOCKS       : -
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOSYS   INTEGER       1     INPUT   Total number of active substances
!     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
!     NORAAI  INTEGER       1     INPUT   Number of raaien
!     NTRAAQ  INTEGER       1     INPUT   Total number of exch. in raaien
!     IORAAI  INTEGER       *     INPUT   Output option for raai
!     NQRAAI  INTEGER       *     INPUT   Number of exchanges in raai
!     IQRAAI  INTEGER       *     INPUT   Exchanges in raai
!     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
!     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
!     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai
!
!     Declaration of arguments
!
      use timers

      INTEGER       NOSYS , NDMPQ , NORAAI, NTRAAQ
      INTEGER       IORAAI(*)             , NQRAAI(*)       ,
     +              IQRAAI(*)             , IQDMP(*)
      REAL          DMPQ(NOSYS,NDMPQ,*)   , TRRAAI(NOSYS,*)

!     local
      integer itel1, isys, iraai, nqc, iopt, iqc, iq, ipq

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "raatra", ithandl )
!
!     Local declarations
!
!
!     Loop over the raaien
!
      ITEL1 = 0
      DO 100 IRAAI = 1 , NORAAI
!
!        the exchange contributes
!
         NQC  = NQRAAI(IRAAI)
         IOPT = IORAAI(IRAAI)
         DO 30 IQC = 1 , NQC
            ITEL1 = ITEL1 + 1
            IQ    = IQRAAI(ITEL1)
            IF ( IQ .GT. 0 ) THEN
               IPQ  = IQDMP(IQ)
               DO 10 ISYS = 1 , NOSYS
                  IF ( IOPT .EQ. 1 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)+
     +                                    DMPQ(ISYS,IPQ,1)  -
     +                                    DMPQ(ISYS,IPQ,2)
                  ELSEIF ( IOPT .EQ. 2 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)+
     +                                    DMPQ(ISYS,IPQ,1)
                  ELSEIF ( IOPT .EQ. 3 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)-
     +                                    DMPQ(ISYS,IPQ,2)
                  ENDIF
   10          CONTINUE
            ELSE
               IPQ  = IQDMP(-IQ)
               DO 20 ISYS = 1 , NOSYS
                  IF ( IOPT .EQ. 1 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)-
     +                                    DMPQ(ISYS,IPQ,1)  +
     +                                    DMPQ(ISYS,IPQ,2)
                  ELSEIF ( IOPT .EQ. 2 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)+
     +                                    DMPQ(ISYS,IPQ,2)
                  ELSEIF ( IOPT .EQ. 3 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)-
     +                                    DMPQ(ISYS,IPQ,1)
                  ENDIF
   20          CONTINUE
            ENDIF
   30    CONTINUE
!
  100 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
!
      END

      end module m_raatra
