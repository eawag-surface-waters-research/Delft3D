!!  Copyright (C)  Stichting Deltares, 2012-2018.
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

      INTEGER FUNCTION POPDLM(N)
!
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 DLMCHR(256),IGNCHR(256)
      COMMON /ZDLMTZ/ IDLM, IIGN, PUSH(256), PTR
      INTEGER DLM(256), IGN(256)
      DATA DLM/32*0, 1, 11*0, 1, 211*0/
      DATA IGN/32*0, 1, 11*0, 1, 211*0/
      DATA PTR/0/, IDLM, IIGN/2, 2/
      DATA CHR/0/
!
      IF (PTR.EQ.0) GOTO 1001
      NDLM = PUSH(PTR-1)
      NIGN = PUSH(PTR)
      PTR = PTR - 2 - NIGN
      DO 10 I=1,NIGN
      CHR = PUSH(PTR+I)
! *** Convert INTEGER to CHARACTER
10    DLMCHR(I) = CHAR(CHR)
! *** Conversion completed.
      PTR = PTR - NDLM
      DO 20 I=1,NDLM
      CHR = PUSH(PTR+I)
! *** Convert INTEGER to CHARACTER
20    IGNCHR(I) = CHAR(CHR)
! *** Conversion completed.
      POPDLM = SETDLM(DLMCHR, NDLM, IGNCHR, NIGN)
      RETURN
1001  POPDLM = 1
      RETURN
      END
