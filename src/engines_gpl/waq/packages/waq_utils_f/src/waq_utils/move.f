!!  Copyright (C)  Stichting Deltares, 2012-2014.
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

      SUBROUTINE MOVE ( ARRAY1 , ARRAY2 , NOTOT )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: june  1988 by L.Postma
C
C     FUNCTION            : moves NVAL values from ARRAY1 to ARRAY2
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ARRAY1  REAL      NOTOT     INPUT   array to be copied
C     ARRAY2  REAL      NOTOT     OUTPUT  array to copy to
C     NOTOT   INTEGER     1       INPUT   total number of entries
C
      DIMENSION   ARRAY1(*) , ARRAY2(*)
C
      DO  10 I = 1,NOTOT
   10 ARRAY2(I) = ARRAY1(I)
C
      RETURN
      END
      subroutine dmove ( array1 , array2 , notot )

      real(8) ::  array1(*)
      real(4) ::  array2(*)

      do i = 1,notot
         array2(i) = array1(i)
      enddo

      return
      end
      subroutine smove ( array1 , array2 , notot )

      real(4) ::  array1(*)
      real(8) ::  array2(*)

      do i = 1,notot
         array2(i) = array1(i)
      enddo

      return
      end
