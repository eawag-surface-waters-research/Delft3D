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

      SUBROUTINE GETLOC ( FNAME  , ITYPE  , LOCDEF , MAXDEF , IPRDEP ,
     *                    ITMDEP , MAXLST , LOCLST , LOCTYP , LOCNR  ,
     *                                      NRLST  , IERROR , OPTION )
!
!
!     Deltares        MARINE & COASTAL MANAGEMENT
!
!     CREATED            : May '96  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : ODS GETLOC routine for DELWAQ HIS-files
!
!     SUBROUTINES CALLED :
!
!     LOGICAL UNITS      :
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     FNAME   CHAR*256   3        IN/LOC  Complete file name
!     ITYPE   INTEGER    1        INPUT   File type
!     LOCDEF  CHAR*20  MAXDEF     INPUT   List with wanted locations
!     MAXDEF  INTEGER    1        INPUT   Length of LOCDEF
!     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
!     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
!     MAXLST  INTEGER    1        INPUT   Dimension of the output arrays
!     LOCLST  CHAR*20  MAXLST     OUTPUT  List of locations found
!     LOCTYP  INTEGER  MAXLST     OUTPUT  List of location types
!     LOCNR   INTEGER  MAXLST     OUTPUT  List of index nr. locations
!     NRLST   INTEGER    1        OUTPUT  Nr of parameters found
!     IERROR  INTEGER    1        OUTPUT  Error code
!     OPTION  CHAR*256   1        IN/OUT  For future use
!
!
      use m_dhucas
      use m_dhopnf
      use m_dhfext

      CHARACTER*256 FNAME(3) , OPTION
      CHARACTER*20  LOCDEF(MAXDEF) , LOCLST(MAXLST)
      DIMENSION     LOCTYP(MAXLST) , LOCNR (MAXLST)
      LOGICAL       SETALL
      character*256         :: ext     ! file extension
      integer               :: extpos  ! position of extension
      integer               :: extlen  ! length of file extension
      logical               :: mapfil  ! true if map file extension
      integer               :: lun
!
!         Open the DELWAQ .HIS file
!
      lun = 10
      CALL DHOPNF ( lun  , FNAME(1) , 24 , 2 , IERROR )
      IF ( IERROR .NE. 0 ) RETURN

      ! map or his

      call dhfext(fname(1), ext, extpos, extlen)
      call dhucas(ext, ext, extlen)
      if ( ext .eq. 'MAP' ) then
         mapfil = .true.
      else
         mapfil = .false.
      endif
!
!         Read primary system characteristics
!
      READ ( lun , ERR=100 )   FNAME(3)(1:160)
      READ ( lun , ERR=110 )   NOTOT, NODUMP
      READ ( lun , ERR=120 ) ( FNAME(3)(181:200) , K = 1,NOTOT )
!
!         Read parameter names and try to find the wanted subset
!
      NRLST = 0
      SETALL = .FALSE.
      IF ( LOCDEF(1) .EQ. '*' ) SETALL = .TRUE.

      DO 40 I1 = 1 , NODUMP , MAXLST
         MAXK = MIN(NODUMP,I1+MAXLST-NRLST-1) - I1 + 1
         if ( .not. mapfil ) then
            READ ( lun , ERR=130 ) ( IDUMMY, LOCLST(K), K = NRLST+1, NRLST+MAXK)
         else
            do k = nrlst+1, nrlst+maxk
               write(loclst(k), '(''segment '',i8)' ) k
            enddo
         endif
         NBASE = NRLST
         DO 30 I2 = 1 , MAXK
            DO 20 I3 = 1 , MAXDEF
               IF ( LOCLST(NBASE+I2) .EQ. LOCDEF(I3) .OR. SETALL ) THEN
                  NRLST = NRLST + 1
                  IF ( NRLST .GT. MAXLST ) THEN
                     IERROR = -NODUMP
                     GOTO 50
                  ENDIF
                  LOCLST(NRLST) = LOCLST(NBASE+I2)
                  LOCNR (NRLST) = I1 + I2 - 1
                  GOTO 30
               ENDIF
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
!
!         Supply the desired statistics
!
   50 DO 60 I1 = 1 , NRLST
         LOCTYP(I1) = 2
   60 CONTINUE
      GOTO 200
!
!         Supply the desired statistics
!
  100 IERROR = 10
      GOTO 200
  110 IERROR = 11
      GOTO 200
  120 IERROR = 12
      GOTO 200
  130 IERROR = 13
!
!         Close the unit
!
  200 CLOSE ( lun )
      RETURN
      END
