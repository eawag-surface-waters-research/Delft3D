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
      module m_rd_tabp3

      implicit none

      contains


      SUBROUTINE RD_TABP3 ( DEFFDS      ,
     +                      NO_FORT_MAX , NO_FORT     ,
     +                      FORT_ID     , LUNREP      ,
     +                      IERROR      )
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_P3 group from NEFIS file
!
!     FILES              :  NEFIS file assumed opened
!
!     SUBROUTINES CALLED :
!
!     ARGUMENTS
!
!     NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     DEFFDS       INT    2993    I/O     Definition file descriptor
!     DATFDS       INT    999     I/O     Data file descriptor
!     NO_FORT_MAX  INT            I       Maximum number of FORTRAN process modules
!     NO_FORT      INT            I       number of FORTRAN process modules
!     FORT_ID      CHA*10 NO_FORT I       unique module identification
!     LUNREP       INT    1       I       Unit number report file
!     IERROR       INT    1       O       Error
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      IMPLICIT NONE
      SAVE
!
!     declaration of arguments
!
      INTEGER       NO_FORT_MAX , NO_FORT     ,
     +              LUNREP      , IERROR
      INTEGER       DEFFDS
      CHARACTER*10  FORT_ID     (NO_FORT_MAX)
!
!     Local variables
!
!     GRPNAM  CHAR*16     1       LOCAL   group name (table)
!     NELEMS  INTEGER     1       LOCAL   number of elements in group (=cell)
!     ELMNMS  CHAR*16  NELEMS     LOCAL   name of elements on file
!     ELMTPS  CHAR*16  NELEMS     LOCAL   type of elements
!     ELMDMS  INTEGER  6,NELEMS   LOCAL   dimension of elements
!     NBYTSG  INTEGER  NELEMS     LOCAL   length of elements (bytes)
!
      INTEGER       NELEMS
      PARAMETER   ( NELEMS = 2 )
!
      INTEGER       I               , IELM          ,
     +              BUFLEN
      INTEGER       ELMDMS(2,NELEMS), NBYTSG(NELEMS),
     +              UINDEX(3)
      CHARACTER*16  GRPNAM
      CHARACTER*16  ELMNMS(NELEMS)  , ELMTPS(NELEMS)
      CHARACTER*64  ELMDES(NELEMS)
!
!     External NEFIS Functions
!
      INTEGER   GETELS
     +         ,GETELT
      EXTERNAL  GETELS
     +         ,GETELT
!
!     element names
!
      DATA  GRPNAM  /'TABLE_P3'/
      DATA
     + (ELMNMS(I),ELMTPS(I),NBYTSG(I),ELMDMS(1,I),ELMDMS(2,I),ELMDES(I),
     +  I = 1 , NELEMS)
     +/'NO_FORT','INTEGER'  , 4,1,1,'number of FORTRAN process modules',
     + 'FORT_ID','CHARACTER',10,1,0,'unique module identification'     /
!
!     Now the reading part
!

      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1

      BUFLEN = NBYTSG(1)*ELMDMS(2,1)
      IERROR = GETELT (DEFFDS ,
     +                 GRPNAM , ELMNMS(1),
     +                 UINDEX , 1        ,
     +                 BUFLEN , NO_FORT  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(1)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
      IF ( NO_FORT .GT. NO_FORT_MAX ) THEN
         WRITE(LUNREP,*) 'ERROR reading group',GRPNAM
         WRITE(LUNREP,*) 'Actual number of process modules:',NO_FORT
         WRITE(LUNREP,*) 'greater than maximum:',NO_FORT_MAX
         IERROR = 1
         GOTO 900
      ENDIF
!
!     Set dimension of table
!
      DO IELM = 2 , NELEMS
         ELMDMS(2,IELM) = NO_FORT
      ENDDO

      BUFLEN = NBYTSG(2)*ELMDMS(2,2)
      IERROR = GETELS (DEFFDS ,
     +                 GRPNAM , ELMNMS(2),
     +                 UINDEX , 1        ,
     +                 BUFLEN , FORT_ID  )
      IF ( IERROR .NE. 0 ) THEN
         WRITE(LUNREP,*) 'ERROR reading element',ELMNMS(2)
         WRITE(LUNREP,*) 'ERROR number:',IERROR
         GOTO 900
      ENDIF
!
  900 CONTINUE
      RETURN
!
      END

      end module m_rd_tabp3
