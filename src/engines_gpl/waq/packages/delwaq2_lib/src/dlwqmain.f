!!  Copyright (C)  Stichting Deltares, 2012-2013.
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

C
C            DELWAQ - Deltares WAter Quality program
C
C                     Version 4.xx - june 2009 - DLL version
C                     Version 4.33 - july 1998
C                     Version 4.32 - june 1998
C
C     INFORMATION   : Deltares
C                     L. Postma,
C                     Rotterdamseweg 185,
C                     P.O. Box 177,
C                     2600 MH Delft,
C                     Netherlands.
C                     telephone (31) 88-3358273
C                     telefax   (31) 88-3358582
C
C     FUNCTION            : MAIN module for DELWAQ2 , dimensioning
C                           of the work array's.
C
C     SUBROUTINES CALLED  : DELWQ2, performs the simulation
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ACTION  INTEGER  1          INPUT   Action to be taken
C     ARGC    INTEGER  1          INPUT   Number of simulated command-line arguments
C     ARGV    INTEGER  1          INPUT   Simulated command-line arguments
C
C     ITOTA   INTEGER  1          INPUT   length of real workarray
C     ITOTI   INTEGER  1          INPUT   length of integer workarray
C     ITOTC   INTEGER  1          INPUT   length of character workarray
C
C
C      PARAMETER (ITOTA=0       ,ITOTI=0       ,ITOTC=0       )

      subroutine delwaq2( argc, argv, errorcode )
      !DEC$ ATTRIBUTES DLLEXPORT::delwaq2

      use delwaq2_data
      implicit none
      include 'actions.inc'
    
      integer, intent(in)                           :: argc
      character(len=*), dimension(argc), intent(in) :: argv
      integer, intent(out)                          :: errorcode

      type(delwaq_data)                             :: dlwqd

      dlwqd%set_timer = .true.

      call dlwqmain( ACTION_FULLCOMPUTATION, argc, argv, dlwqd )

      ! Delwaq2_lib should never use a stop, but must be modified to return an error code instead (0 = normal end)
      ! Currently a return from the delwaq2_lib assumes a normal end.
      errorcode = 0
      return

      end subroutine delwaq2


      subroutine dlwqmain(ACTION, ARGC, ARGV, DLWQD)

      !DEC$ ATTRIBUTES DLLEXPORT::dlwqmain

      USE DELWAQ2
      USE DELWAQ2_DATA
      USE DHCOMMAND

      IMPLICIT NONE

      INTEGER, INTENT(IN)                           :: ACTION
      INTEGER, INTENT(IN)                           :: ARGC
      CHARACTER(LEN=*), DIMENSION(ARGC), INTENT(IN) :: ARGV
      TYPE(DELWAQ_DATA)                             :: DLWQD

      CHARACTER*20  RUNDAT
C
      LOGICAL                                       :: INIT        ! Do not save!
      INTEGER                                       :: LUNREP

      INTEGER, SAVE                                 :: ITOTA
      INTEGER, SAVE                                 :: ITOTI
      INTEGER, SAVE                                 :: ITOTC

      INCLUDE 'sysn.inc'
      INCLUDE 'sysi.inc'
      INCLUDE 'actions.inc'
      INCLUDE 'fsm-fix.i'

C
C     Initial step ...
C
      INIT = .FALSE.
      IF ( ACTION == ACTION_INITIALISATION  .OR.
     &     ACTION == ACTION_FULLCOMPUTATION      ) THEN
          INIT = .TRUE.

          call dhstore_command( argv )

          CALL AVUNDF
C
          ITOTA=0
          ITOTI=0
          ITOTC=0

          NULLIFY( DLWQD%RBUF  )
          NULLIFY( DLWQD%IBUF  )
          NULLIFY( DLWQD%CHBUF )

          ALLOCATE( DLWQD%RBUF(0) )
          ALLOCATE( DLWQD%IBUF(0) )
          ALLOCATE( DLWQD%CHBUF(0) )

      ENDIF

!     Computation step is always done

      CALL DELWQ2 ( DLWQD%RBUF, DLWQD%IBUF, DLWQD%CHBUF, ITOTA, ITOTI,
     &              ITOTC, INIT, ACTION, DLWQD )

C
C     Finalise - only if the full computation was done
C
      IF ( ACTION == ACTION_FULLCOMPUTATION      ) THEN

          CALL GETMLU(LUNREP)
          WRITE ( * , * )
          WRITE ( * , * ) ' SIMULATION ENDED '
          WRITE ( LUNREP , * )
          WRITE ( LUNREP , '(A)' ) ' Simulation ended normal'
          CALL DATTIM(RUNDAT)
          WRITE (LUNREP,'(2A)') ' Execution stop : ',RUNDAT
C
          close(lunrep)

          RETURN

      ENDIF

      IF ( ACTION == ACTION_FINALISATION      ) THEN

          CALL GETMLU(LUNREP)
          WRITE ( * , * )
          WRITE ( * , * ) ' SIMULATION ENDED '
          WRITE ( * , * )
          WRITE ( LUNREP , * )
          WRITE ( LUNREP , '(A)' ) ' Simulation ended normal'
          CALL DATTIM(RUNDAT)
          WRITE (LUNREP,'(2A)') ' Execution stop : ',RUNDAT
C
          close(lunrep)

      ENDIF


      RETURN
      END
