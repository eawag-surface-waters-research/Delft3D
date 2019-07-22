!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

!    Module to read BLOOM input files
!
!    Called by: BLOOMC
!    Calls    : INPUT2, OPTION, CHANGE
!
      subroutine blinpu (ntyp_m, ntyp_a, ngro_a, algtyp, lmixo , lfixn ,
     j                   lcarb , nunucom, nutcon, flxcon, con2out)
      
      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_arran   
      use bloom_data_mass_balance  
      use bloom_data_caldynam
      use bloom_data_io  
      use bloom_data_matrix  
      use bloom_data_phyt    
      use bloom_data_putin   
      use bloom_data_sumou   

      implicit none

!     Arguments
!
!     131011  Jos van Gils    optional C limitation
!     971217  Marnix vd Vat   MrtExAlg added
!
!     Name    Type  Length   I/O  Description
!
!     NTYP_A  I     1        O    Actual number of types
!     NGRO_A  I     1        O    Actual number of groups
!     ALGTYP  R   0:20,*     O    Characteristics per algae type
!     LMIXO   L     1        O    Flag mixotrophy
!     LFIXN   L     1        O    Flag N-fixation
!     LCARB   L     1        I    Flag carbon limitation
!     NUTCON  I*4   8        O    Nutrients involved in active nutrient constraints
!     FLXCON  I*4   8        O    Uptake fluxes involved in active nutrient constraints

      INTEGER      NTYP_M, NTYP_A, NGRO_A, NUNUCOM
      INTEGER      J, K, IS
      REAL         ALGTYP(0:20,NTYP_M)
      LOGICAL      LMIXO,LFIXN,LCARB
      INTEGER      NUTCON(NUNUCOM), FLXCON(NUNUCOM), CON2OUT(NUNUCOM)
!
!     Local variables
!
!     Name    Type  Length   I/O  Description
!
!     NDEC    I     1             Dummy in coupled version
!     LPARAM  I     1
!     I       I     1

      INTEGER      NDEC  , LPARAM, I
      REAL         AUTOFR
!
!  Read title lines of BLOOM II input file.
!  Note: In the standalone BLOOM II version these comments are read by
!  INPUT1.
!
      READ (INUNI, '(I4,1X,9A8)') IYEAR, (CASE (I), I = 1,9)
      READ (INUNI, '(9A8,8X)') COM

!  DETERMINE NUSPEC AND NUECOG
      IS = 0
      NUECOG = 0
   60   IS = IS + 1
        IF ((ALGTYP(0,IS).GT.-100.).AND.(IS.LE.NTYP_M)) THEN
          IF (IS.EQ.1) THEN
            J=1
            IT2(1,1)=1
          ELSEIF (IS.EQ.NTYP_M) THEN
            IT2(J,2) = NTYP_M
          ELSEIF (NINT(ALGTYP(1,IS)).NE.NINT(ALGTYP(1,IS-1))) THEN
            IT2(J,2) = IS-1
            J = J + 1
            IT2(J,1) = IS
          ENDIF
          IT2(J,2) = IS
          NUECOG = MAX(NUECOG,NINT(ALGTYP(1,IS)))
          GOTO 60
        ENDIF

      NUECOG = J
      NUSPEC = IS - 1
      IF ((IS.EQ.NTYP_M).AND.(ALGTYP(0,NTYP_M).GT.-100.)) NUSPEC =NTYP_M

!  SET THE ALGAE CHARACTERISTICS
! 
      LMIXO = .FALSE.
      LFIXN = .FALSE.
      DO 70 J=1,NUECOG
        GRNAME(J)(1:1) = CHAR(ICHAR('A')+J-1)
        K = 0
        DO 80 I=IT2(J,1),IT2(J,2)
          K = K + 1
          SPNAME(I)(1:1) = CHAR(ICHAR('A')+J-1)
          WRITE(SPNAME(I)(3:3),'(I1)') K
          CTODRY(I) = ALGTYP(3,I)
          EKX(I)    = ALGTYP(2,I) * 0.001 / CTODRY(I)
          IF (ALGTYP(16,I).GT.0.0) LMIXO = .TRUE.
          IF (ALGTYP(17,I).GT.0.0) LMIXO = .TRUE.
          IF (ALGTYP(18,I).GT.0.0) LFIXN = .TRUE.
          CHLTOC(I) = 1./ ALGTYP(7,I)
          CHLR(I)   = CHLTOC(I)*CTODRY(I)
          PMAX1(I)  = ALGTYP(8,I)
          PMAX2(I)  = ALGTYP(9,I)
          IF (NINT(ALGTYP(10,I)).EQ.0) THEN
            LPMAX(I) = 1
          ELSE
            LPMAX(I) = 0
          ENDIF
          RMORT1(I) = ALGTYP(11,I)
          RMORT2(I) = ALGTYP(12,I)
          RMORT3(I) = ALGTYP(20,I)
          RES1(I)   = ALGTYP(13,I)
          RES2(I)   = ALGTYP(14,I)
          SDMIX(I)  = ALGTYP(19,I)
          AUTOFR = ALGTYP(15,I)
          AVAILN(I) = DBLE(1.D0 - AUTOFR)
   80   CONTINUE
   70 CONTINUE

!     Set admin dependent on NUNUCO
!     Note that we handle different sets of nutrient constraints
!      - optional carbon limitation (LCARB) 
!      - mixotrophy (N,P) (LMIXO)
!      - N-fixation (LFIXN)

      DO I=1,NUSPEC
        AA(1,I)   = ALGTYP(4,I) / CTODRY(I)
        AA(2,I)   = ALGTYP(5,I) / CTODRY(I)
        AA(3,I)   = ALGTYP(6,I) / CTODRY(I)
        IF (LCARB) AA(4,I)   = 1. / CTODRY(I)
      ENDDO
      NUTCON (1) = 1
      NUTCON (2) = 2
      NUTCON (3) = 3
      FLXCON (1) = 2  ! NH4 uptake
      FLXCON (2) = 4  ! PO4 uptake
      FLXCON (3) = 5  ! Si uptake
      CON2OUT(1) = 1
      CON2OUT(2) = 2
      CON2OUT(3) = 3
      NUNUCO = 3
      IF (LCARB) THEN
        NUTCON (NUNUCO+1) = 4
        FLXCON (NUNUCO+1) = 1  ! C uptake
        CON2OUT(NUNUCO+1) = 4
        NUNUCO = 4
      ENDIF
      IF (LMIXO) THEN
        DO I=1,NUSPEC
          AA(NUNUCO+1,I) = MAX(0.0,ALGTYP(16,I) / CTODRY(I))
          AA(NUNUCO+2,I) = MAX(0.0,ALGTYP(17,I) / CTODRY(I))
        ENDDO
        CSTRA(NUNUCO+1) = 'N-Detr'
        LIMNAM(NUNUCO+1) = 'N-D'
        CSTRA(NUNUCO+2) = 'P-Detr'
        LIMNAM(NUNUCO+2) = 'P-D'
        NUTCON (NUNUCO+1) = 1
        NUTCON (NUNUCO+2) = 2
        FLXCON (NUNUCO+1) = 6  ! DetN uptake
        FLXCON (NUNUCO+2) = 7  ! DetP uptake
        CON2OUT(NUNUCO+1) = 5
        CON2OUT(NUNUCO+2) = 6
        NUNUCO = NUNUCO + 2
      ENDIF
      IF (LFIXN) THEN
        DO I=1,NUSPEC
          AA(NUNUCO+1,I) = MAX(0.0,ALGTYP(18,I) / CTODRY(I))
        ENDDO
        CSTRA(NUNUCO+1) = 'N-Fix'
        LIMNAM(NUNUCO+1) = 'N-F'
        NUTCON (NUNUCO+1) = 1
        FLXCON (NUNUCO+1) = 8  ! NFix
        CON2OUT(NUNUCO+1) = 7
        NUNUCO = NUNUCO + 1
      ENDIF
      IF (NUNUCO.GT.NUNUCOM) GOTO 901
!
!  Call subroutine INPUT2 to read BLOOM specific data for
!  species, constraints, stochiometry etc.
!
      NDEC = 0
      CALL INPUT2 (NDEC,INUNI,IOU(12),1)
!
!  Close the efficiency file.
!
      CLOSE (IOU(12))
!
!  Set various counters used in several routines of BLOOM II.
!  NREP   = counter for number of calls to all main BLOOM II routines.
!  NPRINT = counter for print routines.
!  NPRODU = counter for BLOOM II production routines (which are NOT
!           used here).
!  LPRINT = flag indicating whether normal BLOOM II output routines
!           are called (LPRINT = 1) or not (LPRINT = 0).
!  MI     = number of time periods considered in one computation step of
!           BLOOM II.
!
      NREP   = 0
      NPRINT = 0
      NPRODU = 0
      LPRINT = 1
      MI     = NPER (1,3)
      IMU    = 1
!
!  Call subroutine "OPTION" to read options for program control.
!  If "RUN" was not specified or if the program has detected
!  an error, it will terminate.
!
      CALL OPTION (0,LPARAM)
      IF (LPARAM. EQ. 1) CALL CHANGE(1)
      CLOSE (IOU(9))
      IF (NUSPEC .GT. MT) GOTO 901
      IF (NUNUCO .GT. MN) GOTO 901
      IF (LRUN .EQ. 0) THEN
         WRITE (OUUNI,40)
   40    FORMAT (1X,'No "RUN" command or a fatal error was detected; ',
     1           'execution terminates',/)
         GOTO 902
      ENDIF

!     Pass actual number of groups and species to main program

      NTYP_A = NUSPEC
      NGRO_A = NUECOG

   50 CONTINUE

      RETURN

!     Maximum number permitted species exceeded
!     Present program version can only handle MT phytoplankton species.
  901 WRITE(*,*) 'Fatal error 901 in BLINPU'
      CALL SRSTOP(1)

!     No "RUN" command or a fatal error was detected,
!     execution terminates
  902 WRITE(*,*) 'Fatal error 902 in BLINPU'
      CALL SRSTOP(1)

      END
