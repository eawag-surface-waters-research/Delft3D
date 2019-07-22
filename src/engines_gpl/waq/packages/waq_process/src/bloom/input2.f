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

!
!  *********************************************************************
!  *        SUBROUTINE INPUT2 TO READ UNIVERSAL INPUTS, CONTROL        *
!  *            WORDS AND THE INTEGRATED EFFICIENCY CURVES.            *
!  *********************************************************************
!
!  Lahey fortran PC version: use FORMATTED input for efficiency curves.
!
      SUBROUTINE INPUT2 (NDEC,INPU,INEFF)

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_arran   
      use bloom_data_io  
      use bloom_data_phyt    
      use bloom_data_putin   
      use bloom_data_sumou   
      use bloom_data_caldynam

      implicit none

!      INCLUDE 'blmdim.inc'
!      INCLUDE 'putin1.inc'
!      INCLUDE 'size.inc'
!      INCLUDE 'arran.inc'
!      INCLUDE 'phyt1.inc'
!      INCLUDE 'phyt2.inc'
!      INCLUDE 'cal1.inc'
!      INCLUDE 'ioblck.inc'
!      INCLUDE 'sumout.inc'
!
      character*60 aline

      CHARACTER*8 CUDATE(3)
      CHARACTER*8 CWORDS(8),BLANK,WTEMP,WBASE,WMULT,WINCR,PWORD
      CHARACTER*8 WRAD,WMODE,WSTOCH,WGROUP
      DATA NIMP/8/
      DATA CWORDS /'TEMPDEP ','NOMINAL ','MULTIPLI','INCREM  ',
     1             'DUMMY   ','TOTALRAD','EXPONENT','LINEAR  '/
      DATA BLANK /'        '/
      
      real(8)      :: zoonut(mn,0:mg), zook, zoogr, xmin, gramo1
      integer      :: i, ioff, ineff, inpu, j, k, nimp
      integer      :: nuadco, nunuc2, ndec
!
!  Equate CWORDS and CONTRO.
!
      DO 10 I = 1,NIMP
   10 CONTRO(I) = CWORDS(I)
!
!  General comment: if "NOMINAL" was entered as control word
!  for some parameter, the program will use the NOMINAL input,
!  whatever multiplier or increment was specified.
!
!
!  Initialize mode indicator IOFLAG to 0: default mode of program
!  is batch.
!
      IOFLAG = 0
      POSIT = -1
!
!  Read descriptive data for program control.
!  Read: NUSPEC--number of types; NUECOG--number of species;
!   NUNUCO--number of nutrient constraints; NUADCO--number of
!   additional constraints in the program; NPER(J,1)--number of
!   first week in a run; NPER(J,2)--number of last week in a run;
!   NPER(J,3)--interval step size between NPER(J,1) and NPER(J,2)
!   NRUN--number of successive runs.
!
      READ (INPU,99980) NUADCO
      IF (NUSPEC .GT. MT) THEN
         WRITE (OUUNI,99985) NUSPEC,MT
         CALL SRSTOP(6)
      END IF
      IF (NUECOG .GT. MS) THEN
         WRITE (OUUNI,99983) NUECOG,MS
         CALL SRSTOP(6)
      END IF
      IF (NUNUCO .GT. MN) THEN
         WRITE (OUUNI,99981) NUNUCO,MN
         CALL SRSTOP(6)
      END IF
      READ (INPU,99987) NPER(1,1),NPER(1,2),NPER(1,3),NRUN
      IF (NRUN .EQ. 0) NRUN = 1
      IF (NRUN .GT. 1) THEN
         DO 20 J=2,NRUN
   20    READ (INPU,99987) NPER(J,1),NPER(J,2),NPER(J,3)
      END IF
!
!  Establish various column and row indicators for A-matrix and output
!  output vector X (or XDEF).
!  It is assumed there are 2 energy constraints.
!
!       NUFILI--number of first light constaint.
!       NUABCO--number of abiotic constaints (total).
!       NUEXRO--number of the exclusion row in A-matrix.
!       NUROWS--number of rows in A-matrix.
!       NUCOLS--number of elements in X-vector.
!       NUSPE1--position of first type in X-vector.
!
      NUFILI=NUNUCO+1
      NUABCO=NUNUCO+2
      NUEXRO=NUABCO+NUADCO
      NUROWS=NUEXRO
      NUSPE1=NUROWS+1
      NUCOLS=NUROWS+NUSPEC

!  Read names of nutrient constraints, temperature (in)dependence,
!  and whether the input value is to be used or some externally
!  specified modification.
!
      NUNUC2 = NUNUCO
      IF (NUNUCO.GT.3) NUNUCO = 3

      DO 40 J=1,NUNUCO
      READ (INPU,99999) CSTRA(J),WTEMP,RNUT(1,J),WBASE,WMULT,BNUT(J),
     1                  WINCR,DNUT(J)
      IF (WTEMP .NE. CONTRO(1)) THEN
         RNUT(2,J)=0.0
      ELSE
         RNUT(2,J)=1.0
      END IF
      IF (WBASE .EQ. CONTRO(2)) THEN
         BNUT(J)=1.0
         DNUT(J)=0.0
      END IF
   40 CONTINUE
!
!  Read control parameters for the background extinction.
!
      READ (INPU,99998) WBASE,WMULT,BACKMU,WINCR,BACKAD
      IF (WBASE .EQ. CONTRO(2)) THEN
         BACKMU=1.0
         BACKAD=0.0
      END IF
!
!  Read control parameters for the temperature.
!
      READ (INPU,99998) WBASE,WMULT,TEMPMU,WINCR,TEMPAD
      IF (WBASE .EQ. CONTRO(2)) THEN
         TEMPMU=1.0
         TEMPAD=0.0
      END IF
!
!  Read control parameters for the solarintensity.
!
      READ (INPU,99997) WRAD,WBASE,WMULT,SOLAMU,WINCR,SOLAAD
      IF (WRAD .EQ. CONTRO(6)) THEN
         SOLACO=0.45
      ELSE
         SOLACO=1.0
      END IF
      IF (WBASE .EQ. CONTRO(2)) THEN
         SOLAMU=1.0
         SOLAAD=0.0
      END IF
!
!  Read control parameters for the depth.
!
      READ (INPU,99998) WBASE,WMULT,DEPTMU,WINCR,DEPTAD
      IF (WBASE .EQ. CONTRO(2)) THEN
         DEPTMU=1.0
         DEPTAD=0.0
      END IF
!
!  Set control parameters for daylength.
!
      DLGTAD=0.0
      DLGTMU=1.0
!
!  Read mineralization rate constant of organic material.
!  Temperature dependence is assumed.
!
      READ (INPU,99988) REMIOR
!
!  Read names of light constraints and the constants to calculate
!  the disappearance rate of light absorption by dead phytoplankton.
!
      READ (INPU,99996) (CSTRA(J),J=NUFILI,NUABCO)
!
!  Read natural mortality rate constants:
!  Only default option is a species dependent function.
!
      READ (INPU,99995) WMODE,FLUSH

!
!  Read zooplankton composition.
!  Read grazing coefficients.
!  Read maximum number of grazing iterations.
!
!  If the maximum number of iterations is put to 1, the program
!  will supress the output on unit 15.
!
      READ (INPU,99994) (ZOONUT(I,0),I=1,NUNUCO)
      READ (INPU,99993) ZOOK,ZOOGR,XMIN,GRAMO1
!
!  Read fraction of nutrients from dying phytoplankton, which is release
!  instantaneously at autolysis and does not enter the dead algal pool.
!  Read sedimentation rate of dead algae.
!
      READ (INPU,99992) SEDRAT
      DO 150 I=1,NUSPEC
!
! Hans Los: Why? Origin: Marinus sources
!       SDMIX(I) = 1.0
        ZOOPR(I,0) = 1.0
  150 CONTINUE
!
!  Read data for the integrated photosynthetic efficiency curves
!  from unit 12. These data are produced by the efficiency program
!  "BLEFFPRO" and transformed for unformatted read.
!
!     READ (INEFF) NZ,TEFCUR,(ZVEC(I),I=1,NZ),NZ
!     DO 160 I=1,NZ
!     READ (INEFF) (FUN(I,J),J=1,NUECOG)
! 160 READ (INEFF) (DER(I,J),J=1,NUECOG)
!     DO 170 I=1,24
! 170 READ (INEFF) DL(I),(DAYMUL(I,J),J=1,NUECOG)
!
!----------------------------------------------------------------------
!  Input section for FORMATTED read of efficiency curves!
!
      VERFRM = 1.0
      READ ( INEFF    , '(A)' ) ALINE
      IOFF =  INDEX(ALINE, 'BLOOMFRM_VERSION')
      IF(IOFF.EQ.0) THEN
         REWIND( INEFF )
      ELSE
         READ (ALINE(IOFF+17:IOFF+20),*) VERFRM
         READ (INEFF,199) (GRNAME(J),J=1,NUECOG)
         READ (INEFF,199) (SPNAME(I),I=1,NUSPEC)
         IF(VERFRM.GT.2.00) THEN
            READ (INEFF,*) NPOINT
            DO I=1,NPOINT
               READ (INEFF,*) POWER(I), (EFFIC(I,J),J=1,NUECOG)
            END DO
         ENDIF
      ENDIF
      IF(VERFRM.GT.2.00) THEN
         READ (INEFF,200) NZ,TEFCUR
         DO I=1,NZ
            READ (INEFF,210) ZVEC(I), (FUN(I,J),J=1,NUECOG)
         ENDDO
         READ (INEFF,200) NZ,TEFCUR
         DO I=1,NZ
            READ (INEFF,210) ZVEC(I), (DER(I,J),J=1,NUECOG)
         ENDDO 
      ELSE
         READ (INEFF,200) NZ,TEFCUR
         READ (INEFF,210) (ZVEC(I),I=1,NZ)
         READ (INEFF,200) NZ
         DO 220 I=1,NZ
         READ (INEFF,210) (FUN(I,J),J=1,NUECOG)
  220    READ (INEFF,210) (DER(I,J),J=1,NUECOG)
      ENDIF
  199 FORMAT (30(A10,X))
  200 FORMAT (I5,5X,F10.2)
  210 FORMAT (10(D15.8,3X))
      DO 230 I=1,24
  230 READ (INEFF,240) DL(I),(DAYMUL(I,J),J=1,NUECOG)
  240 FORMAT (11F5.0)
!----------------------------------------------------------------------
      CASE(10) = BLANK
      CASE(11) = BLANK
      CASE(12) = BLANK
      CASE(13) = BLANK
!
!  Record names of limiting factors.
!
      DO 250 I = 1,NUNUCO
      LIMNAM (I) = CSTRA (I) (1:3)
250   CONTINUE

      NUNUCO = NUNUC2

      LIMNAM (NUNUCO+1) = 'E  '
      LIMNAM (NUNUCO+2) = 'Gro'
      LIMNAM (NUNUCO+3) = 'Mor'
!
!  Formats.
!
99999 FORMAT (A8,7X,A8,F7.0,A8,7X,2(A8,F7.0))
99998 FORMAT (30X,A8,7X,2(A8,F7.0))
99997 FORMAT (15X,2(A8,7X),2(A8,F7.0))
99996 FORMAT (2(A8,7X),2(8X,F7.0))
99995 FORMAT (15X,A8,15X,F7.0)
99994 FORMAT (23X,F7.0,5(8X,F7.0))
99993 FORMAT (4(8X,F7.0),8X,I4)
99992 FORMAT (23X,F7.0)
99991 FORMAT (A8)
99990 FORMAT (A8,2X,D10.0,8F10.0)
99989 FORMAT (8X,2F8.0,A8,6F8.0)
99988 FORMAT (23X,F7.0)
99987 FORMAT (5(8X,I5,2X))
99986 FORMAT ('  Unrecognised control word ',A8,' for mortality',
     1        ' computation.', /'  Excecution terminates.')
99985 FORMAT ('  The number of types ',I2,' exceeds the maximum ',
     1        I2,' set by the program.',/'  Excecution terminates.')
99984 FORMAT ('  Unrecognised control word ',A8,' for Pmax',
     1        ' computation.', /'  Excecution terminates.')
99983 FORMAT ('  The number of species ',I2,' exceeds the maximum ',
     1        I2,' set by the program.',/'  Excecution terminates.')
99981 FORMAT ('  The number of nutrients ',I2,' exceeds the maximum ',
     1        I2,' set by the program.',/'  Excecution terminates.')
99980 FORMAT (53X,I5)
99979 FORMAT ('  The number of grazers ',I2,' exceeds the maximum ',
     1        I2,' set by the program.',/'  Excecution terminates.')
99978 FORMAT (2(8X,F7.0))
      RETURN
      END
