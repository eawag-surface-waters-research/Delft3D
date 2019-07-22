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

!    Date:       4 Nov 1992
!    Time:       14:32
!    Program:    SETABC.FOR
!    Version:    1.0
!    Programmer: Hans Los
!    Previous version(s):
!    0.0 -- 26 Sep 1989 -- 14:15 -- Operating System: DOS
!
!    Update 1.0: Added DEP to parameter list
!              : Include new section to compute surface light intensity
!                and mixing depth fraction of bottom algae (Ulva)
!
!  *********************************************************************
!  *          SUBROUTINE TO SET MATRIX A AND B                         *
!  *********************************************************************
!
      SUBROUTINE SETABC(XINIT,EXTB,EXTTOT,CSOL,DSOL,T,DEP,ID,NSET)

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_arran   
      use bloom_data_matrix   
      use bloom_data_io  
      use bloom_data_phyt  
      use bloom_data_caldynam
      use bloom_data_putin

      implicit none

      SAVE
!      INCLUDE 'blmdim.inc'
!      INCLUDE 'putin1.inc'
!      INCLUDE 'size.inc'
!      INCLUDE 'phyt1.inc'
!      INCLUDE 'phyt2.inc'
!      INCLUDE 'cal1.inc'
!      INCLUDE 'arran.inc'
!      INCLUDE 'matri.inc'
!      INCLUDE 'dynam.inc'
!      INCLUDE 'ioblck.inc'

      real(8)  :: xinit(*),pmax20(mt),tcorr(mt),sdmixn(mt)
      real(8)  :: csol, dsol, dep, expmul, extb, exttot, t
      integer  :: k, j, nset, id, idrem,  idprev,  imprev
      
!      INTEGER SWBLSA
!
!  If this is the first time through the subroutine,
!  then initiate A, B and C
!

!jvb  , perform this every time for Ulva
!
      IDREM = IDUMP
      IDUMP = 0
      CALL MAXPRD(TEFCUR)
      DO 20 K = 1,NUSPEC
   20 PMAX20(K) = PMAX(K)
      CALL MAXPRD(T)
      IDUMP = IDREM
!
      IDREM = 0
      DO 30 K = 1, NUSPEC
      IF (SDMIX(K) .LT. 0.0) THEN
         SDMIXN(K) = 1.0D0 + SDMIX(K)
         DMIX(K) = DABS(SDMIX(K)) * DEP
         IDREM = 1
      ELSE
         SDMIXN(K) = 0.0D0
      END IF
30    CONTINUE
!jvb
      NSET = NSET + 1
      IF (NSET .GT. 1) GO TO 70
!
!  Initialize "C" values for all species to 1.0: maximize.
!  (See also subroutines SOLVLP and MAXGROGR)
!
      DO 10 J=1,NUSPEC
   10 C(J)=1.0
!
!  Initiate multiplier for exponential term at zero:
!  start with steady state solution for the dead algal pool
!
      EXPMUL=0.0
!
!  Find PMAX values at TEFCUR degrees to determine temperature
!  correction for the efficiency curves.
!
!     IDREM = IDUMP
!     IDUMP = 0
!     CALL MAXPRD(TEFCUR)
!     DO 20 K = 1,NUSPEC
!  20 PMAX20(K) = PMAX(K)
!     CALL MAXPRD(T)
!     IDUMP = IDREM
!
!  Update november 1992.
!  If a negative value of SDMIX was specified in the input, we are
!  dealing with a type that is only mixed over the lower fraction SDMIX
!  of the water column. Determine:
!  1. The fraction of the depth over which this type is NOT mixed
!     (SDMIXN). This is used to compute the "surface" light intensity.
!  2. The fraction of the depth over which this type DOES get mixed.
!  Note: DMIX has been already computed by RUN, but as we reset SDMIX
!  here we must also recompute DMIX for the initial time step.
!
!     IDREM = 0
!     DO 30 K = 1, NUSPEC
!     IF (SDMIX(K) .LT. 0.0) THEN
!        SDMIXN(K) = 1.0D0 + SDMIX(K)
!        DMIX(K) = DABS(SDMIX(K)) * DEP
!        IDREM = 1
!     ELSE
!        SDMIXN(K) = 0.0D0
!     END IF
!  30 CONTINUE
      IF (IDREM .EQ. 1) THEN
         WRITE (IOU(10), 99996) (DABS(SDMIX(K)), K = 1, NUSPEC)
         WRITE (IOU(10), 99995) (SDMIXN(K), K = 1, NUSPEC)
      END IF
!
!  Print warning message if a non-zero value is specified for the
!  sedimentation or flushing rate
!
      IF ( SEDRAT .LT. 1.0D-6) GO TO 60
      WRITE (IOU(10),99999) SEDRAT
   60 CONTINUE
      IF ( FLUSH .LT. 1.0D-6) GO TO 70
      WRITE (IOU(10),99998) FLUSH
   70 CONTINUE
!  Convert CSOL from:
!  Joules per cm2 per week to Joules per m2 per day.
!  Determine temperature correction, assuming that the nominal
!  efficiency curves are all for temperatures of TEFCUR deg. centigrade.
!
      DSOL=1428.57*CSOL
!
!  Determine the base level for the growth constraints (optionally).
!  If there is a discontinuity in the period numbers, EXTTOT and XINIT
!  are reinitialized.
!
      IF (NRUN .LE. 1) GO TO 90
      IF (IMU .EQ. 1) GO TO 90
      IDPREV = ID - MI
      IF (IDPREV .GE. NPER(IMU,1)) GO TO 90
      IMPREV = IMU - 1
      IF (IDPREV .GE. NPER(IMPREV,1) .AND.
     1    IDPREV .LE. NPER(IMPREV,2)) GO TO 90
      EXTTOT = EXTB
      IF (LGROCH .NE. 1) GO TO 90
      DO 80 J=1,NUECOG
   80 XINIT(J)=1.D+6
   90 CONTINUE
!
!  Compute equivalent radiation level.
!  Update November 1992.
!110  SURF(K)= TCORR(K) * DSOL
!  Multiply by the light reduction of overlying water columns. Usually
!  this factor is 1.0 as SDMIXN = 0.0; for types attached to the bottom
!  (Ulva) this factor is not 1.0, however.
!
      DO 100 K=1,NUSPEC
 100  TCORR(K) = PMAX20(K)/PMAX(K)
      DO 110 K=1,NUSPEC
 110  SURF(K)= TCORR(K) * DSOL * DEXP (- EXTTOT * SDMIXN(K) * DEP)
      IF (IDUMP .EQ. 1) WRITE (IOU(6),99997) (TCORR(K),K=1,NUSPEC)
!
!  Set "B" values for nutrients by substracting the amount in
!  zooplankton from the input values and correcting for deviations
!  from steady state if option DYNADEAD was selected
!
  170 DO 180 K=1,NUNUCO
        B(K)=CONCEN(K)    
  180 CONTINUE
      IF (LDYDEA .EQ. 0) RETURN
      EXPMUL=1.0
!
!  Formats for this subroutine
!
99999 FORMAT (//,1X,'* WARNING *   A sedimentation rate of',2X,F6.3,2X,
     1        'has been specified.',/,1X,'In order to keep the total',
     2        ' amount of nutrients constant, the program assumes',/,
     3        1X,'the amount of sedimented nutrients to be replaced',/,
     4        ' by dissolved nutrients from the bottom.',//)
99998 FORMAT (//,1X,'* WARNING *  A flushing rate of',2X,F6.3,2X,
     1        'has been specified.',/,1X,'In order to keep the total',
     2        ' amount of nutrients constant, the program assumes',/,
     3        1X,'the amount of nutrients flushed from the dead algal',
     4        ' pool',/,' to be replaced by dissolved nutrients',
     5        ' from the intake water.',//)
99997 FORMAT ('  Tcorr(j):  ',30(F5.2,1X))
99996 FORMAT (//,1X,'Computation with inhomogeneous mixing.',/,
     1        '  SDMIX(J):   ',30(F5.2,1X))
99995 FORMAT ('  SDMIXN(J):  ',30(F5.2,1X))
      RETURN
      END
