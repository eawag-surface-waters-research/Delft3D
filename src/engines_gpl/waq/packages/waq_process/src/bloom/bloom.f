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

!    Date:       31 Dec 1999
!    Time:       11:52
!    Program:    BLOOM.FOR
!    Version:    2.0
!    Programmer: Hans Los
!    Previous version(s):
!    2.00 -- 13 Apr 2016 -- 12:55 -- Operating System: DOS
!    1.82 -- 30 Dec 1999 -- 08:36 -- Operating System: DOS
!    1.81 -- 12 Feb 1993 -- 08:33 -- Operating System: DOS
!    1.8 -- 11 Feb 1993 -- 14:08 -- Operating System: DOS
!    1.7 -- 4 Nov 1992 -- 14:17 -- Operating System: DOS
!    1.6 -- 26 Feb 1992 -- 19:09 -- Operating System: DOS
!    1.5 -- 26 Dec 1991 -- 19:57 -- Operating System: DOS
!    1.4 -- 7 May 1991 -- 13:30 -- Operating System: DOS
!    1.3 -- 21 Jun 1990 -- 14:14 -- Operating System: DOS
!    1.2 -- 9 Jan 1990 -- 08:33 -- Operating System: DOS
!    1.1 -- 9 Jan 1990 -- 08:08 -- Operating System: DOS
!    1.0 -- 2 Jan 1990 -- 08:05 -- Operating System: DOS
!    BLOOM FORTRAN -- 20 Dec 1989 -- 07:47 -- Operating System: DOS
!    BLOOM.FOR  -- 22 Nov 1989 -- 13:03 -- Operating System: CMS
!    BLOOM FORTRAN -- 20 Nov 1989 -- 14:57 -- Operating System: DOS
!    BLOOM FORTRAN -- 26 Oct 1989 -- 10:20
!    1.0 -- 26 Oct 1989 -- 07:52
!    0.0 -- 26 Oct 1989 --  7:52
!    and many, many others!
!
!    Update 2.0 - TT/HL adjusted coupled model to emulate stand-alone version.
!                 In case SWBLSA=1 a steady state is calculated on basis of
!                 predescribed nutrients and the detritus concentration from
!                 previous time step. Main changes are made in blprim.f
!                 Also affected are blprim.f, dynrun.f, fixinf.f, setabc.f
!               - TT/HL Cleaned up bloom.f by removing all CONSB2 related
!                 statements and print statements triggered by LPRINT>0
!                 Also cleaned up were bloutc.f, dynrun.f, input2.f, 
!                 maxgro.f, maxprd.f, natmor.f, ophelp.f, option.f, 
!                 prinma.f, prinsu.f, print6.f, promes.f, setabc.f

!    Update 1.82: Added LCOUPL flag to if statement before call to
!                 MAXMOR:this makes it possible to maitain a single
!                 version on all platforms!
!                 Added conditional write statements for N fixation
!                 and or mixotrophy (NUNUCO > 3). Although not very
!                 elegant, the same code can be used for all possible
!                 cases.
!
!    0895 MvdV extended for multiple grazer types, subroutine CONSBL added
!              CONSBL is called if NUGRAZ > 0
!              extension of the dimension of ZOOD for multiple grazers
!              addition of ZOOC, GRADET, GDTOLD
!
!    Update 1.81: Solved the actual cause of problem at 1.8. Error in
!                 PRINT6. This module needs both INOW and J so changed
!                 the header.
!
!    Update 1.8: Use logical LSOLU to determine if at least one feasible
!                solution exists. Previous criterium SOMETIMES failed
!                in a situation of photo-inhibition.
!
!    Update 1.7: Added DEP to Calls of SETABC, MAXGRO and MAXGRA.
!
!    Update 1.6: Changed call to FIXINF.

!    Update 1.5: Alternative optimization scheme.
!       All types of a species with the same KMAX, but with different
!       objective function.
!       Modified modules:
!       BLOOM.FOR
!       MAXGRO.FOR
!    Update 1.4: Distinguish LPRINT = 0, 1, or 2 (See RUN).
!    Update 1.3: Exit as soon as possible if solar radiation is below
!    100 J/m2/week -- speed of ECOLUMN - BLOOM II computations for
!    for segments 2 and 3.
!    Update 1.2: INT initialized at 0; initialization of NIN moved to
!    section included EACH run through the subroutine.
!    Update 1.1: CSOL = DSOL/10000 * DAY set upon exit of the routine to
!    enable calling programs using the same, corrected solar intensity
!    level as BLOOM II.
!
!  *********************************************************************
!  *    SUBROUTINE FOR SETTING UP AND SOLVING BLOOM MODEL PROBLEM      *
!  *********************************************************************
!
!
      SUBROUTINE BLOOM(CDATE,ID,MI,T,CSOL,PHYT,EXTB,DAY,DEATH,ZOOD,
     1           DEP,XINIT,XDEF,XECO,TOTAL,EXTTOT,EXTLIM,NSET,INFEAS,
     2           NONUN,NUMUN,LCOUPL,SWBLSA)

      USE DATA_3DL

      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'ioblck.inc'
      INTEGER IRS3, SWBLSA
      INTEGER NONUNI(MT),NONUN(MT),IRS(3),LIB(MX),JKMAX(MS)
      SAVE    IRS, IRS3
      DIMENSION X(MX),XINIT(*),XDEF(*),BIO(2),GROOT(2),
     1          OROOT(2*MT),ROOT(2),EMIN(MT),
     2          OUT15(20+MG),OUTST(20+MG),XECO(*),XECOST(MS),ZOOD(0:MG)
      CHARACTER*8 CDATE
*     CHARACTER*4 COUT(10),COUTST(10)
      CHARACTER*4 COUT(MN+5),COUTST(MN+5)
      CHARACTER*1 ERRIND
      LOGICAL LSOLU
      PARAMETER (SOLMIN=100.0)

!
!
!  Calculate maximum primary production and respiration rates.
!
      CALL MAXPRD(T)
!
!  In the stand-alone version of BLOOM II
!  compute the total extinction (EXTTOT) as:
!
!   EXTTOT = EXTB + EXLIVE + EXDEAD
!
!  to account for eventual variations in the background extinction.
!  Set A-matrix and B and C vectors; inititate detritus pools (optional)
!  In coupled model versions EXTTOT is updated by the calling program.
!
!
      IF (NREP .NE. 1 .AND. LCOUPL .EQ. 0) EXTTOT = EXTB+EXLIVE+EXDEAD
!
!  Update 900109:
!  Make a copy of the (unconverted) solar radiation level. PRINUN needs
!  this value to replicate the boundary conditions.
!  Update 28 oct 92: added DEP to argument list.
!
      USOL = CSOL
      CALL SETABC(XINIT,EXTB,EXTTOT,ZOOD,CSOL,DSOL,T,DEP,ID,NSET)
!
!   Test for (in)feasibility of the nutrient constraints in
!   a run with a dynamic detritus computation.
!
      IF (LDYDEA .EQ. 0) GO TO 10
      CALL NUTFEA (INFEAS)
      IF (INFEAS .EQ. 1) THEN
         IRERUN = 3
         GO TO 270
      END IF
   10 CONTINUE
!
! Compute the euphotic depth and the euphotic day lenght if option
! "DAYEUPHO" is on.
!
      IF (LDAYEU .EQ. 1) THEN
         CALL DAYEU (DAY,DAYEUF,EXTTOT,DEP,DEPEUF,DSOL,EULIGH,IDUMP)
         DO 75 I = 1,NUSPEC
         DMIX(I) = DABS(SDMIX(I)) * DEPEUF
   75    CONTINUE
      ELSE
         DAYEUF = DAY
         DEPEUF = DEP
      END IF
!
! If the light intensity is very low, declare the problem 'infeasible',
! don't rerun, only write output. But do not do this for 3DL approach the
! efficiency can be obtained in other layers with light.
!
      IF (CSOL .LT. SOLMIN .AND. .NOT. ACTIVE_3DL .AND. .NOT. ACTIVE_EFFT) THEN
         IRERUN = 0
         ERRIND = ' '
         NI = 0
         GO TO 205
      END IF
!
! Determine indices for interpolation of day length correction factors.
!
      DO 80 INDEX2 = 1,24
      IF (DL(INDEX2) .GE. DAYEUF) GOTO 90
   80 CONTINUE
   90 CONTINUE
      INDEX1 = INDEX2 - 1
!
!  Compute minimum efficiency requirements of species.
!
      DO 110 J=1,NUECOG
      DAYM = DAYMUL(INDEX1,J) + ( DAYMUL(INDEX2,J) - DAYMUL(INDEX1,J) )
     1       * ( DAYEUF - DL(INDEX1) ) / ( DL(INDEX2) - DL(INDEX1) )
      DO 100 K=IT2(J,1),IT2(J,2)
      EMIN(K)=(RESP(K)+RMORT(K)+FLUSH) / (PMAX(K)*DAYM)
  100 CONTINUE
  110 CONTINUE
!
!  Tranform sunlight to per hour units.
!
      DO 120 J=1,NUSPEC
  120 SURF(J)=SURF(J)/DAY
!
!  Average EMIN in time and find range of extinction coefficient for
!  each species.
!
      DO 170 J=1,NUECOG
      L1 = IT2(J,1)
      L2 = IT2(J,2)
      GROOT (2) = -1.0
      ISKMAX  = L1
      DO 160 K=L1,L2
      IF ( .NOT. ACTIVE_3DL ) THEN
         IF ( .NOT. ACTIVE_EFFT ) THEN
            CALL CONSTR(SURF(K),DEPEUF,EMIN(K),ROOT,J)
         ELSE
            EFFI = AVEFFI(K)
            ROOT(1) = 0.0
            ROOT(2) = EFFI*EXTTOT/EMIN(K)
         ENDIF
      ELSE
         IF ( IFIX_3DL(K) .LT. 0 ) THEN
            EFFI = EFFIC_3DL(K,ISEG_3DL)
         ELSE
            CALL EFFI_3DL(EFFI,K)
         ENDIF
         ROOT(1) = 0.0
         ROOT(2) = EFFI*EXTTOT/EMIN(K)
      ENDIF
      AROOT(2*K-1)=ROOT(1)
      AROOT(2*K)=ROOT(2)
      IF (ROOT(2) .LE. GROOT(2)) GO TO 140
      GROOT(2) = ROOT(2)
      ISKMAX = K
  140 CONTINUE
!
  160 CONTINUE
!
!  New section starts here.
!  We now know the extinction roots all types of species J.
!  Replace them by the MAXIMUM, but only for types, whose KMAX is
!  positive. Do nothing if KMAX is negative.
!
      DO 165 K = IT2(J,1),IT2(J,2)
         IF (K .EQ. ISKMAX) GO TO 165
         IF (AROOT(2*K) .LE. 0.0) GO TO 165
         AROOT(2*K-1)=AROOT(2*ISKMAX-1)
         AROOT(2*K)=AROOT(2*ISKMAX)
 165  CONTINUE

!
!  Calculate the maximum growth rate and the right hand side for the
!  growth constraints of each species, if option "GROCHECK" is on.
!  Copy the growth constraints to BGRO to deal with infeasible
!  solutions, which are recomputed without mortality constraints.
!  Store the type number of each group with the highest KMAX value
!  in JKMAX.
!
!
!  Update nov 4 1992: added DEP to parameter list.
!
!
      IF (LGROCH .EQ. 1 .OR. LOBFUN .EQ. 1) THEN
         CALL MAXGRO(XINIT,GROOT,EXTTOT,EMIN(ISKMAX),J,
     *               ISKMAX,DEP)
         BGRO(J) = B(NUEXRO + J)
      END IF
      JKMAX(J) = ISKMAX
  170 CONTINUE
!
! Print KMIN and KMAX roots of types if option "DUMP" is on.
!
      IF (IDUMP .EQ. 1) THEN
         WRITE (IOU(6),180) (AROOT(I),I=1,2*NUSPEC,2)
  180    FORMAT (' KMIN: ',20(F7.2,1X))
         WRITE (IOU(6),190) (AROOT(I),I=2,2*NUSPEC,2)
  190    FORMAT (' KMAX: ',20(F7.2,1X))
      END IF
!
!  Initialize RERUN flag to 0: do not rerun the problem.
!  Set error indicator to blank.
!
      IRERUN = 0
      ERRIND = ' '
!
!  Order extinction values and determine species in intervals.
!  Initialize counter infeasible intervals and number maximum interval
!  at 0
!
  200 CALL SPCSD(AROOT,OROOT,ACO,EXTLIM,EXTB,NI)
  205 NIN=0
      INT=0
      LSOLU = .FALSE.
      IF (NI .NE. 0) GO TO 220
      IF (IDUMP .EQ. 1) WRITE (IOU(6),210)
  210 FORMAT (5X,'No species permitted in any interval')
      INFEAS=1
      INHIB = 0
      GO TO 270
!
!  Begin solving linear programs for each interval.
!  Set initial values.
!
  220 CONTINUE
      BIO(1)=0.0
      BIO(2)=-1.0
!
!----------------------------------------------------------------------
!            Solve program for biomass in each interval
!----------------------------------------------------------------------
!
      IRS(2)=0
      INOW = 0
      INHIB = 0
      DO 250 J=1,NI
      INOW = INOW + 1
!
!  Set "B" values for extinction coefficient rows.
!
      LINF=0
      B(NUFILI)=OROOT(J)
      B(NUABCO)=OROOT(J+1)
!
!  Determine allowable species for feasibility interval J.
!
      CALL EXCLUD (J,LINF,IRS)
      IF (LINF .EQ. 0) GO TO 230
      INOW = INOW - 1
      IF (LINF .EQ. 1) GO TO 240
!
!  LINF = 2: photo inhibition.
!
      IF (IDUMP .EQ. 1) WRITE (IOU(6),225) J
  225 FORMAT(5X,'No species in interval ',I2,' due to photo inhibition')
      NIN = NIN + 1
      INHIB = 1
      GO TO 250
!
!  Solve, test for feasibility, and find total biomass.
!
  230 CONTINUE
      CALL SOLVLP(INOW,X,BIOMAX,IER,IRS,NONUNI,NUMUNI,LIB)
      IF (IER .NE. 0 .AND. INOW .EQ. 1) IRS3 = IRS(3)
      LINF=IER
      IF (IER .EQ. 0) LSOLU = .TRUE.
  240 CALL PRINT6(BIO,BIOMAX,X,XDEF,INOW,J,LINF,IRS,INT,NIN,NONUNI,
     1            NONUN,NUMUNI,NUMUN,LIB)
  250 CONTINUE
!
!  Check to determine if there has been any feasible solution;
!  if not, INFEAS = 1
!
      INFEAS=0
!     IF (NIN .LT. NI) GO TO 280
      IF (LSOLU) GO TO 280
      INFEAS=1
      IF (IDUMP .EQ. 1) WRITE (IOU(6),260)
  260 FORMAT (5X,'No solution--all intervals are infeasible')
!
!  Infeasible solution. Call FIXINF to deal with this problem.
!
  270 CONTINUE
      IRS(3) = IRS3
      CALL FIXINF(XDEF,BIO,EXTTOT,EXTB,INHIB,NI,IRERUN,IRS,INFEAS,
     1            ERRIND,JKMAX,AROOT,CDATE,SWBLSA)
      IF (IRERUN .NE. 0) GO TO 200
!
!----------------------------------------------------------------------
!              START SECOND PART OF THE SUBROUTINE
!----------------------------------------------------------------------
!
!  The program will now:
!    1. Print output according to the options specified and the
!       solution obtained.
!    2. Determine grazing rate constant and re-run for this timeperiod
!       if grazing is substantial.
!    3. Print a message for non-unique solutions of there may have been
!       any.
!
!
!  Print maximum solution(s) on unit 6 if option DUMP was selected.
!  Print all summarized solutions for all zooplankton iterations
!  on unit 15.
!
  280 CONTINUE
      CALL PRINSU(XDEF,XECO,BIO(2),TOTAL,ZOOD,COUT,OUT15,NTSTOT,
     1     ITNUM,15)
      IF (IDUMP .NE. 0) CALL PRINMA(XDEF,BIO(2),TOTAL,NI,NIN,INT)
!
!  If two intervals have the same maximum biomass, print both of them
!  in the complete and summarized output.
!
      IF (LST .EQ. 1) THEN
        CALL PRINSU(XST,XECOST,BIOST,TOTST,ZOOD,COUTST,OUTST,
     1       NTSTOT,ITNUM,15)
        NTSST = NTSTOT
        IF (IDUMP .NE. 0) CALL PRINMA(XST,BIOST,TOTST,NI,NIN,INTST)
      END IF
!
!  Compute the total extinction
!  Update nov 4 1992: don't divide by SDMIX. Questionable for species
!  with buoyancy regulation; definitaly incorrect for species at the
!  bottom.
!
      EXDEAD = 0.0
      EXLIVE = 0.0
      K1 = NUROWS
      DO 320 K = 1,NUSPEC
      K1 = K1 + 1
      IF (XDEF(K1) .LT. 1.D-6) GO TO 310
      EKXI = EKX (K) * XDEF (K1)
      EXDEAD = EXDEAD + QMREM * RMORT(K) * EKXI
*     EXLIVE = EXLIVE + EKXI/SDMIX(K)
      EXLIVE = EXLIVE + EKXI
  310 CONTINUE
  320 CONTINUE
      EXTTOT = EXDEAD + EXLIVE + EXTB
!
!  Print a warning message if potential non-unique solutions have been
!  determined by subroutine SOLVLP.
!
      IF (IDUMP. EQ. 0 .OR. NUMUN .EQ. 0 .OR.BIO(2) .LE. 0.0) GO TO 460
      WRITE (IOU(6),450) (NONUN(I),I=1,NUMUN)
  450 FORMAT ('  The following species have minimum reduced cost =',
     1        ' 0.0 and might replace',/'  one of the species in the ',
     2        'bloom:',1X,20I3)
  460 CONTINUE
!
!  Print Relevant information.
!  Update 11-10-90:
!  Call prinun, when  LPRINT >= 1
!
      IF (LPRINT .GE. 1)
     1    CALL PRINUN (CDATE, TOTAL, PHYT, EXTTOT, EXLIVE, EXDEAD,
     2                 EXTB, T, USOL, DAY, DEP , ZOOD,ZMAX,GRAMX)
!
!  Return the converted and corrected solar radiation level as
!  CSOL in Joules / cm2 / hour.
!
      CSOL = DSOL * 1.0D-4 / DAY

      RETURN
      END
