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

      subroutine consbl ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_srstop
      use m_monsys
      use m_dhkmrk

!>\file
!>       Grazing module

!
!     Description of the module :
!
!     CONSBL ROUTINE FOR CALCULATION OF GRAZING PRESSURE ON ALGAE
!
! Name    T   L I/O   Description                                   Units
! ----    --- -  -    -------------------                            ----

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
!     Local
!
!     Name    Type  Length   I/O  Description
!     ------  ----  ------   ---  -----------

!     ALGDM   R     1             Dry matter in algae (gDM/m3)
!  GRZMC         0.100000E-01 x Minimum input concentration of Zooplank    (gC/m3)
! ZDETFF         0.500000     x Faecal fraction for detritus of Zooplank       (-)
! ZDETPR          1.00000     x Preference of Zooplank for detritus            (-)
! ZGRZFM          1.50000     x Max. filtration velocity Zooplank        (m3/gC/d)
! ZGRZGM         0.500000     x Max. relative growth rate Zooplank           (1/d)
! ZGRZML          1.00000     x Mult. factor for biomass Zooplank              (-)
! ZGRZMM         0.500000     x Max. relative mortality Zooplank             (1/d)
! ZGRZMO         0.100000     x Monod term filtration rate Zooplank        (gC/m3)
! ZGRZRE         0.250000     x Maintenance respiration coefficient Zooplank   (-)
! ZGRZRM          1.50000     x Max. daily uptake Zooplank             (mgC/mgC.d)
! ZGRZSE         0.500000E-01 x Standard respiration coefficient Zooplank    (1/d)
! ZFrDetBot       0.00000     x Fract. produced detritus to bottom Zooplank    (-)
! ZGRZSTC         1.00000     x C:C ratio Zooplank                         (gC/gC)
! ZGRZSTN        0.181800     x N:C ratio Zooplank                         (gN/gC)
! ZGRZSTP        0.263000E-01 x P:C ratio Zooplank                         (gP/gC)
! ZGRZSTSi        0.00000     x Si:C ratio Zooplank                       (gSi/gC)
! ZTMPFM         0.400000E-01 x temperature coefficient Zooplank filtration (1/oC)
! ZTMPGM         0.400000E-01 x temperature coefficient Zooplank growth     (1/oC)
! ZTMPMM         0.400000E-01 x temperature coefficient Zooplank mortality  (1/oC)
! ZTMPRE         0.400000E-01 x temperature coefficient Zooplank routine met(1/oC)
! ZTMPRM         0.400000E-01 x temperature coefficient Zooplank feeding rat(1/oC)
! ZTMPSE         0.400000E-01 x temperature coefficient Zooplank standard me(1/oC)
! ZUnitSW         0.00000     x Use gC/m3 (0) or gC/m2 (1) for Zooplankton     (-)
! Zooplank       -999.000     x input concentration of zooplankton-grazer1 (gC/m3)
! CZooplank      -999.000       calculated concentration of zooplankton-gra(gC/m3)
! DetC            0.00000     x Detritus Carbon  (DetC)                    (gC/m3)
! POC1            0.00000       POC1 (fast decaying fraction)              (gC/m3)
! GREEN           0.00000       Algae (non-Diatoms)                        (gC/m3)
! DIAT            0.00000       Diatoms                                    (gC/m3)
! BLOOMALG01     -101.000       concentration of algae type 1              (gC/m3)
! ..
! BLOOMALG15     -101.000       concentration of algae type 15             (gC/m3)
! NCRatGreen     0.160000       N:C ratio Greens                           (gN/gC)
! PCRatGreen     0.200000E-01   P:C ratio Greens                           (gP/gC)
! SCRatGreen      0.00000       Si:C ratio Greens                         (gSi/gC)
! ZALGPRGrn       1.00000       Preference of Zooplank for Greens              (-)
! ZALGFFGrn      0.500000       Faecal fraction Greens for Zooplank            (-)

      INTEGER NTOGRZ,        NTONUT,          NTOALG,
     1        IFILSP,        I     ,          J     ,
     2        NIN   ,        NINGRZ,
     3        IP

      PARAMETER (NTOGRZ =  5, NTONUT =  4, NTOALG = 34,
     1           NINGRZ = 25,
     2           NIN    = 5+(NTONUT+2*NTOGRZ)*NTOALG+2*NTONUT+
     3                    NTOGRZ*NINGRZ)

      DIMENSION  IP(NIN)

      REAL    DETBIO(NTONUT), ALGBIO(NTOALG), ALGSTC(NTONUT ,NTOALG),
     1        GRZML (NTOGRZ), GRZOLD(NTOGRZ), GRZNEW(NTOGRZ),
     2        DISFLX(NTONUT), DETFLX(NTONUT), BOTFLX(NTONUT),
     3        ALGFLX(NTOALG), GRZGM (NTOGRZ), TMPGM (NTOGRZ),
     4        WATEMP        , PERIOD        , GRZMM (NTOGRZ),
     5        TMPMM (NTOGRZ), DETPR (NTOGRZ), GRZFOO        ,
     6        ALGPR (NTOALG , NTOGRZ)       , TMPRM (NTOGRZ),
     7        GRZRM (NTOGRZ), TMPFM (NTOGRZ), GRZFM (NTOGRZ),
     8        GRZMO (NTOGRZ), GRZGRZ        , GRZRAT        ,
     9        GRZFIL        , GRZUPT(NTONUT), DETFF (NTOGRZ)
      REAL    ALGFF (NTOALG , NTOGRZ)       , GRZPMX        ,
     A        GRZST (NTONUT , NTOGRZ)       , GRZPRT        ,
     B        GRZRE (NTOGRZ), GRZMET        , TMPSE (NTOGRZ),
     C        GRZSE (NTOGRZ), GRZBRU        , ALTFLX(NTONUT),
     D        GRZFLX(NTONUT), TOTFLX(NTONUT), VOLUME        ,
     E        TMPRE (NTOGRZ), FRDBOT(NTOGRZ), DETRIT(NTONUT),
     F        POC   (NTONUT),                 DEPTH         ,
     G        FRDBOT_SAVE(NTOGRZ)           ,
     H        GRZMC (NTOGRZ)
      INTEGER BENTHS(NTOGRZ)
      INTEGER IKMRK2
      integer lunrep
      REAL    GEM, MaxFiltration, MaxUptake, GrowthResp,
     j        DetrGrazing

      LOGICAL INIT, active_grazer(ntogrz), problem
      SAVE INIT
      DATA INIT /.TRUE./

!     Segment pointers en incrementen
      DO 10 I=1,NIN
        IP(I)  = IPOINT( I)
   10 CONTINUE
!
!     Check parameters not space dependent
!
      if (init) then
        problem = .false.
        if (increm(1) .gt. 0 ) problem = .true.
        if (increm(5) .gt. 0 ) problem = .true.
        DO IFILSP = 1,NTOGRZ
          if (increm(8+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(9+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(10+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(11+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(12+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(13+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(14+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(15+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(16+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(17+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(18+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          DO I = 1,NTONUT
            if (increm(18+(IFILSP-1)*NINGRZ+I) .gt. 0 ) problem = .true.
          ENDDO
          if (increm(23+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(24+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(25+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(26+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(27+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(28+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(29+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
        ENDDO
        DO I=1,NTOALG
          DO J=1,NTONUT-1
            if (increm(5+J*NTOALG+2*NTONUT+NTOGRZ*NINGRZ+I) .gt. 0 )
     j          problem = .true.
          ENDDO
          DO IFILSP=1,NTOGRZ
            if (increm(5+(NTONUT+IFILSP-1)*NTOALG+2*NTONUT+
     1                            NTOGRZ*NINGRZ+I) .gt. 0 )
     j          problem = .true.
            if (increm(5+(NTONUT+NTOGRZ+IFILSP-1)*NTOALG+
     1                             2*NTONUT+NTOGRZ*NINGRZ+I) .gt. 0 )
     j          problem = .true.
          ENDDO
        ENDDO
        if (problem) then
          call getmlu(lunrep)
          write (lunrep, *) 'Error Memory Management CONSBL - Consult system manager'
          write (*, *) 'Error Memory Management CONSBL - Consult system manager'
          call srstop (1)
        endif
      endif

      IFLUX = 0

!     Skip homogeneous grazers at old AND new biomass zero

      DO IFILSP = 1,NTOGRZ
        active_grazer(ifilsp) = .false.
        if ( increm(6+(IFILSP-1)*NINGRZ) .gt. 0 .or.
     j       (PMSA(IP(6+(IFILSP-1)*NINGRZ)).gt.1e-20 .or.
     j        PMSA(IP(7+(IFILSP-1)*NINGRZ)).gt.1e-20)      )
     j   active_grazer(ifilsp) = .true.
      ENDDO

!     Set parameters not space dependent, active grazers only

      PERIOD = PMSA(IP(1))
      GEM    = PMSA(IP(5))
      DO IFILSP = 1,NTOGRZ
       if (active_grazer(ifilsp)) then
        GRZMC (IFILSP) = PMSA(IP(8+(IFILSP-1)*NINGRZ))
        DETFF (IFILSP) = PMSA(IP(9+(IFILSP-1)*NINGRZ))
        DETPR (IFILSP) = PMSA(IP(10+(IFILSP-1)*NINGRZ))
        GRZFM (IFILSP) = PMSA(IP(11+(IFILSP-1)*NINGRZ))
        GRZGM (IFILSP) = PMSA(IP(12+(IFILSP-1)*NINGRZ))
        GRZML (IFILSP) = PMSA(IP(13+(IFILSP-1)*NINGRZ))
        GRZMM (IFILSP) = PMSA(IP(14+(IFILSP-1)*NINGRZ))
        GRZMO (IFILSP) = PMSA(IP(15+(IFILSP-1)*NINGRZ))
        GRZRE (IFILSP) = PMSA(IP(16+(IFILSP-1)*NINGRZ))
        GRZRM (IFILSP) = PMSA(IP(17+(IFILSP-1)*NINGRZ))
        GRZSE (IFILSP) = PMSA(IP(18+(IFILSP-1)*NINGRZ))
        FRDBOT_SAVE(IFILSP) = PMSA(IP(19+(IFILSP-1)*NINGRZ))
        DO I = 1,NTONUT
          GRZST (I,IFILSP) = PMSA(IP(19+(IFILSP-1)*NINGRZ+I))
        ENDDO
        TMPFM (IFILSP) = PMSA(IP(24+(IFILSP-1)*NINGRZ))
        TMPGM (IFILSP) = PMSA(IP(25+(IFILSP-1)*NINGRZ))
        TMPMM (IFILSP) = PMSA(IP(26+(IFILSP-1)*NINGRZ))
        TMPRE (IFILSP) = PMSA(IP(27+(IFILSP-1)*NINGRZ))
        TMPRM (IFILSP) = PMSA(IP(28+(IFILSP-1)*NINGRZ))
        TMPSE (IFILSP) = PMSA(IP(29+(IFILSP-1)*NINGRZ))
        BENTHS(IFILSP) = NINT(PMSA(IP(30+(IFILSP-1)*NINGRZ)))
       endif
      ENDDO
      DO I=1,NTOALG
        ALGSTC(1,I) = 1.0
        DO 71 J=1,NTONUT-1
          ALGSTC(J+1,I) = PMSA(IP(5+J*NTOALG+2*NTONUT+NTOGRZ*NINGRZ+I))
   71   CONTINUE
        DO 81 IFILSP=1,NTOGRZ
         if (active_grazer(ifilsp)) then
          ALGPR(I,IFILSP) = PMSA(IP(5+(NTONUT+IFILSP-1)*NTOALG+2*NTONUT+
     1                             NTOGRZ*NINGRZ+I))
          ALGFF(I,IFILSP) = PMSA(IP(5+(NTONUT+NTOGRZ+IFILSP-1)*NTOALG+
     1                             2*NTONUT+NTOGRZ*NINGRZ+I))
         endif
   81   CONTINUE
      ENDDO

!     Loop over segments

      DO 9000 ISEG = 1 , NOSEG
      IF (BTEST(IKNMRK(ISEG),0)) THEN

      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)

!     RESET FLUXES
      DO 31 I=1,5*NTONUT+NTOALG
        FL(I+IFLUX) = 0.0
   31 CONTINUE
!
!     Input items (potentially) dependent on space
      VOLUME = PMSA(IP(2))
      WATEMP = PMSA(IP(3))
      DEPTH  = PMSA(IP(4))
      DO 30 IFILSP = 1,NTOGRZ
       if (active_grazer(ifilsp)) then
        GRZNEW(IFILSP) = MAX(PMSA(IP(6+(IFILSP-1)*NINGRZ)),GRZMC(IFILSP)) *
     1                   GRZML(IFILSP)
        GRZOLD(IFILSP) = MAX(PMSA(IP(7+(IFILSP-1)*NINGRZ)),GRZMC(IFILSP)) *
     1                   GRZML(IFILSP)
!       Correct unit of input concentration for zoobenthos
!       Force concentration zero for zoobenthos segments without bottom
        FRDBOT(IFILSP) = FRDBOT_SAVE(IFILSP)
        IF ( BENTHS(IFILSP) .EQ. 1 ) THEN
!         Species, defined in g/m2 (typically ZOOBENTHOS)
!         Convert input unit to g/m3!!
          GRZNEW(IFILSP) = GRZNEW(IFILSP)/DEPTH
          GRZOLD(IFILSP) = GRZOLD(IFILSP)/DEPTH
!         FRDBOT(IFILSP) = FRDBOT_SAVE(IFILSP)
          CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
          IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
             GRZNEW(IFILSP) = 0.0
             FRDBOT(IFILSP) = 0.0
          ENDIF
        ENDIF
        IF (INIT) GRZOLD(IFILSP) = GRZNEW(IFILSP)
       endif
   30 CONTINUE
      DO 51 I=1,NTONUT
        DETRIT(I) = PMSA(IP(5+NTOGRZ*NINGRZ+I))
        POC(I) =    PMSA(IP(5+NTOGRZ*NINGRZ+NTONUT+I))
        DETBIO(I) = DETRIT(I)*(1.0-GEM) + POC(I)*GEM
   51 CONTINUE
      DO 61 I=1,NTOALG
        ALGBIO(I) = MAX ( PMSA(IP(5+2*NTONUT+NTOGRZ*NINGRZ+I)) ,0.0 )
   61 CONTINUE

!*******************************************************************************
!**** Processes connected to the GRAZING of algae
!***********************************************************************

!****
!*  2 Loop over the grazers
!****
      DO 200 IFILSP = 1, NTOGRZ
       if (active_grazer(ifilsp)) then
!****
!*  3 Initialize output variables
!****
          DO 40 I = 1, NTONUT
              DISFLX(I) = 0.0E0
              DETFLX(I) = 0.0E0
              BOTFLX(I) = 0.0E0
   40     CONTINUE
          DO 50 I = 1, NTOALG
              ALGFLX(I) = 0.0
   50     CONTINUE
!****
!*  4 Check if grazer growth or mortality exceeds constraints
!*    If this is the case, correct new biomass to constraint value
!****
          IF (GRZOLD(IFILSP) .GT. 0.0) THEN
!             There was biomass
              IF ((GRZNEW(IFILSP) - GRZOLD(IFILSP)) .GE. 0.0) THEN
!                 Net growth
                  IF (GRZNEW(IFILSP) .GT. GRZOLD(IFILSP) *
     &            (1.0 + GRZGM(IFILSP) * EXP(TMPGM(IFILSP) *
     &            (WATEMP - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 + GRZGM(IFILSP) * EXP(TMPGM(IFILSP) *
     &                (WATEMP - 20.0)) * PERIOD)
                  ENDIF
              ELSE
!                 Net mortality
                  IF (GRZNEW(IFILSP) .LT. GRZOLD(IFILSP) *
     &            (1.0 - GRZMM(IFILSP) * EXP(TMPMM(IFILSP) *
     &            (WATEMP - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 - GRZMM(IFILSP) * EXP(TMPMM(IFILSP) *
     &                (WATEMP - 20.0)) * PERIOD)
                  ENDIF
              ENDIF
          ENDIF
!****
!*  5 Calculate total available amount of food (mg C/l)
!****
          GRZFOO = DETBIO(1) * DETPR(IFILSP)
          DO 60 I = 1, NTOALG
              GRZFOO = GRZFOO + ALGBIO(I) * ALGPR(I,IFILSP)
   60     CONTINUE
!****
!*  6 Calculate grazing (1/d) rate
!****
          IF (GRZFOO .GT. 0.0) THEN

              MaxFiltration = EXP(TMPFM(IFILSP) * (WATEMP - 20.0)) *
     &        GRZFM(IFILSP) * (GRZFOO / (GRZFOO + GRZMO(IFILSP)))
              MaxUptake = EXP(TMPRM(IFILSP) * (WATEMP - 20.0)) *
     j        GRZRM(IFILSP)
              if ( MaxFiltration .lt. 1e-20 ) then
!               grazing rate limited by filtration
                GRZGRZ = GRZOLD(IFILSP) * MaxFiltration
              else
                IF (GRZFOO .LT. (MaxUptake/MaxFiltration) ) THEN
!                 grazing rate limited by filtration
                  GRZGRZ = GRZOLD(IFILSP) * MaxFiltration
                ELSE
!                 grazing rate limited by uptake
                  GRZGRZ = GRZOLD(IFILSP) * MaxUptake / GRZFOO
                ENDIF
              endif
          ELSE
              GRZGRZ = 0.0
          ENDIF
!****
!*  7 Calculate realized feeding (mg C/mg C.d) and filtration rate (l/mg C.d)
!*    GRZRAT and GRZFIL are output variables, not used at the moment
!****
          IF (GRZOLD(IFILSP) .GT. 0.0) THEN
              GRZRAT = GRZFOO * GRZGRZ / GRZOLD(IFILSP)
              GRZFIL = GRZGRZ / GRZOLD(IFILSP)
          ELSE
              GRZRAT = 0.0
              GRZFIL = 0.0
          ENDIF
!****
!*  8 Add grazing part to fluxes for algae and detritus
!****
          DO 70 I = 1, NTONUT
              DETFLX(I) = -(DETBIO(I) * GRZGRZ * DETPR(IFILSP))
   70     CONTINUE
          DO 80 I = 1, NTOALG
              ALGFLX(I) = -(ALGBIO(I) * GRZGRZ *
     &        ALGPR(I,IFILSP))
   80     CONTINUE
!****
!*  9 Add fecal fraction to fluxes (choose water/sediment detritus)
!****
          DO 100 J = 1, NTONUT
!             JvG Code is not consistent for FRDBOT/=0 or 1!
!              GRZUPT(J) = -(DETFLX(J) * (1.0 - DETFF(IFILSP)))
!              DETFLX(J) = DETFLX(J) - DETFLX(J) * DETFF(IFILSP) *
!     &                    (1. - FRDBOT(IFILSP))
!              BOTFLX(J) = BOTFLX(J) - DETFLX(J) * DETFF(IFILSP) *
!     &                    FRDBOT(IFILSP)
              DetrGrazing = DETFLX(J)
              GRZUPT(J) = -(DetrGrazing * (1.0 - DETFF(IFILSP)))
              DETFLX(J) = DETFLX(J) - DetrGrazing * DETFF(IFILSP) *
     &                    (1. - FRDBOT(IFILSP))
              BOTFLX(J) = BOTFLX(J) - DetrGrazing * DETFF(IFILSP) *
     &                    FRDBOT(IFILSP)
              DO 90 I = 1, NTOALG
                  GRZUPT(J) = GRZUPT(J) - ALGFLX(I) * ALGSTC(J,I) *
     1            (1.0 - ALGFF(I,IFILSP))
                  DETFLX(J) = DETFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                        ALGFF(I,IFILSP) * (1. - FRDBOT(IFILSP))
                  BOTFLX(J) = BOTFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                        ALGFF(I,IFILSP) * FRDBOT(IFILSP)
   90         CONTINUE
  100     CONTINUE
!****
!* 10 Calculate limiting element for growth
!****
          GRZPMX = 10.0E20
          DO 110 I = 1, NTONUT
              IF (GRZST(I,IFILSP) .GT. 0.0) THEN
                  GRZPRT = GRZUPT(I) / GRZST(I,IFILSP)
                  IF (GRZPRT .LT. GRZPMX) THEN
                      GRZPMX = GRZPRT
                  ENDIF
              ENDIF
  110     CONTINUE
!****
!* 11 Calculate routine respiration (mgC/l.d)
!****
          GrowthResp = EXP(TMPRE(IFILSP)*(WATEMP-20.))*GRZRE(IFILSP)
          DO 120 I = 1, NTONUT
              DISFLX(I) = GRZPMX * GrowthResp * GRZST(I,IFILSP)
              GRZUPT(I) = GRZUPT(I) - DISFLX(I)
  120     CONTINUE
          GRZPMX = GRZPMX * (1.0 - GrowthResp)
!****
!* 12 Calculate the standard respiration (1/d)
!****
          GRZMET = EXP(TMPSE(IFILSP) * (WATEMP - 20.0)) *
     &    GRZSE(IFILSP)
!****
!* 13 Correct for length of period (d)
!****
          DO 130 I = 1, NTOALG
              ALGFLX(I) = ALGFLX(I) * PERIOD
  130     CONTINUE
          DO 140 I = 1, NTONUT
              GRZUPT(I) = GRZUPT(I) * PERIOD
              DETFLX(I) = DETFLX(I) * PERIOD
              BOTFLX(I) = BOTFLX(I) * PERIOD
              DISFLX(I) = DISFLX(I) * PERIOD
  140     CONTINUE
          GRZPMX = GRZPMX * PERIOD
          GRZMET = GRZMET * PERIOD
!****
!* 14 Calculate bruto growth (mg C/period)
!****
          GRZBRU = GRZNEW(IFILSP) - GRZOLD(IFILSP) * (1.0 - GRZMET)
!****
!* 15 Correct bruto growth if intake can not sustain respiration and growth
!****
          IF (GRZBRU .GT. GRZPMX) THEN
              GRZBRU = GRZPMX
              GRZNEW(IFILSP) = GRZOLD(IFILSP) * (1.0 - GRZMET) + GRZPMX
          ENDIF
!****
!* 16 Add respiration to dissolved nutrients
!****
          DO 150 I = 1, NTONUT
              DISFLX(I) = DISFLX(I) + GRZOLD(IFILSP) * GRZMET *
     &        GRZST(I,IFILSP)
!****
!* 17 If there is bruto growth, subtract nutrients from the intake
!****
              IF (GRZBRU .GE. 0.0) THEN
                  GRZUPT(I) = GRZUPT(I) - GRZBRU * GRZST(I,IFILSP)
!****
!* 18 If their is mortality, add the nutrients to the detritus pool
!****
              ELSE
                DETFLX(I) = DETFLX(I) - (GRZBRU * GRZST(I,IFILSP)*
     &          (1 -FRDBOT(IFILSP)))
                BOTFLX(I) = BOTFLX(I) - (GRZBRU * GRZST(I,IFILSP)*
     &          FRDBOT(IFILSP))
              ENDIF
!****
!* 19 Convert the intake not used for bruto growth to the detritus pool
!****
              DETFLX(I) = DETFLX(I) + GRZUPT(I) * (1. - FRDBOT(IFILSP))
              BOTFLX(I) = BOTFLX(I) + GRZUPT(I) * FRDBOT(IFILSP)
  150     CONTINUE
!****
!* 20 Check massbalans
!****
          DO 170 J=1,NTONUT
              ALTFLX(J) = 0.0E0
              DO 160 I=1,NTOALG
                  ALTFLX(J) = ALTFLX(J) + ALGFLX(I) * ALGSTC(J,I)
  160         CONTINUE
              GRZFLX(J) = (GRZNEW(IFILSP) - GRZOLD(IFILSP)) *
     &        GRZST(J,IFILSP)
              TOTFLX(J) = DISFLX(J) + DETFLX(J) + BOTFLX(J) +
     &        ALTFLX(J) + GRZFLX(J)
!
!             Total nutrients in PHYT:
!
              FL(4 + J + IFLUX) = FL(4 + J + IFLUX) - ALTFLX(J)/PERIOD
  170     CONTINUE
!****
!* 21 Update total fluxes and detritus and algal biomass
!****
          DO 180 I = 1, NTONUT
              FL(I + IFLUX)      = FL(I + IFLUX)      + DISFLX(I)/PERIOD
!             MvdV 981130 added division over Detr and GEM POC
              FL(I+8+IFLUX) = FL(I+8+IFLUX) + DETFLX(I)/PERIOD
     &                      * (1.0-GEM)
              FL(I+12+IFLUX) = FL(I+12+IFLUX) + DETFLX(I)/PERIOD * GEM
              FL(I+16+IFLUX) = FL(I+16+IFLUX) + BOTFLX(I)/PERIOD
  180     CONTINUE
          DO 190 I = 1, NTOALG
              FL(I + 20 + IFLUX) = FL(I + 20 + IFLUX) - ALGFLX(I)/PERIOD
  190     CONTINUE
!****
!* 23 Save biomass grazers for next time step
!****
          IF ( BENTHS(IFILSP) .EQ. 1 ) THEN
!           Zoobenthos species
!           Convert input unit to g/m2!!
            GRZNEW(IFILSP) = GRZNEW(IFILSP)*DEPTH
          ENDIF
          IF (GRZML(IFILSP).GT.0.0) THEN
            PMSA(IP(7+(IFILSP-1)*NINGRZ)) = GRZNEW(IFILSP)/GRZML(IFILSP)
          ELSE
            PMSA(IP(7+(IFILSP-1)*NINGRZ)) = 0.0
          ENDIF
!****
!* 22 End loop over filter feeders
!****
       endif
  200 CONTINUE
!
      ENDIF
      IFLUX = IFLUX + NOFLUX
      DO 20 I=1,NIN
        IP(I) = IP(I) + INCREM (  I )
   20 CONTINUE
!
 9000 CONTINUE

      IF (INIT) INIT = .FALSE.

      RETURN
!
      END
