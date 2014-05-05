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

C    Version 0.3 16 August 2010
C    Version 0.2 22 July 1994
C    Version 0.1 7 Januari 1994
C    Program:    BLMORT.FOR
C    Programmer: Jos van Gils
C
C    Compute fluxes associated with mortality
C
C    Called by: BLOOMC
C    Calls    : NATMOR

      SUBROUTINE BLMORT (BIOMAS, TEMP  , FAUT  , FDET  , FLAUTN, FLDETN,
     J                   FLOOXN, FLMORA, DEAT4 , TSTEPI, LMIXO , LFIXN ,
     J                   LCARB , NUTCON, FLXCON)

      IMPLICIT NONE

C     Arguments
C
C     Name    Type  Length   I/O  Description
C
C     BIOMAS  R*4   NUSPEC   I    Biomass (gC/m3)
C     TEMP    R*4   1        I    Temperature (deg.C)
C     FAUT    R*4   NUSPEC   I    Fraction autolysis (-)
C     FDET    R*4   NUSPEC   I    Fraction detritus (-)
C     FLAUTN  R*4   4        O    Nutrient autolysis fluxes (g/m3/d)
C     FLDETN  R*4   4        O    Detritus production fluxes (g/m3/d)
C     FLOOXN  R*4   4        O    OOX production fluxes (g/m3/d)
C     FLMORA  R*4   NUSPEC   O    Algae mortality fluxes (gC/m3/d)
C     DEAT4   R*4   1        O    ??$Check necessity to transfer$
C     TSTEPI  R*4   1        I    Time step (d)
C     LMIXO   L     1        O    Flag mixotrophy
C     LFIXN   L     1        O    Flag N-fixation
C     LCARB   L     1        I    Flag carbon limitation
c     NUTCON  I*4   8        O    Nutrients involved in active nutrient constraints
c     FLXCON  I*4   8        O    Uptake fluxes involved in active nutrient constraints

      LOGICAL      LMIXO,LFIXN,LCARB
      INTEGER      NUTCON(*), FLXCON(*)

      REAL            BIOMAS(*), TEMP, FAUT(*), FDET(*), FLAUTN(*),
     J                FLDETN(*), FLOOXN(*), FLMORA(*), DEAT4, TSTEPI

C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     NUSPEC  I     1        I    phyt2     Number of types
C     RMORT   R*8   MT       O    size      Mortality rate (1/day)
C     AA      R*8   MN,MT    I    phyt1     Stoichiometry matrix (g/gDW)
C     CTODRY  R*8   MT       I    size      Conversion (gDW/gC)

      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'size.inc'
C
C     Local variables
C
C     Name    Type  Length   I/O  Description
C
C     TEMP8   R*8   1             Temperature (deg.C)
C     DEAT    R*8   1             ??
C     ZOODD   R*8   1             Dummy??$Check$
C     CPHYT   R*4   1             Biomass (gC/m3)
C     CMORT   R*4   1             Mortality flux (gC/m3/d)
C     CMORTA  R*4   1             Autolysis flux (gC/m3/d)
C     CMORTD  R*4   1             Detritus prod. (gC/m3/d)
C     CMORTO  R*4   1             OOx production (gC/m3/d)
C     J       I     1

      REAL            CMORT , CMORTA, CMORTD, CMORTO, CPHYT
      REAL*8          FOOX  , TEMP8 , ZOODD , DEAT
      INTEGER         I, J, K
C
C  Zero fluxes
C
      DO 1 J = 1,4
         FLAUTN(J) = 0.0
         FLDETN(J) = 0.0
         FLOOXN(J) = 0.0
    1 CONTINUE
C
C  Call subroutine NATMOR: calculate natural mortality rate constants.
C
      DEAT  = 0D0
      ZOODD = 0D0
      TEMP8 = DBLE(TEMP)
      CALL NATMOR ( DEAT  , ZOODD , TEMP8 , 1)
      DEAT4 = SNGL(DEAT)
C
C  Mortality module.
C
C  Objective: obtain nutrient fluxes to detritus, OOx and dissolved
C  nutrient pools due to mortality.
C
C  Again note that nutrient fluxes are computed from BLOOM's
C  stochiometry matrix and hence follow from biomasses in units dry
C  weight. The biomass mortality flux for DLWQWQ, however, is in units
C  of carbon.
C
C  Loop over algae species

      DO J=1,NUSPEC
         CPHYT = MAX ( BIOMAS(J) , 0.0 )

C  Compute total mortality for this species and store the flux
c  JvG 16-8-2010 avoid undershoots leading to negative biomass

         CMORT = MIN ( CPHYT * SNGL(RMORT(J)) , CPHYT/TSTEPI )
         FLMORA(J) = CMORT
C
C Partition the mortality flux over detritus(D)/OOx(O)/autolysis(A)
C
         FOOX   = (1. - FAUT(J) - FDET(J))
         CMORTA = CMORT * FAUT(J)
         CMORTD = CMORT * FDET(J)
         CMORTO = CMORT * FOOX
C
C Detritus production for C, N, P, Si (for C including part autolysis)
C Autolysis for C, N, P, Si (NOT for carbon)
C OOx production for C, N, P, Si (for C including part autolysis)
C
         FLDETN(1) = FLDETN(1) + CMORTD + CMORTA *FDET(J)/(FDET(J)+FOOX)
         FLOOXN(1) = FLOOXN(1) + CMORTO + CMORTA * FOOX / (FDET(J)+FOOX)
         DO K=1,NUNUCO
            I = NUTCON(K)
            IF (I.LE.3) THEN
            FLDETN(I+1) = FLDETN(I+1) + CMORTD * SNGL(CTODRY(J)*AA(K,J))
            FLAUTN(I+1) = FLAUTN(I+1) + CMORTA * SNGL(CTODRY(J)*AA(K,J))
            FLOOXN(I+1) = FLOOXN(I+1) + CMORTO * SNGL(CTODRY(J)*AA(K,J))
            ENDIF
         ENDDO

      ENDDO

      RETURN
      END

