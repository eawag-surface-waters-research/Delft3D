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
      module m_extinc

      implicit none

      contains


      subroutine extinc ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculation total and partial extinction coefficients

!
!     This module calculates the total and partial extinction coeffcients,
!     optionally with additional module UITZICHT
!     In input DOC has replaced DisHum for UITZICHT
!
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------
!
      IMPLICIT NONE
!
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT(41) , INCREM(41) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
!     Local declaration
!
      REAL(8) A1      ! R*8 1 I specific ext. inorganic suspended matter 1  [m2/gDM]
      REAL(8) A2      ! R*8 1 I specific ext. inorganic suspended matter 2  [m2/gDM]
      REAL(8) A3      ! R*8 1 I specific ext. inorganic suspended matter 3  [m2/gDM]
      REAL(8) EXT     ! R*8 1 O total extinction                               [1/m]
      REAL(8) EXTIM   ! R*8 1 O calculated extinction IM                       [1/m]
      REAL(8) EXTPOC  ! R*8 1 O extinction POC                                 [1/m]
      REAL(8) EXTDOC  ! R*8 1 O extinction DOC                                 [1/m]
      REAL(8) EXT0    ! R*8 1 I background extinction                          [1/m]
      REAL(8) EXTBL   ! R*8 1 I extinction algae (Bloom)                       [1/m]
      REAL(8) EXTDYN  ! R*8 1 I extinction algae (Dynamo)                      [1/m]
      REAL(8) EXTPRO  ! R*8 1 I extinction algae (Protist)                     [1/m]
      REAL(8) EXTALG  ! R*8 1 I extinction algae                                [1/m]
      REAL(8) EXTMAC  ! R*8 1 I extinction macrophytes                         [1/m]
      REAL(8) EXTSAL  ! R*8 1 O extinction DOC for fresh water fraction        [1/m]
      REAL(8) AIM1    ! R*8 1 I suspended solids  fraction 1                [gDM/m3]
      REAL(8) AIM2    ! R*8 1 I suspended solids  fraction 2                [gDM/m3]
      REAL(8) AIM3    ! R*8 1 I suspended solids  fraction 3                [gDM/m3]
      REAL(8) POC1    ! R*8 1 I fast decomposing detritus                    [gC/m3]
      REAL(8) POC2    ! R*8 1 I medium decomposing detritus                  [gC/m3]
      REAL(8) POC3    ! R*8 1 I slow decomposing detritus                    [gC/m3]
      REAL(8) POC4    ! R*8 1 I refractory detritus                          [gC/m3]
      INTEGER SW_UIT  ! Extinction by UITZICHT on (1) or Off (0)              [-]
      REAL(8) DOC     ! R*8 1 I dissolved organic carbon                     [gC/m3]
      REAL(8) ADOC    ! R*8 1 I Specific extinction of DOC                   [m2/gC]
      REAL(8) DIEP1   ! R*8 1 I argument UITZICHT
      REAL(8) DIEP2   ! R*8 1 I argument UITZICHT
      REAL(8) CORCHL  ! R*8 1 I argument UITZICHT
      REAL(8) C_DET   ! R*8 1 I argument UITZICHT
      REAL(8) C_GL1   ! R*8 1 I argument UITZICHT
      REAL(8) C_GL2   ! R*8 1 I argument UITZICHT
      REAL(8) HELHUM  ! R*8 1 I argument UITZICHT
      REAL(8) TAU     ! R*8 1 I argument UITZICHT
      REAL(8) ANGLE   ! R*8 1 I argument UITZICHT
      REAL(8) DETCDM  ! R*8 1 I dry matter carbon ratio detritus              [g/g]
      REAL(8) XTSAL0  ! R*8 1 I extra VL extinction at Salinity = 0           [1/m]
      REAL(8) SALMAX  ! R*8 1 I salinity value for extra extinction = 0      [g/kg]
      REAL(8) SALIN   ! R*8 1 I actual salinity                              [g/kg]
      REAL(8) APOC1   ! R*8 1 I specific extintion POC1                [1/m/(G/M3)]
      REAL(8) APOC2   ! R*8 1 I specific extintion POC2                [1/m/(G/M3)]
      REAL(8) APOC3   ! R*8 1 I specific extintion POC3                [1/m/(G/M3)]
      REAL(8) APOC4   ! R*8 1 I specific extintion POC4                [1/m/(G/M3)]
!
      REAL(8) CHLORP, DETRIC, GLOEIR, AH_380
      REAL(8) SECCHI, D_1   , EXTP_D, EXTDET, EXTGL, EXTHUM
      INTEGER      IFLUX, ISEG
!
      INTEGER  IPNT(41)
      INTEGER  NR_MES
      SAVE     NR_MES
      DATA     NR_MES / 0 /
!
      IPNT = IPOINT
      IFLUX = 0
!
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
      A1        = PMSA(IPNT(1))
      A2        = PMSA(IPNT(2))
      A3        = PMSA(IPNT(3))
      APOC1     = PMSA(IPNT(4))
      EXT0      = PMSA(IPNT(5))
      EXTBL     = PMSA(IPNT(6))
      EXTDYN    = PMSA(IPNT(7))
      EXTPRO    = PMSA(IPNT(8))
      EXTMAC    = PMSA(IPNT(9))
      AIM1      = PMSA(IPNT(10))
      AIM2      = PMSA(IPNT(11))
      AIM3      = PMSA(IPNT(12))
      POC1      = PMSA(IPNT(13))
      POC2      = PMSA(IPNT(14))
      SW_UIT    = NINT(PMSA(IPNT(15)))
      DOC       = PMSA(IPNT(16))
      ADOC      = PMSA(IPNT(17))
      DIEP1     = PMSA(IPNT(18))
      DIEP2     = PMSA(IPNT(19))
      CORCHL    = PMSA(IPNT(20))
      C_DET     = PMSA(IPNT(21))
      C_GL1     = PMSA(IPNT(22))
      C_GL2     = PMSA(IPNT(23))
      HELHUM    = PMSA(IPNT(24))
      TAU       = PMSA(IPNT(25))
      ANGLE     = PMSA(IPNT(26))
      DETCDM    = PMSA(IPNT(27))
      XTSAL0    = PMSA(IPNT(28))
      SALIN     = PMSA(IPNT(29))
      SALMAX    = PMSA(IPNT(30))
      APOC2     = PMSA(IPNT(31))
      APOC3     = PMSA(IPNT(32))
      APOC4     = PMSA(IPNT(33))
      POC3      = PMSA(IPNT(34))
      POC4      = PMSA(IPNT(35))
!
      IF (SW_UIT.EQ.0) THEN
!
!  calculate extinction coefficients - no UITZICHT
!
        EXTIM  =  A1 * AIM1  + A2 * AIM2 + A3 * AIM3
        EXTPOC =  APOC1*POC1 + APOC2*POC2 + APOC3*POC3 + APOC4*POC4
        EXTDOC =  ADOC*DOC
        EXTALG =  EXTBL + EXTDYN + EXTPRO
        SALIN  =  MIN(SALIN,SALMAX)
        SALIN  =  MAX(SALIN,0.0)
        EXTSAL =  XTSAL0 * (1.0-SALIN/SALMAX)
        EXT    =  EXT0 + EXTIM + EXTPOC + EXTDOC + EXTALG  + EXTMAC
     J            + EXTSAL
!
        IF ( EXT .LT. 1.0E-20 ) THEN
           IF ( NR_MES .LT. 25 ) THEN
              NR_MES = NR_MES + 1
              WRITE(*,*) ' WARNING : zero or negative extinction'
              WRITE(*,*) ' Extinction due to inorganic matter:',EXTIM
              WRITE(*,*) ' Extinction due to organic matter  :',EXTPOC
              WRITE(*,*) ' Extinction due to algae           :',EXTALG
              WRITE(*,*) ' Background extinction             :',EXT0
              WRITE(*,*) ' Extinction by macrophytes         :',EXTMAC
              WRITE(*,*) ' In segment number                 :',ISEG
              WRITE(*,*) ' Background extinction is assumed.'
           ENDIF
           IF ( NR_MES .EQ. 25 ) THEN
              NR_MES = NR_MES + 1
              WRITE(*,*) ' 25 WARNINGS on extinction'
              WRITE(*,*) ' Further messages on extinction surpressed'
           ENDIF
           IF ( EXT0 .LT. 1.E-20 ) THEN
              EXT = 1.E-15
           ELSE
              EXT = EXT0
           ENDIF
        ENDIF
!
      ELSE
!
!  calculate extinction coefficients - with UITZICHT
!
        CHLORP = 0.0
        DETRIC = MAX ( 0.0, DETCDM * (POC1 +POC2 +POC3 + POC4))
        AH_380 = DOC*ADOC
        GLOEIR = AIM1 + AIM2 + AIM3
!
!  total extinction coefficient exclusive of algae, macrophytes and background
!
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               DETRIC, GLOEIR, AH_380, SECCHI, D_1   ,
     3               EXT   , EXTP_D,.FALSE.)
!
!  total extinction coefficient of detritus
!
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               0.0   , GLOEIR, AH_380, SECCHI, D_1   ,
     3               EXTDET, EXTP_D,.FALSE.)
        EXTDET = EXT - EXTDET
!
!  total extinction coefficient of inorganic sediment
!
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               DETRIC, 0.0   , AH_380, SECCHI, D_1   ,
     3               EXTGL , EXTP_D,.FALSE.)
        EXTGL  = EXT - EXTGL
!
!  total extinction coefficient of DOC (humic acids)
!
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               DETRIC, GLOEIR, 0.0   , SECCHI, D_1   ,
     3               EXTHUM, EXTP_D,.FALSE.)
        EXTHUM = EXT - EXTHUM
!
        EXTIM  =  EXTGL
        EXTPOC =  EXTDET
        EXTDOC =  EXTHUM
        EXTSAL =  0.0
        EXT    =  EXT0 + EXT + EXTALG + EXTMAC
!
      ENDIF
!
      PMSA(IPNT(36)) = EXT
      PMSA(IPNT(37)) = EXTIM
      PMSA(IPNT(38)) = EXTPOC
      PMSA(IPNT(39)) = EXTDOC
      PMSA(IPNT(40)) = EXTALG
      PMSA(IPNT(41)) = EXTSAL
!
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IPNT  = IPNT  + INCREM
!
 9000 CONTINUE
!
      RETURN
!
      END

      end module m_extinc
