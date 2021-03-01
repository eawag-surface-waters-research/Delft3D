!!  Copyright (C)  Stichting Deltares, 2012-2020.
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

     subroutine DLWQG2     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'DLWQG2' :: DLWQG2
!
!*******************************************************************************
!
      use layered_sediment

      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!     This process replaces DELWAQ G in stand alone DELWAQ in the context of DFM-WAQ
!
!     PRELIMINARY VERSION for feasibility checks

!     Author Jos van Gils
!
!     Type    Name         I/O Description                                        Unit
!
      ! fixed quantities
      real, parameter  :: NOX_RATIO = 4.57
      real, parameter  :: OXC_RATIO = 2.667
      real, parameter  :: NIC_RATIO = 0.933
      real, parameter  :: FEC_RATIO = 18.67
      real, parameter  :: SUC_RATIO = 1.333
      real, parameter  :: CHC_RATIO = 0.5
      real, parameter  :: COX_RATIO = 5.33
      real, parameter  :: CSU_RATIO = 2.67
      real, parameter  :: rhodm = 2.6e6  ! g/m3 solid phase

      integer,parameter   :: nofl = 79
      integer             :: extpsys(nototsedpart)
      integer             :: extssys(nototsedpart)
      integer             :: extdsys(nototseddis)
      integer             :: extfsys(nototseddis)

      ! pointers to concrete items
      integer,parameter :: ip_ku_dFdcC20 = 1
      integer,parameter :: ip_kl_dFdcC20 = 2
      integer,parameter :: ip_ku_dFdcN20 = 3
      integer,parameter :: ip_kl_dFdcN20 = 4
      integer,parameter :: ip_ku_dFdcP20 = 5
      integer,parameter :: ip_kl_dFdcP20 = 6
      integer,parameter :: ip_ku_dMdcC20 = 7
      integer,parameter :: ip_kl_dMdcC20 = 8
      integer,parameter :: ip_ku_dMdcN20 = 9
      integer,parameter :: ip_kl_dMdcN20 = 10
      integer,parameter :: ip_ku_dMdcP20 = 11
      integer,parameter :: ip_kl_dMdcP20 = 12
      integer,parameter :: ip_ku_dSdcC20 = 13
      integer,parameter :: ip_kl_dSdcC20 = 14
      integer,parameter :: ip_ku_dSdcN20 = 15
      integer,parameter :: ip_kl_dSdcN20 = 16
      integer,parameter :: ip_ku_dSdcP20 = 17
      integer,parameter :: ip_kl_dSdcP20 = 18
      integer,parameter :: ip_k_dprdcC20 = 19
      integer,parameter :: ip_k_DOCdcC20 = 20
      integer,parameter :: ip_al_dNf = 21
      integer,parameter :: ip_al_dPf = 22
      integer,parameter :: ip_au_dNf = 23
      integer,parameter :: ip_au_dPf = 24
      integer,parameter :: ip_al_dNm = 25
      integer,parameter :: ip_al_dPm = 26
      integer,parameter :: ip_au_dNm = 27
      integer,parameter :: ip_au_dPm = 28
      integer,parameter :: ip_al_dNs = 29
      integer,parameter :: ip_al_dPs = 30
      integer,parameter :: ip_au_dNs = 31
      integer,parameter :: ip_au_dPs = 32
      integer,parameter :: ip_a_dNpr = 33
      integer,parameter :: ip_a_dPpr = 34
      integer,parameter :: ip_a_dSpr = 35
      integer,parameter :: ip_b_ni = 36
      integer,parameter :: ip_b_poc1doc = 37
      integer,parameter :: ip_b_poc1poc2 = 38
      integer,parameter :: ip_b_poc2doc = 39
      integer,parameter :: ip_b_poc2poc3 = 40
      integer,parameter :: ip_b_poc3doc = 41
      integer,parameter :: ip_b_poc3poc4 = 42
      integer,parameter :: ip_b_su = 43
      integer,parameter :: ip_kT_dec = 44
      integer,parameter :: ip_SWOMDec = 45
      integer,parameter :: ip_KsOxCon = 46
      integer,parameter :: ip_KsNiDen = 47
      integer,parameter :: ip_KsFeRed = 48
      integer,parameter :: ip_KsSuRed = 49
      integer,parameter :: ip_KsOxDenInh = 50
      integer,parameter :: ip_KsNiIRdInh = 51
      integer,parameter :: ip_KsNiSRdInh = 52
      integer,parameter :: ip_KsSuMetInh = 53
      integer,parameter :: ip_CoxDenInh = 54
      integer,parameter :: ip_CoxIRedInh = 55
      integer,parameter :: ip_CoxSRedInh = 56
      integer,parameter :: ip_CoxMetInh = 57
      integer,parameter :: ip_CniMetInh = 58
      integer,parameter :: ip_RedFacDen = 59
      integer,parameter :: ip_RedFacIRed = 60
      integer,parameter :: ip_RedFacSRed = 61
      integer,parameter :: ip_RedFacMet = 62
      integer,parameter :: ip_CTBactAc = 63
      integer,parameter :: ip_SWOxCon = 64
      integer,parameter :: ip_TcDen = 65
      integer,parameter :: ip_TcIRed = 66
      integer,parameter :: ip_TcMet = 67
      integer,parameter :: ip_TcOxCon = 68
      integer,parameter :: ip_TcSRed = 69
      integer,parameter :: ip_RcNit20 = 70
      integer,parameter :: ip_TcNit = 71
      integer,parameter :: ip_KsAmNit = 72
      integer,parameter :: ip_KsOxNit = 73
      integer,parameter :: ip_ZNit = 74
      integer,parameter :: ip_Rc0NitOx = 75
      integer,parameter :: ip_COXNIT = 76
      integer,parameter :: ip_CTNit = 77
      integer,parameter :: ip_RcSox20 = 78
      integer,parameter :: ip_TcSox = 79
      integer,parameter :: ip_Rc0Sox = 80
      integer,parameter :: ip_CoxSUD = 81
      integer,parameter :: ip_DisSEqFeS = 82
      integer,parameter :: ip_RcDisS20 = 83
      integer,parameter :: ip_TcDisS = 84
      integer,parameter :: ip_RcPrcS20 = 85
      integer,parameter :: ip_lKstH2S = 86
      integer,parameter :: ip_lKstHS = 87
      integer,parameter :: ip_TcKstH2S = 88
      integer,parameter :: ip_TcKstHS = 89
      integer,parameter :: ip_TcPrcS = 90
      integer,parameter :: ip_RcMetOx20 = 91
      integer,parameter :: ip_TcMetOx = 92
      integer,parameter :: ip_Rc0MetOx = 93
      integer,parameter :: ip_RcMetSu20 = 94
      integer,parameter :: ip_TcMetSu = 95
      integer,parameter :: ip_Rc0MetSu = 96
      integer,parameter :: ip_CoxMet = 97
      integer,parameter :: ip_CsuMet = 98
      integer,parameter :: ip_KsMet = 99
      integer,parameter :: ip_KsOxMet = 100
      integer,parameter :: ip_KsSuMet = 101
      integer,parameter :: ip_CTMetOx = 102
      integer,parameter :: ip_fScEbul = 103
      integer,parameter :: ip_FrMetGeCH4 = 104
      integer,parameter :: ip_KadsP_20 = 105
      integer,parameter :: ip_TCKadsP = 106
      integer,parameter :: ip_a_OH_PO4 = 107
      integer,parameter :: ip_fr_FeIM1 = 108
      integer,parameter :: ip_fr_FeIM2 = 109
      integer,parameter :: ip_fr_FeIM3 = 110
      integer,parameter :: ip_fr_Feox = 111
      integer,parameter :: ip_Cc_oxPsor = 112
      integer,parameter :: ip_RcAdPO4AAP = 113
      integer,parameter :: ip_RCprecP20 = 114
      integer,parameter :: ip_TCprecipP = 115
      integer,parameter :: ip_RCdissP20 = 116
      integer,parameter :: ip_TCdissolP = 117
      integer,parameter :: ip_EqVivDisP = 118
      integer,parameter :: ip_RatAPandVP = 119
      integer,parameter :: ip_RCdisAP20 = 120
      integer,parameter :: ip_EqAPATDisP = 121
      integer,parameter :: ip_Cc_oxVivP = 122
      integer,parameter :: ip_KdPO4AAP = 123
      integer,parameter :: ip_MaxPO4AAP = 124
      integer,parameter :: ip_SWAdsP = 125
      integer,parameter :: ip_Ceq_disSi = 126
      integer,parameter :: ip_RCdisSi20 = 127
      integer,parameter :: ip_TCdisSi = 128
      integer,parameter :: ip_SWDisSi = 129
      integer,parameter :: ip_DELT = 130
      integer,parameter :: ip_Poros = 131
      integer,parameter :: ip_Th_DelwaqG = 132
      integer,parameter :: linsconstant = 132

      integer,parameter :: ip_pH = 133
      integer,parameter :: ip_Temp = 134
      integer,parameter :: ip_DifLen = 135
      integer,parameter :: ip_TurCoef = 136
      integer,parameter :: ip_DifCoef = 137
      integer,parameter :: ip_Depth = 138

      ! dissolved concentrations in overlying water (dissolved) and S1 concentrations (particulate)
      integer,parameter :: ip_CH4 = 139
      integer,parameter :: ip_DOC = 140
      integer,parameter :: ip_DON = 141
      integer,parameter :: ip_DOP = 142
      integer,parameter :: ip_DOS = 143
      integer,parameter :: ip_NH4 = 144
      integer,parameter :: ip_NO3 = 145
      integer,parameter :: ip_OXY = 146
      integer,parameter :: ip_PO4 = 147
      integer,parameter :: ip_Si = 148
      integer,parameter :: ip_SO4 = 149
      integer,parameter :: ip_SUD = 150
      integer,parameter :: ip_AAPS1 = 151
      integer,parameter :: ip_APATPS1 = 152
      integer,parameter :: ip_FeIIIpaS1 = 153
      integer,parameter :: ip_OpalS1 = 154
      integer,parameter :: ip_POC1S1 = 155
      integer,parameter :: ip_POC2S1 = 156
      integer,parameter :: ip_POC3S1 = 157
      integer,parameter :: ip_POC4S1 = 158
      integer,parameter :: ip_PON1S1 = 159
      integer,parameter :: ip_PON2S1 = 160
      integer,parameter :: ip_PON3S1 = 161
      integer,parameter :: ip_PON4S1 = 162
      integer,parameter :: ip_POP1S1 = 163
      integer,parameter :: ip_POP2S1 = 164
      integer,parameter :: ip_POP3S1 = 165
      integer,parameter :: ip_POP4S1 = 166
      integer,parameter :: ip_POS1S1 = 167
      integer,parameter :: ip_POS2S1 = 168
      integer,parameter :: ip_POS3S1 = 169
      integer,parameter :: ip_POS4S1 = 170
      integer,parameter :: ip_SUPS1 = 171
      integer,parameter :: ip_VIVPS1 = 172

      ! settling fluxes
      integer,parameter :: ip_fSedAAP = 173
      integer,parameter :: ip_fSedAPATP = 174
      integer,parameter :: ip_fSedFeIIIpa = 175
      integer,parameter :: ip_fSedOpal = 176
      integer,parameter :: ip_fSedPOC1 = 177
      integer,parameter :: ip_fSedPOC2 = 178
      integer,parameter :: ip_fSedPOC3 = 179
      integer,parameter :: ip_fSedPOC4 = 180
      integer,parameter :: ip_fSedPON1 = 181
      integer,parameter :: ip_fSedPON2 = 182
      integer,parameter :: ip_fSedPON3 = 183
      integer,parameter :: ip_fSedPON4 = 184
      integer,parameter :: ip_fSedPOP1 = 185
      integer,parameter :: ip_fSedPOP2 = 186
      integer,parameter :: ip_fSedPOP3 = 187
      integer,parameter :: ip_fSedPOP4 = 188
      integer,parameter :: ip_fSedPOS1 = 189
      integer,parameter :: ip_fSedPOS2 = 190
      integer,parameter :: ip_fSedPOS3 = 191
      integer,parameter :: ip_fSedPOS4 = 192
      integer,parameter :: ip_fSedSUP = 193
      integer,parameter :: ip_fSedVIVP = 194
      integer,parameter :: ip_OutInt = 195
      integer,parameter :: ip_Itime = 196
      integer,parameter :: lins = 196

      ! input constants
      real :: ku_dFdcC20
      real :: kl_dFdcC20
      real :: ku_dFdcN20
      real :: kl_dFdcN20
      real :: ku_dFdcP20
      real :: kl_dFdcP20
      real :: ku_dMdcC20
      real :: kl_dMdcC20
      real :: ku_dMdcN20
      real :: kl_dMdcN20
      real :: ku_dMdcP20
      real :: kl_dMdcP20
      real :: ku_dSdcC20
      real :: kl_dSdcC20
      real :: ku_dSdcN20
      real :: kl_dSdcN20
      real :: ku_dSdcP20
      real :: kl_dSdcP20
      real :: k_dprdcC20
      real :: k_DOCdcC20
      real :: al_dNf
      real :: al_dPf
      real :: au_dNf
      real :: au_dPf
      real :: al_dNm
      real :: al_dPm
      real :: au_dNm
      real :: au_dPm
      real :: al_dNs
      real :: al_dPs
      real :: au_dNs
      real :: au_dPs
      real :: a_dNpr
      real :: a_dPpr
      real :: a_dSpr
      real :: b_ni
      real :: b_poc1doc
      real :: b_poc1poc2
      real :: b_poc2doc
      real :: b_poc2poc3
      real :: b_poc3doc
      real :: b_poc3poc4
      real :: b_su
      real :: kT_dec
      real :: SWOMDec
      real :: KsOxCon
      real :: KsNiDen
      real :: KsFeRed
      real :: KsSuRed
      real :: KsOxDenInh
      real :: KsNiIRdInh
      real :: KsNiSRdInh
      real :: KsSuMetInh
      real :: CoxDenInh
      real :: CoxIRedInh
      real :: CoxSRedInh
      real :: CoxMetInh
      real :: CniMetInh
      real :: RedFacDen
      real :: RedFacIRed
      real :: RedFacSRed
      real :: RedFacMet
      real :: CTBactAc
      real :: SWOxCon
      real :: TcDen
      real :: TcIRed
      real :: TcMet
      real :: TcOxCon
      real :: TcSRed
      real :: RcNit20
      real :: TcNit
      real :: KsAmNit
      real :: KsOxNit
      real :: ZNit
      real :: Rc0NitOx
      real :: COXNIT
      real :: CTNit
      real :: RcSox20
      real :: TcSox
      real :: Rc0Sox
      real :: CoxSUD
      real :: DisSEqFeS
      real :: RcDisS20
      real :: TcDisS
      real :: RcPrcS20
      real :: lKstH2S
      real :: lKstHS
      real :: TcKstH2S
      real :: TcKstHS
      real :: TcPrcS
      real :: RcMetOx20
      real :: TcMetOx
      real :: Rc0MetOx
      real :: RcMetSu20
      real :: TcMetSu
      real :: Rc0MetSu
      real :: CoxMet
      real :: CsuMet
      real :: KsMet
      real :: KsOxMet
      real :: KsSuMet
      real :: CTMetOx
      real :: fScEbul
      real :: FrMetGeCH4
      real :: KadsP_20
      real :: TCKadsP
      real :: a_OH_PO4
      real :: fr_FeIM1
      real :: fr_FeIM2
      real :: fr_FeIM3
      real :: fr_Feox
      real :: Cc_oxPsor
      real :: RcAdPO4AAP
      real :: RCprecP20
      real :: TCprecipP
      real :: RCdissP20
      real :: TCdissolP
      real :: EqVivDisP
      real :: RatAPandVP
      real :: RCdisAP20
      real :: EqAPATDisP
      real :: Cc_oxVivP
      real :: KdPO4AAP
      real :: MaxPO4AAP
      real :: SWAdsP
      real :: Ceq_disSi
      real :: RCdisSi20
      real :: TCdisSi
      real :: SWDisSi

      ! not yet input
      real,parameter :: exp_dif = 50.
      real,parameter :: exp_tur = 10.

      ! environment
      real :: DELT
      real :: Poros
      real :: Th_DelwaqG
      real :: pH
      real :: Temp
      real :: DifLen
      real :: TurCoef
      real :: DifCoef
      real :: Depth

      ! states (2 versions for particulates, local without "S1", external with "S1"
      real :: CH4
      real :: DOC
      real :: DON
      real :: DOP
      real :: DOS
      real :: NH4
      real :: NO3
      real :: OXY
      real :: PO4
      real :: Si
      real :: SO4
      real :: SUD

      real :: AAP
      real :: APATP
      real :: FeIIIpa
      real :: Opal
      real :: POC1
      real :: POC2
      real :: POC3
      real :: POC4
      real :: PON1
      real :: PON2
      real :: PON3
      real :: PON4
      real :: POP1
      real :: POP2
      real :: POP3
      real :: POP4
      real :: POS1
      real :: POS2
      real :: POS3
      real :: POS4
      real :: SUP
      real :: VIVP

      !real :: AAPS1
      !real :: APATPS1
      !real :: FeIIIpaS1
      !real :: OpalS1
      !real :: POC1S1
      !real :: POC2S1
      !real :: POC3S1
      !real :: POC4S1
      !real :: PON1S1
      !real :: PON2S1
      !real :: PON3S1
      !real :: PON4S1
      !real :: POP1S1
      !real :: POP2S1
      !real :: POP3S1
      !real :: POP4S1
      !real :: POS1S1
      !real :: POS2S1
      !real :: POS3S1
      !real :: POS4S1
      !real :: SUPS1
      !real :: VIVPS1
      !
      integer :: OutInt
      integer :: Itime

      ! local declarations per parent subroutine
      ! adspo4
      real tfe     , cads    , kads    , oh      , fra     , eqaapm  , im1     , im2     , im3     , cadst   , eqaap   , fads
      ! qim1  , qim2  , qim3  , fim1  , fim2  , fim3
      ! nitrif
      real k0nit   , tempc   , amfunc  , oxfunc  , flnit, k1nit
      !Dec
      integer icfrac
      integer,parameter :: ncfrac = 5
      real poc     , pon     , pop     , pos     , rc20upc , rc20loc , rc20upn , rc20lon , rc20upp , rc20lop , &
           aln     , alp     , aun     , aup     , b_dtp   , b_dtd   , swomd   ,                   decflx(12), &
           rc20c   , elfact  , rc20n   , rc20p   , n_fact  , p_fact  , s_fact  , fnut    , rc20s
      !Vivianit & APATITE
      real fprc    , tmpsol  , fsol    , tmpprc
      !CONSELAC
      real rtmin   , froxc   , frnic   , frfec   , frsuc   , frch4c  , fct2    , fct3    , fct4    , fct5    , &
           tempc1  , tempc2  , tempc3  , tempc4  , tempc5  , fox20   , fni20   , ffe20   , fsu20   , fch420  , &
           fox     , fni     , ffe     , fsu     , fch4    , fsum    , frox    , frni    , frfe    , frsu    , &
           frch4   , romax   , rdmax   , rimax   , rsmax   , rden    , roxc    , rired   , rsred   , conflx(6)
      !EBULCH4
      real laythick, cch4s   , dch4    , ebulfl
      !SPECSUD
      real h_ion   , ks1     , ks2     , csdt    , csd1    , csd2    , csd3    , dish2swk, dishswk , disswk  , &
           frh2sdis, frhsdis , frsdis
      !OXIDSUD
      real fluxox  , k0sox   , k1sox
      !PRECSUL
      real tempcp  , tempcd  , fluxpr  , fluxds
      !OXIDCH4
      real chfunc  , sufunc  , lifunc  , flcox   , flcsu   , k0metox , k1metox , k0metsu , k1metsu
      !     other
      real temp20  , thick

      ! sediment substances definition - in module layered_sediment

      ! dissolved return fluxes
      integer,parameter :: if_CH4 = 1
      integer,parameter :: if_DOC = 2
      integer,parameter :: if_DON = 3
      integer,parameter :: if_DOP = 4
      integer,parameter :: if_DOS = 5
      integer,parameter :: if_NH4 = 6
      integer,parameter :: if_NO3 = 7
      integer,parameter :: if_OXY = 8
      integer,parameter :: if_PO4 = 9
      integer,parameter :: if_Si = 10
      integer,parameter :: if_SO4 = 11
      integer,parameter :: if_SUD = 12

      character*255 errorstring
      integer item, iflux, iseg, itel, noseg2d, iatt1, iatt2, ilay, isys, iseg2d, ip, ifl
      real :: mass3d, sedwatflx, cwater, totmas

      real*8 av(nolay,nolay), bv(nolay), rwork(nolay), term
      real kp(nolay,nototsed), lp(nolay,nototsed)
      integer iwork(nolay), ierror

      integer :: luout

      character*160 :: moname                                                             12345678901234567890
      character*20  :: syname(nototsed) = ['CH4-pore            ','DOC-pore            ','DON-pore            ', &
                                           'DOP-pore            ','DOS-pore            ','NH4-pore            ', &
                                           'NO3-pore            ','OXY-pore            ','PO4-pore            ', &
                                           'Si-pore             ','SO4-pore            ','SUD-pore            ', &
                                           'AAP-bulk            ','APATP-bulk          ','FeIIIpa-bulk        ', &
                                           'Opal-bulk           ','POC1-bulk           ','POC2-bulk           ', &
                                           'POC3-bulk           ','POC4-bulk           ','PON1-bulk           ', &
                                           'PON2-bulk           ','PON3-bulk           ','PON4-bulk           ', &
                                           'POP1-bulk           ','POP2-bulk           ','POP3-bulk           ', &
                                           'POP4-bulk           ','POS1-bulk           ','POS2-bulk           ', &
                                           'POS3-bulk           ','POS4-bulk           ','SUP-bulk            ', &
                                           'VIVP-bulk           ']

      logical :: first = .true.
      logical :: only_ox, dissub

      !
      ! For efficiency
      !
      save

!
!******************************************************************************* INITIAL PROCESSING

      if (first) then

          ! load constants
          ku_dFdcC20 = PMSA(IPOINT(ip_ku_dFdcC20))
          kl_dFdcC20 = PMSA(IPOINT(ip_kl_dFdcC20))
          ku_dFdcN20 = PMSA(IPOINT(ip_ku_dFdcN20))
          kl_dFdcN20 = PMSA(IPOINT(ip_kl_dFdcN20))
          ku_dFdcP20 = PMSA(IPOINT(ip_ku_dFdcP20))
          kl_dFdcP20 = PMSA(IPOINT(ip_kl_dFdcP20))
          ku_dMdcC20 = PMSA(IPOINT(ip_ku_dMdcC20))
          kl_dMdcC20 = PMSA(IPOINT(ip_kl_dMdcC20))
          ku_dMdcN20 = PMSA(IPOINT(ip_ku_dMdcN20))
          kl_dMdcN20 = PMSA(IPOINT(ip_kl_dMdcN20))
          ku_dMdcP20 = PMSA(IPOINT(ip_ku_dMdcP20))
          kl_dMdcP20 = PMSA(IPOINT(ip_kl_dMdcP20))
          ku_dSdcC20 = PMSA(IPOINT(ip_ku_dSdcC20))
          kl_dSdcC20 = PMSA(IPOINT(ip_kl_dSdcC20))
          ku_dSdcN20 = PMSA(IPOINT(ip_ku_dSdcN20))
          kl_dSdcN20 = PMSA(IPOINT(ip_kl_dSdcN20))
          ku_dSdcP20 = PMSA(IPOINT(ip_ku_dSdcP20))
          kl_dSdcP20 = PMSA(IPOINT(ip_kl_dSdcP20))
          k_dprdcC20 = PMSA(IPOINT(ip_k_dprdcC20))
          k_DOCdcC20 = PMSA(IPOINT(ip_k_DOCdcC20))
          al_dNf = PMSA(IPOINT(ip_al_dNf))
          al_dPf = PMSA(IPOINT(ip_al_dPf))
          au_dNf = PMSA(IPOINT(ip_au_dNf))
          au_dPf = PMSA(IPOINT(ip_au_dPf))
          al_dNm = PMSA(IPOINT(ip_al_dNm))
          al_dPm = PMSA(IPOINT(ip_al_dPm))
          au_dNm = PMSA(IPOINT(ip_au_dNm))
          au_dPm = PMSA(IPOINT(ip_au_dPm))
          al_dNs = PMSA(IPOINT(ip_al_dNs))
          al_dPs = PMSA(IPOINT(ip_al_dPs))
          au_dNs = PMSA(IPOINT(ip_au_dNs))
          au_dPs = PMSA(IPOINT(ip_au_dPs))
          a_dNpr = PMSA(IPOINT(ip_a_dNpr))
          a_dPpr = PMSA(IPOINT(ip_a_dPpr))
          a_dSpr = PMSA(IPOINT(ip_a_dSpr))
          b_ni = PMSA(IPOINT(ip_b_ni))
          b_poc1doc = PMSA(IPOINT(ip_b_poc1doc))
          b_poc1poc2 = PMSA(IPOINT(ip_b_poc1poc2))
          b_poc2doc = PMSA(IPOINT(ip_b_poc2doc))
          b_poc2poc3 = PMSA(IPOINT(ip_b_poc2poc3))
          b_poc3doc = PMSA(IPOINT(ip_b_poc3doc))
          b_poc3poc4 = PMSA(IPOINT(ip_b_poc3poc4))
          b_su = PMSA(IPOINT(ip_b_su))
          kT_dec = PMSA(IPOINT(ip_kT_dec))
          SWOMDec = PMSA(IPOINT(ip_SWOMDec))
          KsOxCon = MAX (1.0E-06 , PMSA(IPOINT(ip_KsOxCon)) )
          KsNiDen = MAX (1.0E-06 , PMSA(IPOINT(ip_KsNiDen)) )
          KsFeRed = MAX (1.0E-06 , PMSA(IPOINT(ip_KsFeRed)) )
          KsSuRed = MAX (1.0E-06 , PMSA(IPOINT(ip_KsSuRed)) )
          KsOxDenInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsOxDenInh)) )
          KsNiIRdInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsNiIRdInh)) )
          KsNiSRdInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsNiSRdInh)) )
          KsSuMetInh = MAX (1.0E-06 , PMSA(IPOINT(ip_KsSuMetInh)) )
          CoxDenInh = PMSA(IPOINT(ip_CoxDenInh))
          CoxIRedInh = PMSA(IPOINT(ip_CoxIRedInh))
          CoxSRedInh = PMSA(IPOINT(ip_CoxSRedInh))
          CoxMetInh = PMSA(IPOINT(ip_CoxMetInh))
          CniMetInh = PMSA(IPOINT(ip_CniMetInh))
          RedFacDen = PMSA(IPOINT(ip_RedFacDen))
          RedFacIRed = PMSA(IPOINT(ip_RedFacIRed))
          RedFacSRed = PMSA(IPOINT(ip_RedFacSRed))
          RedFacMet = PMSA(IPOINT(ip_RedFacMet))
          CTBactAc = PMSA(IPOINT(ip_CTBactAc))
          SWOxCon = PMSA(IPOINT(ip_SWOxCon))
          TcDen = PMSA(IPOINT(ip_TcDen))
          TcIRed = PMSA(IPOINT(ip_TcIRed))
          TcMet = PMSA(IPOINT(ip_TcMet))
          TcOxCon = PMSA(IPOINT(ip_TcOxCon))
          TcSRed = PMSA(IPOINT(ip_TcSRed))
          RcNit20 = PMSA(IPOINT(ip_RcNit20))
          TcNit = PMSA(IPOINT(ip_TcNit))
          KsAmNit = PMSA(IPOINT(ip_KsAmNit))
          KsOxNit = PMSA(IPOINT(ip_KsOxNit))
          ZNit = PMSA(IPOINT(ip_ZNit))
          Rc0NitOx = PMSA(IPOINT(ip_Rc0NitOx))
          COXNIT = PMSA(IPOINT(ip_COXNIT))
          CTNit = PMSA(IPOINT(ip_CTNit))
          RcSox20 = PMSA(IPOINT(ip_RcSox20))
          TcSox = PMSA(IPOINT(ip_TcSox))
          Rc0Sox = PMSA(IPOINT(ip_Rc0Sox))
          CoxSUD = PMSA(IPOINT(ip_CoxSUD))
          DisSEqFeS = PMSA(IPOINT(ip_DisSEqFeS))
          RcDisS20 = PMSA(IPOINT(ip_RcDisS20))
          TcDisS = PMSA(IPOINT(ip_TcDisS))
          RcPrcS20 = PMSA(IPOINT(ip_RcPrcS20))
          lKstH2S = PMSA(IPOINT(ip_lKstH2S))
          lKstHS = PMSA(IPOINT(ip_lKstHS))
          TcKstH2S = PMSA(IPOINT(ip_TcKstH2S))
          TcKstHS = PMSA(IPOINT(ip_TcKstHS))
          TcPrcS = PMSA(IPOINT(ip_TcPrcS))
          RcMetOx20 = PMSA(IPOINT(ip_RcMetOx20))
          TcMetOx = PMSA(IPOINT(ip_TcMetOx))
          Rc0MetOx = PMSA(IPOINT(ip_Rc0MetOx))
          RcMetSu20 = PMSA(IPOINT(ip_RcMetSu20))
          TcMetSu = PMSA(IPOINT(ip_TcMetSu))
          Rc0MetSu = PMSA(IPOINT(ip_Rc0MetSu))
          CoxMet = PMSA(IPOINT(ip_CoxMet))
          CsuMet = PMSA(IPOINT(ip_CsuMet))
          KsMet = PMSA(IPOINT(ip_KsMet))
          KsOxMet = PMSA(IPOINT(ip_KsOxMet))
          KsSuMet = PMSA(IPOINT(ip_KsSuMet))
          CTMetOx = PMSA(IPOINT(ip_CTMetOx))
          fScEbul = PMSA(IPOINT(ip_fScEbul))
          FrMetGeCH4 = PMSA(IPOINT(ip_FrMetGeCH4))
          KadsP_20 = PMSA(IPOINT(ip_KadsP_20))
          TCKadsP = PMSA(IPOINT(ip_TCKadsP))
          a_OH_PO4 = PMSA(IPOINT(ip_a_OH_PO4))
          fr_FeIM1 = PMSA(IPOINT(ip_fr_FeIM1))
          fr_FeIM2 = PMSA(IPOINT(ip_fr_FeIM2))
          fr_FeIM3 = PMSA(IPOINT(ip_fr_FeIM3))
          fr_Feox = PMSA(IPOINT(ip_fr_Feox))
          Cc_oxPsor = PMSA(IPOINT(ip_Cc_oxPsor))
          RcAdPO4AAP = PMSA(IPOINT(ip_RcAdPO4AAP))
          RCprecP20 = PMSA(IPOINT(ip_RCprecP20))
          TCprecipP = PMSA(IPOINT(ip_TCprecipP))
          RCdissP20 = PMSA(IPOINT(ip_RCdissP20))
          TCdissolP = PMSA(IPOINT(ip_TCdissolP))
          EqVivDisP = PMSA(IPOINT(ip_EqVivDisP))
          RatAPandVP = PMSA(IPOINT(ip_RatAPandVP))
          RCdisAP20 = PMSA(IPOINT(ip_RCdisAP20))
          EqAPATDisP = PMSA(IPOINT(ip_EqAPATDisP))
          Cc_oxVivP = PMSA(IPOINT(ip_Cc_oxVivP))
          KdPO4AAP = PMSA(IPOINT(ip_KdPO4AAP))
          MaxPO4AAP = PMSA(IPOINT(ip_MaxPO4AAP))
          SWAdsP = PMSA(IPOINT(ip_SWAdsP))
          Ceq_disSi = PMSA(IPOINT(ip_Ceq_disSi))
          RCdisSi20 = PMSA(IPOINT(ip_RCdisSi20))
          TCdisSi = PMSA(IPOINT(ip_TCdisSi))
          SWDisSi = PMSA(IPOINT(ip_SWDisSi))

!          ! hardcoded for the moment, needs to be input
!          ! fraction of mixing 0.01 achieved at depth 0.1
!          exp_dif = Log(1./0.01)/0.1
!          exp_tur = exp_dif

          ! Fixed layer thickness and other parameters
          Th_DelwaqG = PMSA(IPOINT(ip_Th_DelwaqG))
          Delt = PMSA(IPOINT(ip_Delt))
          Poros = PMSA(IPOINT(ip_Poros))
          OutInt = nint(PMSA(IPOINT(ip_OutInt)))

          ! check on non-constants (assume numbering is from 1 to linsconstant)
          do item = 1,linsconstant
              if (increm(item).ne.0) then
                  errorstring = 'Input item _____ is not supposed to be space dependent'
                  write (errorstring(12:16),'(i5)') item
                  CALL ERRSYS (errorstring, 1 )
              endif
          enddo

          ! validity checks
          IF (kdpo4aap .LT. 0.0) CALL ERRSYS ('kdpo4aap negative', 1 )
           IF (a_dNpr .LT. 1E-30) CALL ERRSYS ('DECDET: a_dNpr =< 0', 1 )
           IF (a_dPpr .LT. 1E-30) CALL ERRSYS ('DECDET: a_dPpr =< 0', 1 )
           IF (a_dSpr .LT. 1E-30) CALL ERRSYS ('DECDET: a_dSpr =< 0', 1 )

           IF (al_dNf .LT. 1E-30) CALL ERRSYS ('DECDET: al_dN/F/M/S/ =< 0', 1 )
           IF (al_dPf .LT. 1E-30) CALL ERRSYS ('DECDET: al_dP/F/M/S/ =< 0', 1 )
           IF (au_dNf .LT. 1E-30) CALL ERRSYS ('DECDET: au_dN/F/M/S/ =< 0', 1 )
           IF (au_dPf .LT. 1E-30) CALL ERRSYS ('DECDET: au_dP/F/M/S/ =< 0', 1 )
           IF (al_dNm .LT. 1E-30) CALL ERRSYS ('DECDET: al_dN/F/M/S/ =< 0', 1 )
           IF (al_dPm .LT. 1E-30) CALL ERRSYS ('DECDET: al_dP/F/M/S/ =< 0', 1 )
           IF (au_dNm .LT. 1E-30) CALL ERRSYS ('DECDET: au_dN/F/M/S/ =< 0', 1 )
           IF (au_dPm .LT. 1E-30) CALL ERRSYS ('DECDET: au_dP/F/M/S/ =< 0', 1 )
           IF (al_dNs .LT. 1E-30) CALL ERRSYS ('DECDET: al_dN/F/M/S/ =< 0', 1 )
           IF (al_dPs .LT. 1E-30) CALL ERRSYS ('DECDET: al_dP/F/M/S/ =< 0', 1 )
           IF (au_dNs .LT. 1E-30) CALL ERRSYS ('DECDET: au_dN/F/M/S/ =< 0', 1 )
           IF (au_dPs .LT. 1E-30) CALL ERRSYS ('DECDET: au_dP/F/M/S/ =< 0', 1 )
!
!          Errors if upper limits =< lower limits
!
           IF (au_dNf .LT. al_dNf)  CALL ERRSYS ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ',1)
           IF (au_dPf .LT. al_dPf)  CALL ERRSYS ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ',1)
           IF (au_dNm .LT. al_dNm)  CALL ERRSYS ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ',1)
           IF (au_dPm .LT. al_dPm)  CALL ERRSYS ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ',1)
           IF (au_dNs .LT. al_dNs)  CALL ERRSYS ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ',1)
           IF (au_dPs .LT. al_dPs)  CALL ERRSYS ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ',1)

           IF (ku_dFdcC20 .LT. kl_dFdcC20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ',1)
           IF (ku_dFdcN20 .LT. kl_dFdcN20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ',1)
           IF (ku_dFdcP20 .LT. kl_dFdcP20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ',1)
           IF (ku_dMdcC20 .LT. kl_dMdcC20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ',1)
           IF (ku_dMdcN20 .LT. kl_dMdcN20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ',1)
           IF (ku_dMdcP20 .LT. kl_dMdcP20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ',1)
           IF (ku_dSdcC20 .LT. kl_dSdcC20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ',1)
           IF (ku_dSdcN20 .LT. kl_dSdcN20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ',1)
           IF (ku_dSdcP20 .LT. kl_dSdcP20) CALL ERRSYS ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ',1)


          ! Switches
          ONLY_OX = .FALSE.
          IF (SWOxCon .GT. 0.5) ONLY_OX = .TRUE.

          ! Check if sum of array dl equals input fixed layer thickness
          thick = sum(dl)
          IF (abs(thick-Th_DelwaqG).gt.0.01*Th_DelwaqG) CALL ERRSYS ('Inconsistent layer definition',1)

          ! Determine 2D structure, first find dimension and next fill a mapping array
          noseg2d = 0
          do iseg = 1,noseg
              !CALL DHKMRK(1,IKNMRK(iseg),iatt1) ! pick up first attribute
              CALL DHKMRK(2,IKNMRK(iseg),iatt2) ! pick up second attribute
              !if (iatt1.gt.0) then
                  if (iatt2.eq.0.or.iatt2.eq.3) then
                      noseg2d = noseg2d+1
                  endif
              !endif
          enddo
          allocate(bottomsegments(noseg2d))
          itel = 0
          do iseg = 1,noseg
              !CALL DHKMRK(1,IKNMRK(iseg),iatt1) ! pick up first attribute
              CALL DHKMRK(2,IKNMRK(iseg),iatt2) ! pick up second attribute
              !if (iatt1.gt.0) then
                  if (iatt2.eq.0.or.iatt2.eq.3) then
                      itel = itel+1
                      bottomsegments(itel) = iseg
                  endif
              !endif
          enddo

          ! create layered structure
          allocate (sedconc(nolay,nototsed,noseg2d))

          ! establish administration for S1 states that are present outside and inside
          extpsys(1) = ip_AAPS1
          extpsys(2) = ip_APATPS1
          extpsys(3) = ip_FeIIIpaS1
          extpsys(4) = ip_OpalS1
          extpsys(5) = ip_POC1S1
          extpsys(6) = ip_POC2S1
          extpsys(7) = ip_POC3S1
          extpsys(8) = ip_POC4S1
          extpsys(9) = ip_PON1S1
          extpsys(10) = ip_PON2S1
          extpsys(11) = ip_PON3S1
          extpsys(12) = ip_PON4S1
          extpsys(13) = ip_POP1S1
          extpsys(14) = ip_POP2S1
          extpsys(15) = ip_POP3S1
          extpsys(16) = ip_POP4S1
          extpsys(17) = ip_POS1S1
          extpsys(18) = ip_POS2S1
          extpsys(19) = ip_POS3S1
          extpsys(20) = ip_POS4S1
          extpsys(21) = ip_SUPS1
          extpsys(22) = ip_VIVPS1

          ! establish administration for dissolved states in overlying water
          extdsys(1) = ip_CH4
          extdsys(2) = ip_DOC
          extdsys(3) = ip_DON
          extdsys(4) = ip_DOP
          extdsys(5) = ip_DOS
          extdsys(6) = ip_NH4
          extdsys(7) = ip_NO3
          extdsys(8) = ip_OXY
          extdsys(9) = ip_PO4
          extdsys(10) = ip_Si
          extdsys(11) = ip_SO4
          extdsys(12) = ip_SUD

          ! idem settling fluxes
          extssys(1) = ip_fSedAAP
          extssys(2) = ip_fSedAPATP
          extssys(3) = ip_fSedFeIIIpa
          extssys(4) = ip_fSedOpal
          extssys(5) = ip_fSedPOC1
          extssys(6) = ip_fSedPOC2
          extssys(7) = ip_fSedPOC3
          extssys(8) = ip_fSedPOC4
          extssys(9) = ip_fSedPON1
          extssys(10) = ip_fSedPON2
          extssys(11) = ip_fSedPON3
          extssys(12) = ip_fSedPON4
          extssys(13) = ip_fSedPOP1
          extssys(14) = ip_fSedPOP2
          extssys(15) = ip_fSedPOP3
          extssys(16) = ip_fSedPOP4
          extssys(17) = ip_fSedPOS1
          extssys(18) = ip_fSedPOS2
          extssys(19) = ip_fSedPOS3
          extssys(20) = ip_fSedPOS4
          extssys(21) = ip_fSedSUP
          extssys(22) = ip_fSedVIVP

          ! idem return fluxes
          extfsys(1) = if_CH4
          extfsys(2) = if_DOC
          extfsys(3) = if_DON
          extfsys(4) = if_DOP
          extfsys(5) = if_DOS
          extfsys(6) = if_NH4
          extfsys(7) = if_NO3
          extfsys(8) = if_OXY
          extfsys(9) = if_PO4
          extfsys(10) = if_Si
          extfsys(11) = if_SO4
          extfsys(12) = if_SUD

          ! create initial values for sedconc; zero except parameters taken from external model
          sedconc = 0.0
          do iseg2d = 1,noseg2d
              iseg = bottomsegments(iseg2d)
              do isys = 1,nototsedpart
                  mass3d = PMSA(IPOINT(extpsys(isys))+(iseg-1)*INCREM(extpsys(isys))) / Th_DelwaqG ! g/m2 to g/m3
                  do ilay = 1,nolay
                      sedconc(ilay,nototseddis+isys,iseg2d) = mass3d
                  enddo
              enddo
          enddo

          ! Header
          open (newunit = luout,file='delwaqg.map',access='stream')
          moname = ''
          write (luout) moname
          write (luout) nototsed,nolay*noseg2d
          write (luout) syname

          first = .false.

      endif

      ! output
      Itime = nint(PMSA(IPOINT(ip_Itime)))
      if (mod(itime,outint*3600).eq.0) then
          write (luout) itime,(((sedconc(ilay,isys,iseg2d)/poros,isys=1,nototseddis), &
                                (sedconc(ilay,isys,iseg2d),isys=nototseddis+1,nototsed),iseg2d=1,noseg2d),ilay=1,nolay)
      endif

      ! loop over bottom segments

      do iseg2d = 1,noseg2d
          iseg = bottomsegments(iseg2d) ! this is the 3D segment number (link to PMSA)

          ! Pick up variable environment
          pH      = PMSA(IPOINT(ip_pH     )+(iseg-1)*INCREM(ip_pH     ))
          Temp    = PMSA(IPOINT(ip_Temp   )+(iseg-1)*INCREM(ip_Temp   ))
          Turcoef = PMSA(IPOINT(ip_TurCoef)+(iseg-1)*INCREM(ip_TurCoef))
          DifCoef = PMSA(IPOINT(ip_DifCoef)+(iseg-1)*INCREM(ip_DifCoef))
          Diflen  = PMSA(IPOINT(ip_Diflen )+(iseg-1)*INCREM(ip_Diflen ))
          Depth   = PMSA(IPOINT(ip_Depth  )+(iseg-1)*INCREM(ip_Depth  ))
          do ilay = 1,nolay
          tt(ilay) = turcoef*exp(-exp_tur*bd(ilay))
          td(ilay) = difcoef*exp(-exp_dif*sd(ilay))
          enddo

          ! store total mass
          do isys = 1,nototsed
              totmas = 0.0
              do ilay = 1,nolay
                  totmas = totmas + sedconc(ilay,isys,iseg2d)*dl(ilay)
              enddo
              ip  = lins+isys
              pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = totmas
          enddo

          ! update sedconc for settling
          do isys = 1,nototsedpart
              sedwatflx = PMSA(IPOINT(extssys(isys))+(iseg-1)*INCREM(extssys(isys))) * delt ! g/m2
              sedconc(1,nototseddis+isys,iseg2d) = sedconc(1,nototseddis+isys,iseg2d) + sedwatflx/dl(1) ! to g/m3
          enddo

          kp = 0.0
          lp = 0.0
          do ifl = 1,nofl
              ip = lins + nototsed + ifl
              pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = 0.0
          enddo

          do ilay = 1,nolay
          ! Derive states from sedconc
          CH4 = sedconc(ilay,is_CH4,iseg2d)
          DOC = sedconc(ilay,is_DOC,iseg2d)
          DON = sedconc(ilay,is_DON,iseg2d)
          DOP = sedconc(ilay,is_DOP,iseg2d)
          DOS = sedconc(ilay,is_DOS,iseg2d)
          NH4 = sedconc(ilay,is_NH4,iseg2d)
          NO3 = sedconc(ilay,is_NO3,iseg2d)
          OXY = sedconc(ilay,is_OXY,iseg2d)
          PO4 = sedconc(ilay,is_PO4,iseg2d)
          Si = sedconc(ilay,is_Si,iseg2d)
          SO4 = sedconc(ilay,is_SO4,iseg2d)
          SUD = sedconc(ilay,is_SUD,iseg2d)
          AAP = sedconc(ilay,is_AAP,iseg2d)
          APATP = sedconc(ilay,is_APATP,iseg2d)
          FeIIIpa = sedconc(ilay,is_FeIIIpa,iseg2d)
          Opal = sedconc(ilay,is_Opal,iseg2d)
          POC1 = sedconc(ilay,is_POC1,iseg2d)
          POC2 = sedconc(ilay,is_POC2,iseg2d)
          POC3 = sedconc(ilay,is_POC3,iseg2d)
          POC4 = sedconc(ilay,is_POC4,iseg2d)
          PON1 = sedconc(ilay,is_PON1,iseg2d)
          PON2 = sedconc(ilay,is_PON2,iseg2d)
          PON3 = sedconc(ilay,is_PON3,iseg2d)
          PON4 = sedconc(ilay,is_PON4,iseg2d)
          POP1 = sedconc(ilay,is_POP1,iseg2d)
          POP2 = sedconc(ilay,is_POP2,iseg2d)
          POP3 = sedconc(ilay,is_POP3,iseg2d)
          POP4 = sedconc(ilay,is_POP4,iseg2d)
          POS1 = sedconc(ilay,is_POS1,iseg2d)
          POS2 = sedconc(ilay,is_POS2,iseg2d)
          POS3 = sedconc(ilay,is_POS3,iseg2d)
          POS4 = sedconc(ilay,is_POS4,iseg2d)
          SUP = sedconc(ilay,is_SUP,iseg2d)
          VIVP = sedconc(ilay,is_VIVP,iseg2d)

          TEMP20   = Temp - 20.0
          IM1 = (1.0-poros)*rhodm
          IM2 = 0.0
          IM3 = 0.0

! STARTPROC
          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process AdsPO4AAP
          ! +++++++++++++++++++++++++++++++++++++++++++

          !     Calculation of the total concentration of iron and
          !     the fractions of adsorbed phosphate in the inorganic matter
          !     fractions IM1-3
    !
          TFE  = fr_feim1 * IM1 + fr_feim2 * IM2 + fr_feim3 * IM3

          ! the below part has been removed as it only calculates output items that are not (yet) noutput
    !      QIM1 = 0.0
    !      QIM2 = 0.0
    !      QIM3 = 0.0
    !      IF ( TFE .LT. 1E-10) THEN
    !           FIM1 = 0.0
    !           FIM2 = 0.0
    !           FIM3 = 0.0
    !      ELSE
    !           FIM1 = fr_feim1 * IM1 / TFE
    !           FIM2 = fr_feim2 * IM2 / TFE
    !           FIM3 = fr_feim3 * IM3 / TFE
    !           IF ( IM1 .GT. 1E-10 ) QIM1 = AAP*FIM1/IM1
    !           IF ( IM2 .GT. 1E-10 ) QIM2 = AAP*FIM2/IM2
    !           IF ( IM3 .GT. 1E-10 ) QIM3 = AAP*FIM3/IM3
    !      ENDIF
    !!
    !     Start the calculation of the sorption flux
    !     Use one of three options
    !
          CADS  = 0.0
          CADST = 0.0
          EQAAP = 0.0
          FADS  = 0.0
          KADS  = 0.0
    !
    !     SWADSP = 0 : Instantaneous equilibrium partitioning
    !
          IF (NINT(SWADSP) .EQ. 0) THEN
              FADS  =(((AAP + PO4) / (1.0 + kdpo4aap)) - AAP) / DELT
              EQAAP = -1.0
          ENDIF
    !
    !     SWADSP = 1 : Langmuir sorption
    !
          IF (NINT(SWADSP) .EQ. 1) THEN
              IF ( (maxpo4aap .ge. 1E-10) .and. (kdpo4aap .gt. 1E-10)) THEN
                 CADST = maxpo4aap * TFE
                 EQAAP = (kdpo4aap * CADST * PO4) / (kdpo4aap * PO4 + 1.0)
                 FADS  = rcadpo4aap * (EQAAP - AAP)
              ENDIF
          ENDIF
    !
    !     SWADSP = 2 : pH dependent Langmuir sorption
    !

          IF (NINT(SWADSP) .EQ. 2) THEN

    !     Calculate pH dependency (hydroxyl) and factor for redox potential
    !
               OH = 10.0**(PH-14.0)
               IF (OXY .GE. (Cc_oxPsor*POROS) ) THEN
                  FRA = 1.0
               ELSE
                  FRA = fr_feox
               ENDIF
    !
    !     Calculate total sorption capacity (mol/l)
    !
               CADST = FRA * TFE / (56000.0 * POROS)
    !
    !     Calculate free sorption capacity (mol/l)
    !
               CADS  = MAX( 0.0, CADST - (AAP / (31000.0 * POROS) ) )
    !
    !     Calculate temperature corrected KADS
    !
               KADS  = kadsp_20 * tckadsp**TEMP20
    !
    !     Calculate equilibrium concentration of adsorbed P (gP/m3)
    !
               IF ( ABS(CADS) .LT. 1.E-20 ) THEN
                  EQAAP = 0.0
               ELSE
                  EQAAP = (AAP + PO4)/(1.0 + OH**a_OH_PO4/(KADS*CADS))
               ENDIF
    !
    !     Maximize EQAAP on equivalent CADST
    !
               IF ( CADS .LT. 0.0 ) THEN
                  EQAAPM = 0.9 * CADST * (31000.0 * POROS)
                  EQAAP = EQAAPM
               ENDIF
    !
    !     Calculate the adsorption flux (gP/m3/d)
    !
               FADS  = rcadpo4aap * (EQAAP - AAP)

          ENDIF
    !
    !     Output of original process: EQAAP CADST KADS FIMi QIMi
    !     fluxes in zero order term for relevant substances
          kp(ilay,is_po4) = kp(ilay,is_po4) - fads
          kp(ilay,is_aap) = kp(ilay,is_aap) + fads
    !     store in output
          ip = lins + nototsed + 1
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fads*dl(ilay)


          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process Nitrif_NH4 (CODE NEEDED CHANGE as constants were redefined ...
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Set the rates according to CRTEMP and CROXY
!
          k1nit = RcNit20
          IF ( TEMP .LT. CTNit .OR. OXY .LE. 0.0 ) k1nit = 0.0
!
          K0NIT = 0.0
!
          IF ( TEMP .LT. CTNit .AND. OXY .GT. 0.0 ) THEN
                  K0NIT = znit
          ELSEIF ( TEMP .GE. CTNit .AND. OXY .LE. 0.0 ) THEN
                  K0NIT = Rc0NitOx
          ENDIF
!
          IF ( OXY .LE. (COXNit * POROS) ) K0NIT = 0.0
!
!         Calculate the nitrification flux
!
          TEMPC  = TCNit ** TEMP20
          AMFUNC = NH4 / ( ksamnit * POROS + NH4 )
          OXFUNC = OXY / ( KSOXnit * POROS + OXY )
          FLNIT  = K0NIT + k1nit * TEMPC * AMFUNC * OXFUNC

!         maximise on the availebility of DO and NH4 with safety margin 0.5/0.9

          FLNIT     = MIN(FLNIT,0.5*OXY/NOX_RATIO/DELT)
          FLNIT     = MIN(FLNIT,0.9*NH4/DELT)

    !     Output of original process: OXFUNC
          kp(ilay,is_nh4) = kp(ilay,is_nh4) - flnit
          kp(ilay,is_no3) = kp(ilay,is_no3) + flnit
          kp(ilay,is_oxy) = kp(ilay,is_oxy) - flnit * 4.571
    !     store in output
          ip = lins + nototsed + 2
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + flnit*dl(ilay)

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process DecFast DecMedium DecSlow DecRefr DecDOC
          ! +++++++++++++++++++++++++++++++++++++++++++

          rtmin = 0.0
          do icfrac = 1,ncfrac  ! loop over 5 organic carbon fractions
                if (icfrac.eq.1) then
                    ! POC1
                        POC     = MAX(POC1,0.0)
                        PON     = MAX(PON1,0.0)
                        POP     = MAX(POP1,0.0)
                        POS     = MAX(POS1,0.0)
                        RC20UPC = ku_dFdcC20
                        RC20LOC = kl_dFdcC20
                        RC20UPN = ku_dFdcN20
                        RC20LON = kl_dFdcN20
                        RC20UPP = ku_dFdcP20
                        RC20LOP = kl_dFdcP20
                        ALN     = al_dNf
                        ALP     = al_dPf
                        AUN     = au_dNf
                        AUP     = au_dPf
                        B_DTP   = b_poc1poc2
                        B_DTD   = b_poc1doc
                        SWOMD   = swomdec

                elseif (icfrac.eq.2) then
                    ! POC2
                        POC     = MAX(POC2,0.0)
                        PON     = MAX(PON2,0.0)
                        POP     = MAX(POP2,0.0)
                        POS     = MAX(POS2,0.0)
                        RC20UPC = ku_dMdcC20
                        RC20LOC = kl_dMdcC20
                        RC20UPN = ku_dMdcN20
                        RC20LON = kl_dMdcN20
                        RC20UPP = ku_dMdcP20
                        RC20LOP = kl_dMdcP20
                        ALN     = al_dNm
                        ALP     = al_dPm
                        AUN     = au_dNm
                        AUP     = au_dPm
                        B_DTP   = b_poc2poc3
                        B_DTD   = b_poc2doc
                        SWOMD   = swomdec

                elseif (icfrac.eq.3) then
                    ! POC3
                        POC     = MAX(POC3,0.0)
                        PON     = MAX(PON3,0.0)
                        POP     = MAX(POP3,0.0)
                        POS     = MAX(POS3,0.0)
                        RC20UPC = ku_dSdcC20
                        RC20LOC = kl_dSdcC20
                        RC20UPN = ku_dSdcN20
                        RC20LON = kl_dSdcN20
                        RC20UPP = ku_dSdcP20
                        RC20LOP = kl_dSdcP20
                        ALN     = al_dNs
                        ALP     = al_dPs
                        AUN     = au_dNs
                        AUP     = au_dPs
                        B_DTP   = b_poc3poc4
                        B_DTD   = b_poc3doc
                        SWOMD   = swomdec

                elseif (icfrac.eq.4) then
                    ! POC4
                        POC     = MAX(POC4,0.0)
                        PON     = MAX(PON4,0.0)
                        POP     = MAX(POP4,0.0)
                        POS     = MAX(POS4,0.0)
                        RC20UPC = k_dprdcC20
                        RC20LOC = RC20UPC
                        RC20UPN = RC20UPC
                        RC20LON = RC20UPC
                        RC20UPP = RC20UPC
                        RC20LOP = RC20UPC
                        ALN     = 1.E-20
                        ALP     = 1.E-20
                        AUN     = 1.E-20
                        AUP     = 1.E-20
                        B_DTP   = 0.0
                        B_DTD   = 0.0
                        SWOMD   = 1.0

                elseif (icfrac.eq.5) then
                    ! DOC
                        POC     = MAX(DOC,0.0)
                        PON     = MAX(DON,0.0)
                        POP     = MAX(DOP,0.0)
                        POS     = MAX(DOS,0.0)
                        RC20UPC = k_DOCdcC20
                        RC20LOC = RC20UPC
                        RC20UPN = RC20UPC
                        RC20LON = RC20UPC
                        RC20UPP = RC20UPC
                        RC20LOP = RC20UPC
                        ALN     = 1.E-20
                        ALP     = 1.E-20
                        AUN     = 1.E-20
                        AUP     = 1.E-20
                        B_DTP   = 0.0
                        B_DTD   = 0.0
                        SWOMD   = 1.0

                endif
!
!           If  detritus = 0 : set fluxes to zero and skip algorithm
            IF (POC .LT. 1E-10)  THEN
!
              decflx = 0.0

              RC20C  = 0.0
              TEMPC  = 1.0
              ELFACT = 1.0
              RC20N  = 0.0
              RC20P  = 0.0
              N_FACT = 1.0
              P_FACT = 1.0
              S_FACT = 1.0
!
            ELSE
!
!             Calculate degrad. rate at 20oC for current stochiometry
!
              IF ((PON/POC) .GT. AUN .AND. (POP/POC) .GT. AUP) THEN
!
!                -- both stoch's above upper limit
!
                 RC20C = RC20UPC
                 RC20N = RC20UPN
                 RC20P = RC20UPP
!
              ELSE IF ((PON/POC) .LT. ALN .OR. (POP/POC) .LT. ALP)  THEN
!
!                -- one or both stoch's below lower limit
!
                 RC20C = RC20LOC
                 RC20N = RC20LON
                 RC20P = RC20LOP
!
              ELSE
!
!                -- both stoch's between upper and lower limit
!                   or one stoch above ul and one between ul and ll
!
                 IF (AUN .EQ. ALN .OR. AUP .EQ. ALP) THEN
                    FNUT = 0.5
                 ELSE
                    FNUT = MIN( ((PON/POC)-ALN) / (AUN-ALN) , &
                                ((POP/POC)-ALP) / (AUP-ALP) )
                 ENDIF
!
                 RC20C = RC20LOC + FNUT * (RC20UPC-RC20LOC)
                 RC20N = RC20LON + FNUT * (RC20UPN-RC20LON)
                 RC20P = RC20LOP + FNUT * (RC20UPP-RC20LOP)
!
              ENDIF
!
              RC20S = RC20C
!
!             Calculate correction factors
!             for temperature
!
              TEMPC = kt_dec**TEMP20
!
!             for dominant electron acceptor
!
              IF (OXY .GT. 0.1) THEN
                 ELFACT = 1.0
              ELSE IF (NO3 .GT. 0.1) THEN
                 ELFACT = b_ni
              ELSE
                 ELFACT = b_su
              ENDIF
!
!             for nutrient stripping
!
              IF (NINT(SWOMD) .EQ. 0 ) THEN
                 N_FACT = 1.0 + ((PON/POC) - a_dNpr) / a_dNpr
                 P_FACT = 1.0 + ((POP/POC) - a_dPpr) / a_dPpr
                 S_FACT = 1.0 + ((POS/POC) - a_dSpr) / a_dSpr
                 N_FACT = MAX(N_FACT,0.5)
                 P_FACT = MAX(P_FACT,0.5)
                 S_FACT = MAX(S_FACT,0.5)
                 N_FACT = MIN(N_FACT,5.0)
                 P_FACT = MIN(P_FACT,5.0)
                 S_FACT = MIN(S_FACT,5.0)
              ELSE
                 N_FACT = 1.0
                 P_FACT = 1.0
                 S_FACT = 1.0
              ENDIF
!
!             Calculate the fluxes for mineralization and conversion
!
              decflx(9) = RC20C * TEMPC * ELFACT * POC ! DECOC
              decflx(1) = B_DTP * decflx(9) ! CNVPC
              decflx(5) = B_DTD * decflx(9) ! CNVDC
!
              decflx(10) = RC20N * TEMPC * ELFACT * N_FACT * PON
              decflx(2) = (1/N_FACT) * B_DTP * decflx(10)
              decflx(6) = (1/N_FACT) * B_DTD * decflx(10)
!
              decflx(11) = RC20P * TEMPC * ELFACT * P_FACT * POP
              decflx(3) = (1/P_FACT) * B_DTP * decflx(11)
              decflx(7) = (1/P_FACT) * B_DTD * decflx(11)
!
              decflx(12) = RC20S * TEMPC * ELFACT * S_FACT * POS
              decflx(4) = (1/S_FACT) * B_DTP * decflx(12)
              decflx(8) = (1/S_FACT) * B_DTD * decflx(12)
!
            ENDIF

            rtmin  = rtmin + decflx(9) ! accumulate C mineralisation for later processing in CONSELAC
!
    !       Output of original process: RC20C*TEMPC*ELFACT RC20N*TEMPC*ELFACT RC20P*TEMPC*ELFACT N_FACT P_FACT S_FACT DECOC

            if (icfrac.eq.1) then
                !    ! POC1
                kp(ilay,is_poc1) = kp(ilay,is_poc1) - decflx(1)
                kp(ilay,is_poc2) = kp(ilay,is_poc2) + decflx(1)
                kp(ilay,is_pon1) = kp(ilay,is_pon1) - decflx(2)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) + decflx(2)
                kp(ilay,is_pop1) = kp(ilay,is_pop1) - decflx(3)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) + decflx(3)
                kp(ilay,is_pos1) = kp(ilay,is_pos1) - decflx(4)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) + decflx(4)
                kp(ilay,is_poc1) = kp(ilay,is_poc1) - decflx(5)
                kp(ilay,is_doc ) = kp(ilay,is_doc ) + decflx(5)
                kp(ilay,is_pon1) = kp(ilay,is_pon1) - decflx(6)
                kp(ilay,is_don ) = kp(ilay,is_don ) + decflx(6)
                kp(ilay,is_pop1) = kp(ilay,is_pop1) - decflx(7)
                kp(ilay,is_dop ) = kp(ilay,is_dop ) + decflx(7)
                kp(ilay,is_pos1) = kp(ilay,is_pos1) - decflx(8)
                kp(ilay,is_dos ) = kp(ilay,is_dos ) + decflx(8)
                kp(ilay,is_poc1) = kp(ilay,is_poc1) - decflx(9)
                kp(ilay,is_pon1) = kp(ilay,is_pon1) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop1) = kp(ilay,is_pop1) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos1) = kp(ilay,is_pos1) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)

          !     store in output (fluxes 3-14
                do ifl = 1,12
                  ip = lins + nototsed + 2 + ifl
                  pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + decflx(ifl)*dl(ilay)
                enddo

            elseif (icfrac.eq.2) then
                !    ! POC2
                kp(ilay,is_poc2) = kp(ilay,is_poc2) - decflx(1)
                kp(ilay,is_poc3) = kp(ilay,is_poc3) + decflx(1)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) - decflx(2)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) + decflx(2)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) - decflx(3)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) + decflx(3)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) - decflx(4)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) + decflx(4)
                kp(ilay,is_poc2) = kp(ilay,is_poc2) - decflx(5)
                kp(ilay,is_doc ) = kp(ilay,is_doc ) + decflx(5)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) - decflx(6)
                kp(ilay,is_don ) = kp(ilay,is_don ) + decflx(6)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) - decflx(7)
                kp(ilay,is_dop ) = kp(ilay,is_dop ) + decflx(7)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) - decflx(8)
                kp(ilay,is_dos ) = kp(ilay,is_dos ) + decflx(8)
                kp(ilay,is_poc2) = kp(ilay,is_poc2) - decflx(9)
                kp(ilay,is_pon2) = kp(ilay,is_pon2) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop2) = kp(ilay,is_pop2) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos2) = kp(ilay,is_pos2) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)

          !     store in output (fluxes 15-26
                do ifl = 1,12
                  ip = lins + nototsed + 14 + ifl
                  pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + decflx(ifl)*dl(ilay)
                enddo

            elseif (icfrac.eq.3) then
                !    ! POC3
                kp(ilay,is_poc3) = kp(ilay,is_poc3) - decflx(1)
                kp(ilay,is_poc4) = kp(ilay,is_poc4) + decflx(1)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) - decflx(2)
                kp(ilay,is_pon4) = kp(ilay,is_pon4) + decflx(2)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) - decflx(3)
                kp(ilay,is_pop4) = kp(ilay,is_pop4) + decflx(3)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) - decflx(4)
                kp(ilay,is_pos4) = kp(ilay,is_pos4) + decflx(4)
                kp(ilay,is_poc3) = kp(ilay,is_poc3) - decflx(5)
                kp(ilay,is_doc ) = kp(ilay,is_doc ) + decflx(5)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) - decflx(6)
                kp(ilay,is_don ) = kp(ilay,is_don ) + decflx(6)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) - decflx(7)
                kp(ilay,is_dop ) = kp(ilay,is_dop ) + decflx(7)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) - decflx(8)
                kp(ilay,is_dos ) = kp(ilay,is_dos ) + decflx(8)
                kp(ilay,is_poc3) = kp(ilay,is_poc3) - decflx(9)
                kp(ilay,is_pon3) = kp(ilay,is_pon3) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop3) = kp(ilay,is_pop3) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos3) = kp(ilay,is_pos3) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)
                !
          !     store in output (fluxes 27-38
                do ifl = 1,12
                  ip = lins + nototsed + 26 + ifl
                  pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + decflx(ifl)*dl(ilay)
                enddo

            elseif (icfrac.eq.4) then
                !    ! POC4
                kp(ilay,is_poc4) = kp(ilay,is_poc4) - decflx(9)
                kp(ilay,is_pon4) = kp(ilay,is_pon4) - decflx(10)
                kp(ilay,is_nh4 ) = kp(ilay,is_nh4 ) + decflx(10)
                kp(ilay,is_pop4) = kp(ilay,is_pop4) - decflx(11)
                kp(ilay,is_po4 ) = kp(ilay,is_po4 ) + decflx(11)
                kp(ilay,is_pos4) = kp(ilay,is_pos4) - decflx(12)
                kp(ilay,is_sud ) = kp(ilay,is_sud ) + decflx(12)

          !     store in output (fluxes 39-50
                do ifl = 1,12
                  ip = lins + nototsed + 38 + ifl
                  pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + decflx(ifl)*dl(ilay)
                enddo
                !
            elseif (icfrac.eq.5) then
                !    ! DOC
                kp(ilay,is_doc) = kp(ilay,is_doc) - decflx(9)
                kp(ilay,is_don) = kp(ilay,is_don) - decflx(10)
                kp(ilay,is_nh4) = kp(ilay,is_nh4) + decflx(10)
                kp(ilay,is_dop) = kp(ilay,is_dop) - decflx(11)
                kp(ilay,is_po4) = kp(ilay,is_po4) + decflx(11)
                kp(ilay,is_dos) = kp(ilay,is_dos) - decflx(12)
                kp(ilay,is_sud) = kp(ilay,is_sud) + decflx(12)

          !     store in output (fluxes 51-62
                do ifl = 1,12
                  ip = lins + nototsed + 50 + ifl
                  pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + decflx(ifl)*dl(ilay)
                enddo
                !
            endif
          enddo   ! end loop over carbon fractions

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process Vivianite
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculation of the precipitation or dissolution flux
!         dependent on dissolved oxygen
!
          IF ( OXY .GE. Cc_oxVivP ) THEN
            FPRC   = 0.0
            TMPSOL = TCdissolP**TEMP20
            FSOL   = RCdissP20 * TMPSOL * vivp * OXY / POROS
            IF ( FSOL .LT. 0.0) FSOL = 0.0
            IF ( FSOL*DELT .GE. vivp) FSOL = 0.5 * vivp / DELT
          ELSE
            FSOL   = 0.0
            TMPPRC = TCprecipP**TEMP20
            FPRC   = RCprecP20 * TMPPRC * ( po4 / POROS - EqVivDisP ) * POROS
            IF ( FPRC .LT. 0.0) FPRC = 0.0
          ENDIF
!
    !     Output of original process: none
          kp(ilay,is_po4 ) = kp(ilay,is_po4 ) - fprc + fsol
          kp(ilay,is_vivp) = kp(ilay,is_vivp) + fprc - fsol
    !     store in output
          ip = lins + nototsed + 63
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fprc*dl(ilay)
          ip = lins + nototsed + 64
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fsol*dl(ilay)
!
          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process DisSi
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculation of the dissolution flux
!
          FSOL = 0.0
!
          IF (POROS .GT. 0.05) THEN
!
            TEMPC = TCdisSi**TEMP20
!
            IF (NINT(SWDISSi) .EQ. 0) THEN
               FSOL  = RCdisSi20 * TEMPC * OPAL * ( Ceq_disSi - Si / POROS )
            ELSE
               FSOL  = RCdisSi20 * TEMPC * OPAL
            ENDIF
!
          ELSE
            FSOL  = 0.0
          ENDIF
!
    !     Output of original process: fsol
          kp(ilay,is_opal) = kp(ilay,is_opal) - fsol
          kp(ilay,is_si  ) = kp(ilay,is_si  ) + fsol
    !     store in output
          ip = lins + nototsed + 65
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fsol*dl(ilay)

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process CONSELAC
          ! +++++++++++++++++++++++++++++++++++++++++++
!
          IF (ONLY_OX) THEN
              FROXC = 1.
              FRNIC = 0.
              FRFEC = 0.
              FRSUC = 0.
              FRCH4C = 0.
          ELSE
!
!           Calculation of the temperature dependency coefficients
!
            IF ( TEMP .GE. CTBactAc ) THEN
                  FCT2 = 1.0
                  FCT3 = 1.0
                  FCT4 = 1.0
                  FCT5 = 1.0
            ELSE
                  FCT2 = RedFacDen
                  FCT3 = RedFacIRed
                  FCT4 = RedFacSRed
                  FCT5 = RedFacMet
            ENDIF
!
            TEMPC1 = TcOxCon ** TEMP20
            TEMPC2 = FCT2 * TcDen ** TEMP20
            TEMPC3 = FCT3 * TcIRed ** TEMP20
            TEMPC4 = FCT4 * TcSRed ** TEMP20
            TEMPC5 = FCT5 * TcMet ** TEMP20
!
!           Calculation of the unscaled (relative) contributions
!
            FOX20  = oxy / (KsOxCon * POROS + oxy)
!
            FNI20  = ( no3 / (KsNiDen * POROS + no3) ) * &
                     ( 1 - oxy / (KsOxDenInh * POROS + oxy) )
!
            FFE20  = ( feiiipa / (KsFeRed * POROS + feiiipa) ) * &
                     ( 1 - no3 / (KsNiIRdInh * POROS + no3) )
!
            FSU20  = ( so4 / (KsSuRed * POROS + so4) ) * &
                     ( 1 - no3 / (KsNiSRdInh * POROS + no3) )
!
            FCH420 = 1 - so4 / (KsSuMetInh * POROS + so4)
!
!           Adjust for temperature dependency.
!
            FOX    = FOX20  * TEMPC1
            FNI    = FNI20  * TEMPC2
            FFE    = FFE20  * TEMPC3
            FSU    = FSU20  * TEMPC4
            FCH4   = FCH420 * TEMPC5
!
!           Correction of the unscaled contributions for too high
!           dissolved oxygen or nitrate concentrations to allow
!           sulphate reduction or methanogenesis.
!
            IF ( oxy .GE. (CoxDenInh*POROS) )  FNI = 0.0
            IF ( oxy .GE. (CoxIRedInh*POROS) )  FFE = 0.0
            IF ( oxy .GE. (CoxSRedInh*POROS) )  FSU = 0.0
            IF ( oxy .GE. (CoxMetInh*POROS) .OR.  &
                 no3 .GE. (CniMetInh*POROS) )  FCH4 = 0.0
!
!           Calculation of the scaled contributions
!
            FSUM  = FOX + FNI + FFE + FSU + FCH4
            FROX  = FOX / FSUM
            FRNI  = FNI / FSUM
            FRFE  = FFE / FSUM
            FRSU  = FSU / FSUM
            FRCH4 = 1 - (FROX + FRNI + FRFE + FRSU)
!
!           Calculate the maximal and proposed consumption fluxes in Carbon equivalents of
!           the electron acceptors oxygen, nitrate and sulphate
!           use a safety margin 0.5 on oxygen 0.9 on the rest
!
            ROMAX = oxy / OXC_RATIO / DELT * 0.5
            RDMAX = no3 / NIC_RATIO / DELT * 0.9
            RIMAX = feiiipa / FEC_RATIO / DELT * 0.9
            RSMAX = so4 / SUC_RATIO / DELT * 0.9

!
!           Correct scaled contributions for availability of electron
!           acceptors nitrate, oxygen, iron and sulphate
!
            RDEN  = FRNI  * RTMIN
            IF ( RDEN .GT. RDMAX .AND. RDEN .GT. 0.0 ) THEN
                   FRNIC = FRNI * RDMAX / RDEN
                   FROX  = FROX + FRNI - FRNIC
            ELSE
                   FRNIC = FRNI
            ENDIF
!
            ROXC  = FROX  * RTMIN
            IF ( ROXC .GT. ROMAX .AND. ROXC .GT. 0.0 ) THEN
                   FROXC = FROX * ROMAX / ROXC
                   FRFE  = FRFE + FROX - FROXC
            ELSE
                   FROXC = FROX
            ENDIF
!
            RIRED  = FRFE  * RTMIN
            IF ( RIRED .GT. RIMAX .AND. RIRED .GT. 0.0 ) THEN
                   FRFEC = FRFE * RIMAX / RIRED
                   FRSU  = FRSU + FRFE - FRFEC
            ELSE
                   FRFEC = FRFE
            ENDIF
!
            RSRED  = FRSU  * RTMIN
            IF ( RSRED .GT. RSMAX .AND. RSRED .GT. 0.0 ) THEN
                   FRSUC = FRSU * RSMAX / RSRED
            ELSE
                   FRSUC = FRSU
            ENDIF
!
            FRCH4C = 1 - (FROXC + FRNIC + FRFEC + FRSUC)

!           endif ONLY_OX
          ENDIF

          conflx(1) = FROXC  * RTMIN
          conflx(2) = FRNIC  * RTMIN
          conflx(3) = FRFEC  * RTMIN
          conflx(4) = FRSUC  * RTMIN
          conflx(5) = FRCH4C * FrMetGeCH4 * RTMIN
          conflx(6) = FRCH4C * (1.-FrMetGeCH4) * RTMIN

          !    Output of original process: FROXC FRNIC FRFEC FRSUC FRCH4C

          kp(ilay,is_oxy    ) = kp(ilay,is_oxy    ) - conflx(1) * 2.667
          kp(ilay,is_no3    ) = kp(ilay,is_no3    ) - conflx(2) * 0.933
          kp(ilay,is_feIIIpa) = kp(ilay,is_feIIIpa) - conflx(3) * 18.6
         ! kp(ilay,is_feIId ) = kp(ilay,is_feIId  ) + conflx(3) * 18.6     TODO
          kp(ilay,is_so4    ) = kp(ilay,is_so4    ) - conflx(4) * 1.333
          kp(ilay,is_sud    ) = kp(ilay,is_sud    ) + conflx(4) * 1.333
          kp(ilay,is_ch4    ) = kp(ilay,is_ch4    ) + conflx(5)

          !     store in output
          do ifl = 1,6
            ip = lins + nototsed + 65 + ifl
            pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + conflx(ifl)*dl(ilay)
          enddo

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process EBULCH4
          ! +++++++++++++++++++++++++++++++++++++++++++

          !     -----Warnings-----
!
          laythick  = dl(ilay)
!
!         Calculate the saturation concentration
!
          CCH4S  = 18.76 * (1 + laythick/10.0) * (1.024**(20 - TEMP))  !! is dit goed?
          DCH4   = CH4 / POROS - CCH4S
!
!         Calculate the ebullition flux
!
          IF ( DCH4 .LT. 0.0 ) THEN
              ebulfl = 0.0
          ELSE
              ebulfl = ( fScEbul * DCH4 ) / DELT
          ENDIF
!
    !     Output of original proces: CCH4S
          kp(ilay,is_ch4) = kp(ilay,is_ch4) - ebulfl
    !     store in output
          ip = lins + nototsed + 72
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + ebulfl*dl(ilay)

          ! +++++++++++++++++++++++++++++++++++++++++++
          !     Existing process specsud
          ! +++++++++++++++++++++++++++++++++++++++++++

          if ( sud .gt. 1e-20 ) then

            ! speciation

            h_ion      = 10.**(-ph)
            ks1        = 10.**lksth2s * tcksth2s**temp20
            ks2        = 10.**lksths  * tcksths**temp20
            csdt       = sud/(32000.*poros)
            csd1       = csdt/(1.+ks1/h_ion+(ks1*ks2)/(h_ion*h_ion))
            csd2       = ks1*csd1/h_ion
            csd3       = csdt - csd1 - csd2

            dish2swk   = csd1
            dishswk    = csd2
            disswk     = csd3
            frh2sdis   = csd1/csdt
            frhsdis    = csd2/csdt
            frsdis     = 1.0 - frh2sdis - frhsdis
            if ( frsdis .lt. 0.0 ) then
               frsdis = csd3/csdt
            endif

          else

            dish2swk   = 0.0
            dishswk    = 0.0
            disswk     = 0.0
            frh2sdis   = 0.0
            frhsdis    = 0.0
            frsdis     = 0.0

          endif

    !     Output of original proces:  dish2swk dishswk disswk frh2sdis frhsdis frsdis

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process OXIDSUD code changes to avoid redefinition of constants
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Set the rates according to the DO concentration
!
          k0sox = Rc0Sox
          k1sox = RcSox20
          IF ( oxy .LE. 0.0 ) THEN
                k1sox  = 0.0
          ENDIF
          IF ( oxy .GT. (CoxSUD * POROS) ) THEN
                k0sox = 0.0
          ENDIF
!
!         Calculate the sulphide oxidation flux
!
          TEMPC  = TcSox ** TEMP20
!
          FLUXOX = k0sox + k1sox * TEMPC * SUD * oxy / POROS
          FLUXOX = MIN(FLUXOX,0.9*SUD/DELT)
          FLUXOX = MIN(FLUXOX,0.5*oxy/2.0/DELT)
!
    !     Output of original proces:  -
          kp(ilay,is_sud) = kp(ilay,is_sud) - fluxox
          kp(ilay,is_oxy) = kp(ilay,is_oxy) - fluxox * 2.
          kp(ilay,is_so4) = kp(ilay,is_so4) + fluxox
    !     store in output
          ip = lins + nototsed + 73
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fluxox*dl(ilay)
!
          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process PRECSUL
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculate the precipitation and dissolution fluxes
!         The constant 32000 concerns conversion mole/l to gS/m3
!
          TEMPCP = TcPrcS ** TEMP20
          TEMPCD = TcDisS ** TEMP20
!
          FLUXPR = 32000.0 * RcPrcS20 * TEMPCP * (DisSWK - DisSEqFeS) * POROS
          FLUXDS = 32000.0 * RcDisS20 * TEMPCD * (DisSEqFeS - DisSWK) * POROS
!
!         Correct fluxes depending on under- or supersaturation
!
          IF ( FLUXPR .LT. 0.0) FLUXPR = 0.0
          IF ( FLUXDS .LT. 0.0) FLUXDS = 0.0
          IF ( FLUXDS*DELT .GE. SUP) FLUXDS = 0.5 * SUP / DELT
!
    !     Output of original proces:  -
          kp(ilay,is_sud) = kp(ilay,is_sud) - fluxpr + fluxds
          kp(ilay,is_sup) = kp(ilay,is_sup) + fluxpr - fluxds
    !     store in output
          ip = lins + nototsed + 74
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fluxpr*dl(ilay)
          ip = lins + nototsed + 75
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fluxds*dl(ilay)

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process OXIDCH4
          ! +++++++++++++++++++++++++++++++++++++++++++
!
!         Set the rates according to CRTEMP, COXC and CSUC
!
          k0metox = Rc0MetOx
          k1metox = RcMetOx20
          IF ( TEMP .LT. CTMetOx .OR. &
               oxy .LE. 0.0 .OR. oxy .LE. (CoxMet * POROS) ) THEN
               k1metox  = 0.0
          ENDIF
          IF ( oxy .LE. 0.0 .OR. oxy .GT. (CoxMet * POROS) ) THEN
               k0metox = 0.0
          ENDIF
!
          k0metsu = Rc0MetSu
          k1metsu = RcMetSu20
          IF ( TEMP .LT. CTMetOx .OR. &
                so4 .LE. 0.0 .OR. so4 .LE. (CsuMet * POROS) ) THEN
                k1metsu  = 0.0
          ENDIF
          IF ( so4 .LE. 0.0 .OR. so4 .GT. (CsuMet * POROS) ) THEN
                k0metsu = 0.0
          ENDIF
!
          IF ( oxy .GT. (CoxMet * POROS) ) THEN
               k1metsu  = 0.0
               k0metsu = 0.0
          ENDIF
!
!         Calculate both methane oxidation fluxes
!
          TEMPC  = TcMetOx ** TEMP20
!
          IF ( (KsMet * POROS + CH4) .GT. 0.0) THEN
                  CHFUNC = CH4 / ( KsMet * POROS + CH4 )
          ELSE
                  CHFUNC = 0.0
          ENDIF
!
          IF ( (KsOxMet * POROS + oxy) .GT. 0.0) THEN
                  OXFUNC = oxy  / ( KsOxMet * POROS  + oxy  )
          ELSE
                  OXFUNC = 0.0
          ENDIF
!

          IF ( (KsSuMet * POROS + so4) .GT. 0.0) THEN
                  SUFUNC = so4  / ( KsSuMet * POROS  + so4  )
          ELSE
                  SUFUNC = 0.0
          ENDIF
!
!         Light inhibition function - removed
          LIFUNC = 0.0
!

          FLCOX = k0metox + k1metox * TEMPC * CHFUNC * OXFUNC * (1-LIFUNC)
          FLCSU = k0metsu + k1metsu * TEMPC * CHFUNC * SUFUNC * (1-LIFUNC) ! I think this was intended to be evaluated with TcMetSu. BUG?
          FLCOX = MIN(FLCOX,0.5*oxy/COX_RATIO/DELT)
          FLCOX = MIN(FLCOX,0.9*CH4/DELT)
          FLCSU = MIN(FLCSU,0.9*so4/CSU_RATIO/DELT)
          FLCSU = MIN(FLCSU,0.9*CH4/DELT)

          !     Output of original proces:  OXFUNC SUFUNC LIFUNC
          kp(ilay,is_ch4) = kp(ilay,is_ch4) - flcox - flcsu
          kp(ilay,is_oxy) = kp(ilay,is_oxy) - flcox * 5.33
          kp(ilay,is_so4) = kp(ilay,is_so4) - flcsu * 2.67
          kp(ilay,is_sud) = kp(ilay,is_sud) + flcsu * 2.67
   !      store in output
          ip = lins + nototsed + 76
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + flcox*dl(ilay)
          ip = lins + nototsed + 77
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + flcsu*dl(ilay)

          ! +++++++++++++++++++++++++++++++++++++++++++
          ! Existing process APATITE
          ! +++++++++++++++++++++++++++++++++++++++++++

!         Calculation of the precipitation or dissolution flux
!
          FPRC = 0.0
          FSOL = 0.0
          TMPPRC = TCprecipP**TEMP20
          TMPSOL = TCdissolP**TEMP20
!
          FPRC = RatAPandVP * RCprecP20 * TMPPRC * ( po4 / POROS - EqAPATDisP ) * POROS
          FSOL = RCdisAP20 * TMPSOL * ApatP * ( EqAPATDisP - po4 / POROS )
!
          IF ( FPRC .LT. 0.0) FPRC = 0.0
          IF ( FSOL .LT. 0.0) FSOL = 0.0
          IF ( FSOL*DELT .GE. ApatP) FSOL = 0.5 * ApatP / DELT

          !     Output of original proces: -
          kp(ilay,is_po4  ) = kp(ilay,is_po4  ) - fprc + fsol
          kp(ilay,is_apatp) = kp(ilay,is_apatp) + fprc - fsol
  !       store in output
          ip = lins + nototsed + 78
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fprc*dl(ilay)
          ip = lins + nototsed + 79
          pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = pmsa(ipoint(ip)+(iseg-1)*increm(ip)) + fsol*dl(ilay)

          ! end of loop over layers in present cell
          enddo


! ENDPROC

          do isys = 1,nototsed
              dissub = (isys.le.nototseddis)

              ! dl:    layer thicknes
              ! tt:    turcoef
              ! td     difcoef
              ! kp     zero order processes term
              ! lp     first order processes term
              ! av     a(iun,ieq)
              ! bv     b(ieq)
              ! cwater    concentration in overlying water
              ! sedconc(nolay,nototsed,noseg2d)

              if (dissub) then
                ip = extdsys(isys)
                cwater = pmsa(IPOINT(ip)+(iseg-1)*INCREM(ip))
              endif

              ! build equations
              av = 0d0
              bv = 0d0
              do ilay = 1,nolay

                  ! main diagonal element and rhs for dm/dt term
                  av(ilay,ilay) = av(ilay,ilay) + dble(dl(ilay)/delt)
                  bv(ilay)    = bv(ilay) + dble(sedconc(ilay,isys,iseg2d)*dl(ilay)/delt)

                  ! zero order processes
                  bv(ilay)    = bv(ilay) + dble(kp(ilay,isys)*dl(ilay))

                  ! first order processes
                  !av(ilay,ilay) = av(ilay,ilay) + dble(lp(ilay,isys)*dl(ilay))

                  ! diffusion
                  if (dissub) then
                      ! dissolved upper
                      if (ilay.eq.1) then
                        term = dble(td(ilay)/(diflen/2.+dl(ilay)/2.))
                        bv(ilay)    = bv(ilay) + term*dble(cwater*poros)
                        av(ilay,ilay) = av(ilay,ilay) + term
                      else
                        term = dble(td(ilay)/(dl(ilay-1)/2.+dl(ilay)/2.))
                        av(ilay-1,ilay) = av(ilay-1,ilay) - term
                        av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                      ! dissolved lower
                      if (ilay.ne.nolay) then
                        term = dble(td(ilay+1)/(dl(ilay+1)/2.+dl(ilay)/2.))
                        av(ilay+1,ilay) = av(ilay+1,ilay) - term
                        av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                  else
                      ! solid upper
                      if (ilay.ne.1) then
                        term = dble(tt(ilay-1)/(dl(ilay-1)/2.+dl(ilay)/2.))
                        av(ilay-1,ilay) = av(ilay-1,ilay) - term
                        av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                      ! solid lower
                      if (ilay.ne.nolay) then
                        term = dble(tt(ilay)/(dl(ilay+1)/2.+dl(ilay)/2.))
                        av(ilay+1,ilay) = av(ilay+1,ilay) - term
                        av(ilay,ilay) = av(ilay,ilay) + term
                      endif
                  endif

              enddo

              ! solve; solution will come back as bv
              !all printstelsel(av,bv,nolay,'IN',isys)
              call INVERM (av,bv,nolay,1,nolay,iwork,rwork,ierror)
              !all printstelsel(av,bv,nolay,'OUT',isys)
              if (ierror.ne.0) CALL ERRSYS ('Error Solving Local Equations', 1 )
              do ilay = 1,nolay
                  sedconc(ilay,isys,iseg2d) = bv(ilay)
              enddo

              ! export solute fluxes
              if (dissub) then
                  !             g/m3                   / m                   * m2/d  / m  = g/m3/d, positive TOWARDS water column
                  sedwatflx = -(cwater*poros-sngl(bv(1)))/(diflen/2.+dl(1)/2.) * td(1) / depth
                  iflux = extfsys(isys)
                  fl(iflux+(iseg-1)*noflux) = sedwatflx
                  ! export
                  ip  = lins+nototsed+nofl+isys
                  pmsa(ipoint(ip)+(iseg-1)*increm(ip)) = sedwatflx*depth ! g/m2/d
              endif

          enddo

      enddo

      return
      end

      subroutine printstelsel(av,bv,nolay,comment,isys)
      integer isys,nolay, ilay,ii
      real*8 av(nolay,nolay),bv(nolay)
      character*(*) comment

      write (1961,'(a,1x,''System '',i5)') trim(comment),isys
      write (1961,'(5x,7(7x,i5))') (ii,ii=1,nolay)
      do ilay=1,nolay
          write (1961,'(i4,1x,8(d12.5))') ilay,(av(ii,ilay),ii=1,nolay),bv(ilay)
      enddo
      write (1961,*)
      return
      end
