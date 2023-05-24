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

      subroutine VBXS12     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
      use m_monsys
      use m_errsys
      use m_dhkmrk

!XXXDEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'VBXS12' :: VBXS12
!
!*******************************************************************************
!
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
!     This process replaces the 8 pre-existing VB processes for the S12 mode, in combination with DELWAQG
!
!     Author Jos van Gils
!
!     Type    Name         I/O Description                                        Unit
!
      ! fixed quantities

      ! pointers to input items
      integer,parameter :: ip_SwEmersion = 1
      integer,parameter :: ip_SwVegMod = 2
      integer,parameter :: ip_VBType = 3
      integer,parameter :: ip_nsfVB = 4
      integer,parameter :: ip_CrnsfVB = 5
      integer,parameter :: ip_Initnsfd = 6
      integer,parameter :: ip_CrdepVB = 7
      integer,parameter :: ip_nscdVB = 8
      integer,parameter :: ip_InnscdVB = 9
      integer,parameter :: ip_DELT = 10
      integer,parameter :: ip_TotalDepth = 11
      integer,parameter :: ip_Depth = 12
      integer,parameter :: ip_LocalDepth = 13
      integer,parameter :: ip_Volume = 14
      integer,parameter :: ip_Surf = 15
      integer,parameter :: ip_RootDeVB = 16
      integer,parameter :: ip_NH4 = 17
      integer,parameter :: ip_NO3 = 18
      integer,parameter :: ip_AAP = 19
      integer,parameter :: ip_PO4 = 20
      integer,parameter :: ip_SO4 = 21
      integer,parameter :: ip_SUD = 22
      integer,parameter :: ip_SWRootVB = 23
      integer,parameter :: ip_VmaxVB = 24
      integer,parameter :: ip_KmVB = 25
      integer,parameter :: ip_ViniVB = 26
      integer,parameter :: ip_PorSed = 27
      integer,parameter :: ip_S1_NH4 = 28
      integer,parameter :: ip_S1_NO3 = 29
      integer,parameter :: ip_S1_AAP = 30
      integer,parameter :: ip_S1_PO4 = 31
      integer,parameter :: ip_S1_SO4 = 32
      integer,parameter :: ip_S1_SUD = 33
      integer,parameter :: ip_HSED = 34
      integer,parameter :: ip_VB = 35
      integer,parameter :: ip_maxVB = 36
      integer,parameter :: ip_minVB = 37
      integer,parameter :: ip_hlfAgeVB = 38
      integer,parameter :: ip_sfVB = 39
      integer,parameter :: ip_dmCfVB = 40
      integer,parameter :: ip_iniVB = 41
      integer,parameter :: ip_iniCovVB = 42
      integer,parameter :: ip_SWiniVB = 43
      integer,parameter :: ip_ageVB = 44
      integer,parameter :: ip_VBNavail = 45
      integer,parameter :: ip_VBPavail = 46
      integer,parameter :: ip_VBSavail = 47
      integer,parameter :: ip_VBFrMaxU = 48
      integer,parameter :: ip_F1VB = 49
      integer,parameter :: ip_F2VB = 50
      integer,parameter :: ip_F3VB = 51
      integer,parameter :: ip_F4VB = 52
      integer,parameter :: ip_F5VB = 53
      integer,parameter :: ip_CNf1VB = 54
      integer,parameter :: ip_CNf2VB = 55
      integer,parameter :: ip_CNf3VB = 56
      integer,parameter :: ip_CNf4VB = 57
      integer,parameter :: ip_CNf5VB = 58
      integer,parameter :: ip_CPf1VB = 59
      integer,parameter :: ip_CPf2VB = 60
      integer,parameter :: ip_CPf3VB = 61
      integer,parameter :: ip_CPf4VB = 62
      integer,parameter :: ip_CPf5VB = 63
      integer,parameter :: ip_CSf1VB = 64
      integer,parameter :: ip_CSf2VB = 65
      integer,parameter :: ip_CSf3VB = 66
      integer,parameter :: ip_CSf4VB = 67
      integer,parameter :: ip_CSf5VB = 68
      integer,parameter :: ip_SWRegrVB = 69
      integer,parameter :: ip_SWVBDec = 70
      integer,parameter :: ip_IniAgeVB = 71
      integer,parameter :: ip_IniVBdec = 72
      integer,parameter :: ip_Rc0GWV = 73
      integer,parameter :: ip_TcGWV = 74
      integer,parameter :: ip_AcGWV = 75
      integer,parameter :: ip_MinRWV = 76
      integer,parameter :: ip_TBmWV = 77
      integer,parameter :: ip_TempAir = 78
      integer,parameter :: ip_MinVBAll = 79
      integer,parameter :: ip_FfolPOC1 = 80
      integer,parameter :: ip_FfolPOC2 = 81
      integer,parameter :: ip_FfrootPOC1 = 82
      integer,parameter :: ip_FfrootPOC2 = 83
      integer,parameter :: ip_RcMrtVB = 84
      integer,parameter :: ip_Rc0MSWV = 85
      integer,parameter :: ip_TcMSWV = 86
      integer,parameter :: ip_RcMGRWV = 87
      integer,parameter :: ip_AcMWV = 88
      integer,parameter :: ip_MaxRWV = 89
      integer,parameter :: ip_SwDisVB = 90
      integer,parameter :: ip_VegHeVB = 91
      integer,parameter :: ip_FfacVB = 92
      integer,parameter :: ip_fNVBup   = 93
      integer,parameter :: ip_fPVBup   = 94
      integer,parameter :: ip_fSVBup   = 95
      integer,parameter :: nin = 95

      ! pointers to output items (except fluxes to S12 and those that are also input)
      integer,parameter :: ip_SwVBGro  = 96
      integer,parameter :: ip_SwVBMrt  = 97
      integer,parameter :: ip_VBha     = 98
      integer,parameter :: ip_VBAha    = 99
      integer,parameter :: ip_rGWV     = 100
      integer,parameter :: ip_fVB      = 101
      integer,parameter :: ip_VBAge0ha = 102
      integer,parameter :: ip_NutLimVB = 103
      integer,parameter :: ip_rMrtVB   = 104
      integer,parameter :: ip_fMrtVB   = 105
      ! offset in parameter array for 6 uptake fluxes
      integer,parameter :: ip_offsetufl = 105
      ! offset in parameter array for 16 release fluxes
      integer,parameter :: ip_offsetrfl = 111
      integer,parameter :: nout = 47
      ! follow 22 fluxes tofrom sediment
      ! follow duplicate input items

      ! pointers to fluxes
      integer,parameter :: if_dVB = 1
      integer,parameter :: if_dMrtC1VB = 2
      integer,parameter :: if_dMrtC3VB = 4
      integer,parameter :: if_dMrtC4VB = 5
      integer,parameter :: if_dMrtC2VB = 3
      integer,parameter :: if_dMrtC5VB = 6
      integer,parameter :: if_dN1VBupw = 7
      integer,parameter :: if_dN2VBupw = 8
      integer,parameter :: if_dP1VBupw = 9
      integer,parameter :: if_dP2VBupw = 10
      integer,parameter :: if_dS1VBupw = 11
      integer,parameter :: if_dS2VBupw = 12
!      integer,parameter :: if_dN1VBups = 13
!      integer,parameter :: if_dN2VBups = 14
!      integer,parameter :: if_dP1VBups = 15
!      integer,parameter :: if_dP2VBups = 16
!      integer,parameter :: if_dS1VBups = 17
!      integer,parameter :: if_dS2VBups = 18
      ! follow the mortality fluxes

!      integer,parameter :: if_firstWat = if_dS2VBups
      integer,parameter :: if_firstWat = if_dS2VBupw
      integer,parameter :: NFlxWat = 20
!      integer,parameter :: if_firstSed = if_firstWat + NFlxWat
      integer,parameter :: NFlxSed = 16

      ! conversion of units
      real, parameter :: tonha_gm2 = 100.0   ! Converts tons/ha to g/m2
      real, parameter :: perc_frac = 100.0   ! Converts percentage to fraction

      ! input items
      real :: SwEmersion
      integer :: SwVegMod
      integer :: VBType
      real :: nsfVB
      real :: CrnsfVB
      real :: Initnsfd
      real :: CrdepVB
      real :: nscdVB
      real :: InnscdVB
      real :: DELT
      real :: TotalDepth
      real :: Depth
      real :: LocalDepth
      real :: Volume
      real :: Surf
      real :: RootDeVB
      real :: NH4
      real :: NO3
      real :: AAP
      real :: PO4
      real :: SO4
      real :: SUD
      real :: SWRootVB
      real :: VmaxVB
      real :: KmVB
      real :: ViniVB
      real :: PorSed
      real :: S1_NH4
      real :: S1_NO3
      real :: S1_AAP
      real :: S1_PO4
      real :: S1_SO4
      real :: S1_SUD
      real :: HSED
      real :: VB
      real :: maxVB
      real :: minVB
      real :: hlfAgeVB
      real :: sfVB
      real :: dmCfVB
      real :: iniVB
      real :: iniCovVB
      integer :: SWiniVB
      real :: ageVB
      real :: VBNavail
      real :: VBPavail
      real :: VBSavail
      real :: VBFrMaxU
      real :: F1VB
      real :: F2VB
      real :: F3VB
      real :: F4VB
      real :: F5VB
      real :: CNf1VB
      real :: CNf2VB
      real :: CNf3VB
      real :: CNf4VB
      real :: CNf5VB
      real :: CPf1VB
      real :: CPf2VB
      real :: CPf3VB
      real :: CPf4VB
      real :: CPf5VB
      real :: CSf1VB
      real :: CSf2VB
      real :: CSf3VB
      real :: CSf4VB
      real :: CSf5VB
      integer :: SWRegrVB
      integer :: SWVBDec
      real :: IniAgeVB
      real :: IniVBdec
      real :: Rc0GWV
      real :: TcGWV
      real :: AcGWV
      real :: MinRWV
      real :: TBmWV
      real :: TempAir
      real :: MinVBAll
      real :: FfolPOC1
      real :: FfolPOC2
      real :: FfrootPOC1
      real :: FfrootPOC2
      real :: RcMrtVB
      real :: Rc0MSWV
      real :: TcMSWV
      real :: RcMGRWV
      real :: AcMWV
      real :: MaxRWV
      integer :: SwDisVB
      real :: VegHeVB
      real :: FfacVB
      real :: fNVBup
      real :: fPVBup
      real :: fSVBup

      ! output items (except ...)
      real :: SwVBGro
      real :: SwVBMrt
      real :: VBha
      real :: VBAha
      real :: rGWV
      real :: fVB
      real :: VBAge0ha
      real :: NutLimVB
      real :: rMrtVB
      real :: fMrtVB

      ! fluxes
      real :: dVB
      real :: dMrtC1VB
      real :: dMrtC3VB
      real :: dMrtC4VB
      real :: dMrtC2VB
      real :: dMrtC5VB
      real :: dN1VBupw
      real :: dN2VBupw
      real :: dP1VBupw
      real :: dP2VBupw
      real :: dS1VBupw
      real :: dS2VBupw
      real :: dN1VBups
      real :: dN2VBups
      real :: dP1VBups
      real :: dP2VBups
      real :: dS1VBups
      real :: dS2VBups
      real :: FlxSed(NFlxSed)

      ! local declarations
      integer,parameter   :: ncohort = 9
      integer, save       :: ifirst(2*ncohort) = 0    ! Separate actions required per cohort
      logical, save       :: first = .true.           ! Actions independent of cohort
      logical, save       :: first_handled = .false.  ! Actions independent of cohort - but keep in mind multiple threads
      integer,allocatable, save :: botseg(:)
      real,allocatable, save    :: work(:,:)          ! local workarray, used for items that can be "forgotten" between calls
      integer             :: ipnt(nin+nout)
      real                :: zm, z1, z2, frlay, fbot, tin, a, b, weighCN, weighCP, weighCS, dVBMaxNL, rvb, temp20, &
                             tempcof, maxfMrtVB
      integer             :: ipb, iseg, iq, ifrom, ito, ikmrk1, ikmrk2, itel, ibotseg
      integer, save       :: ilumon

      real, parameter     :: tinyavail = 1.0e-10 ! Avoid division by zero

!
!******************************************************************************* INITIAL PROCESSING

!     retrieve constants

      ipnt = ipoint(1:nin+nout)
      VBType     = NINT(pmsa(ipnt(ip_VBType)))
      DELT       = pmsa(ipnt(ip_Delt))
      SwVegMod   = NINT(pmsa( ipnt(ip_SwVegMod) ))   ! check that this is indeed a constant

      ! set bottom segment number for all (water only in this approach) segments
!$omp critical
      if (first) then
          first_handled = .true.
          CALL GETMLU(ILUMON)

          allocate(botseg(noseg))
          allocate(work(NFlxWat,noseg))
          botseg = -1

           ! set botseg equal to iseg for the segments which have a bottom

          do iseg = 1,noseg
              call dhkmrk(1,iknmrk(iseg),ikmrk1)
              call dhkmrk(2,iknmrk(iseg),ikmrk2)

              if (ikmrk1.lt.3 .and. (ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
                  botseg(iseg) = iseg
              endif

          enddo

          ! loop to find bottom segment in water columns
          do iq = noq1+noq2+noq3, noq1 + noq2 +1, -1
              ifrom   = iexpnt(1,iq)
              ito     = iexpnt(2,iq)
              if ( ifrom .gt. 0 .and. ito .gt. 0 ) then
                  botseg(ifrom) = botseg(ito)
              endif
          enddo
      endif
!$omp end critical

!*** VEG2DN ************************ first part

!*** Initial loop to accumulate available nutrients in the water column

      ! zero the available nutrients pools for all water segments
      ipnt = ipoint(1:nin+nout)
      do iseg = 1 , noseg
          pmsa(ipnt(ip_VBNavail)) = 0.0
          pmsa(ipnt(ip_VBPavail)) = 0.0
          pmsa(ipnt(ip_VBSavail)) = 0.0
          ipnt = ipnt + increm(1:nin+nout)
      enddo

      ! accumulate available nutrients in the water column, to totals in the bottom segment
      ipnt = ipoint(1:nin+nout)
      do iseg = 1 , noseg

          totaldepth = pmsa(ipnt(ip_totaldepth))
          localdepth = pmsa(ipnt(ip_localdepth))
          depth = pmsa(ipnt(ip_depth))
          VegHeVB = pmsa(ipnt(ip_VegHeVB))
          volume = pmsa(ipnt(ip_volume))
          nh4 = max( 0.0, pmsa(ipnt(ip_nh4)) )
          no3 = max( 0.0, pmsa(ipnt(ip_no3)) )
          aap = max( 0.0, pmsa(ipnt(ip_aap)) )
          po4 = max( 0.0, pmsa(ipnt(ip_po4)) )
          so4 = max( 0.0, pmsa(ipnt(ip_so4)) )
          sud = max( 0.0, pmsa(ipnt(ip_sud)) )

          if ( VegHeVB .gt. 0.0 ) then

              VegHeVB = min(VegHeVB,totaldepth)
              zm = totaldepth - VegHeVB
              z1 = localdepth - depth
              z2 = localdepth

              ! bug fixed JvG March 2021 while integrating all VB processes
              if (zm .gt. z2) then
                  ! not in segment:
                  frlay = 0.0
              elseif (zm .lt. z1 ) then
!                 ! partialy in segment:
!                 frlay = (z2-zm)/depth
                  ! completely in segment:
                  frlay = 1.0
              else
!                 ! completely in segment:
!                 frlay = 1.0
                  ! partialy in segment:
                  frlay = (z2-zm)/depth
              endif

              if (frlay.gt.0.0) then
                  ipb = ipoint(ip_VBNavail)+(botseg(iseg)-1)*increm(ip_VBNavail)
                  pmsa(ipb) = pmsa(ipb) + (nh4+no3)*volume*frlay
                  ipb = ipoint(ip_VBPavail)+(botseg(iseg)-1)*increm(ip_VBPavail)
                  pmsa(ipb) = pmsa(ipb) + (aap+po4)*volume*frlay
                  ipb = ipoint(ip_VBSavail)+(botseg(iseg)-1)*increm(ip_VBSavail)
                  pmsa(ipb) = pmsa(ipb) + (so4+sud)*volume*frlay
              endif

          ! End Plant height check
          endif
          ipnt = ipnt + increm(1:nin+nout)
      enddo

!*** Now follows a loop over water segments with a bottom only

      ipnt = ipoint(1:nin+nout)
      work = 0.0

      do iseg = 1 , noseg

!         lowest water and 2d segments only, also dry, ikmrk1 = 0
          call dhkmrk(1,iknmrk(iseg),ikmrk1)
          call dhkmrk(2,iknmrk(iseg),ikmrk2)
          if (ikmrk1.lt.3 .and. (ikmrk2.eq.0).or.(ikmrk2.eq.3)) then

!*** VBSTAT ************************

              SwEmersion = pmsa( ipnt(ip_SwEmersion) )
              nsfVB      = pmsa( ipnt(ip_nsfVB) )
              CrnsfVB    = pmsa( ipnt(ip_CrnsfVB) )
              Initnsfd   = pmsa( ipnt(ip_Initnsfd) )
              CrdepVB    = pmsa( ipnt(ip_CrdepVB) )
              nscdVB     = pmsa( ipnt(ip_nscdVB) )
              InnscdVB   = pmsa( ipnt(ip_InnscdVB) )
              TotalDepth = pmsa( ipnt(ip_TotalDepth) )
              depth      = pmsa( ipnt(ip_Depth) )

              SWVBGro = 1.0
              SWVBMrt = 0.0
              if ( SwVegMod .eq. 0) then
                  if (ifirst(1) .eq. 0) nsfVB = Initnsfd      ! not really understood ...
                  if ( NINT(SwEmersion) .eq. 0 ) then
                      nsfVB = nsfVB + DELT
                      SWVBGro = 0.0
                  else
                      nsfVB = 0
                  endif
                  if (nsfVB .gt. CrnsfVB) then
                      SWVBMrt = 1.0
                  endif
              else
                  if (ifirst(vbtype) .eq. 0) then
                      nscdVB = InnscdVB
                      nsfVB = 0.0
                  endif
                  if ( TotalDepth .gt. CrdepVB ) then
                      nscdVB = nscdVB + DELT
                      SWVBGro = 0.0
                  else
                      nscdVB = 0
                  endif
                  if (nscdVB .gt. CrnsfVB) then
                      SWVBMrt = 1.0
                  endif
              endif
              pmsa( ipnt(ip_SWVBGro)   ) = SWVBGro
              pmsa( ipnt(ip_SWVBMrt)   ) = SWVBMrt
              pmsa( ipnt(ip_nsfVB)   )   = nsfVB
              pmsa( ipnt(ip_nscdVB)   )  = nscdVB

!*** VEG2DN ************************ second part bottom layer only

              ! add sediment pools
              ! S12 assume well-mixed sediment layer, as the DELWAQG module does not export information on the profile

              RootDeVB = pmsa(ipnt(ip_RootDeVB))
              HSed = pmsa(ipnt(ip_HSed))
              surf = pmsa(ipnt(ip_surf))
              PorSed = max( 0.0, pmsa(ipnt(ip_PorSed)) )
              s1_nh4 = max( 0.0, pmsa(ipnt(ip_s1_nh4)) )
              s1_no3 = max( 0.0, pmsa(ipnt(ip_s1_no3)) )
              s1_aap = max( 0.0, pmsa(ipnt(ip_s1_aap)) )
              s1_po4 = max( 0.0, pmsa(ipnt(ip_s1_po4)) )
              s1_so4 = max( 0.0, pmsa(ipnt(ip_s1_so4)) )
              s1_sud = max( 0.0, pmsa(ipnt(ip_s1_sud)) )
              SWRootVB = pmsa(ipnt(ip_SWRootVB))
              VmaxVB = pmsa(ipnt(ip_VmaxVB))
              KmVB = pmsa(ipnt(ip_KmVB))
              ViniVB = pmsa(ipnt(ip_ViniVB))

              fbot = min( 1.0, -RootDeVB / hsed )

              ! add sediment nutrients and convert to g/m2
              ipb = ipnt(ip_VBNavail)
              pmsa(ipb) = (pmsa(ipb) + (s1_nh4+s1_no3)*fbot*surf)/surf
              ipb = ipnt(ip_VBPavail)
              pmsa(ipb) = (pmsa(ipb) + (s1_aap+s1_po4)*fbot*surf)/surf
              ipb = ipnt(ip_VBSavail)
              pmsa(ipb) = (pmsa(ipb) + (s1_so4+s1_sud)*fbot*surf)/surf

              ! RootShoot Model using the Michaelis-Menten eq.
              if ( Nint(SWRootVB) .eq. 1) then
                  ! express the availeble nitrogen conc in sediment as g/m3
                  if (PorSed .gt. 1.0e-10) then
                      TIN = (s1_nh4+s1_no3)/Hsed/PorSed   !  IS THIS INDEED WHAT WAS INTENDED?
                  else
                      TIN = 0.0
                  endif
                  F2VB = ViniVB + (VmaxVB*TIN)/(KmVB + TIN)  ! foliage
                  pmsa(ipnt(ip_F1VB    )) = 0.0
                  pmsa(ipnt(ip_F2VB    )) = F2VB
                  pmsa(ipnt(ip_F3VB    )) = 0.0
                  pmsa(ipnt(ip_F4VB    )) = 0.0
                  pmsa(ipnt(ip_F5VB    )) = 1.0 - F2VB  ! fineroot
              else
                  ! these five factors are supposed to come from the input !  IS THIS INDEED WHAT WAS INTENDED?
              endif

!*** VBGRO ************************

              VB = pmsa(ipnt(ip_VB))
              maxVB = pmsa(ipnt(ip_maxVB))
              minVB = pmsa(ipnt(ip_minVB))
              hlfAgeVB = pmsa(ipnt(ip_hlfAgeVB))
              sfVB = pmsa(ipnt(ip_sfVB))
              dmCfVB = pmsa(ipnt(ip_dmCfVB))
              iniVB = pmsa(ipnt(ip_iniVB))
              iniCovVB = pmsa(ipnt(ip_iniCovVB))
              SWiniVB = nint(pmsa(ipnt(ip_SWiniVB)))
              ageVB = pmsa(ipnt(ip_ageVB))
              VBNavail = pmsa(ipnt(ip_VBNavail))
              VBPavail = pmsa(ipnt(ip_VBPavail))
              VBSavail = pmsa(ipnt(ip_VBSavail))
              VBFrMaxU = pmsa(ipnt(ip_VBFrMaxU))
              F1VB = pmsa(ipnt(ip_F1VB))
              F2VB = pmsa(ipnt(ip_F2VB))
              F3VB = pmsa(ipnt(ip_F3VB))
              F4VB = pmsa(ipnt(ip_F4VB))
              F5VB = pmsa(ipnt(ip_F5VB))
              CNf1VB = pmsa(ipnt(ip_CNf1VB))
              CNf2VB = pmsa(ipnt(ip_CNf2VB))
              CNf3VB = pmsa(ipnt(ip_CNf3VB))
              CNf4VB = pmsa(ipnt(ip_CNf4VB))
              CNf5VB = pmsa(ipnt(ip_CNf5VB))
              CPf1VB = pmsa(ipnt(ip_CPf1VB))
              CPf2VB = pmsa(ipnt(ip_CPf2VB))
              CPf3VB = pmsa(ipnt(ip_CPf3VB))
              CPf4VB = pmsa(ipnt(ip_CPf4VB))
              CPf5VB = pmsa(ipnt(ip_CPf5VB))
              CSf1VB = pmsa(ipnt(ip_CSf1VB))
              CSf2VB = pmsa(ipnt(ip_CSf2VB))
              CSf3VB = pmsa(ipnt(ip_CSf3VB))
              CSf4VB = pmsa(ipnt(ip_CSf4VB))
              CSf5VB = pmsa(ipnt(ip_CSf5VB))
              SWRegrVB = nint(pmsa(ipnt(ip_SWRegrVB)))
              SWVBDec = nint(pmsa(ipnt(ip_SWVBDec)))
              IniAgeVB = pmsa(ipnt(ip_IniAgeVB))
              IniVBDec = pmsa(ipnt(ip_IniVBDec))
              Rc0GWV = pmsa(ipnt(ip_Rc0GWV))
              TcGWV = pmsa(ipnt(ip_TcGWV))
              AcGWV = pmsa(ipnt(ip_AcGWV))
              MinRWV = pmsa(ipnt(ip_MinRWV))
              TBmWV = pmsa(ipnt(ip_TBmWV))
              TempAir = pmsa(ipnt(ip_TempAir))
              MinVBAll = pmsa(ipnt(ip_MinVBAll))
              volume = pmsa(ipnt(ip_volume))

              if (ifirst (vbtype) .eq. 0) then
                   AgeVB = IniAgeVB
              endif

              if (ifirst (vbtype + ncohort) .eq. 0) then
                  SWVBDec = IniVBDec
              endif

!             evaluate use initialisation 0=no, 1=yes
!             always use iniCovVB, 100% (default) means ha=haC, if unequal 100% ha<>haC
              IF (  (SWiniVB .EQ. 1) .or. (SWiniVB .eq. 0 ) ) THEN
                   iniCovVB = iniCovVB / perc_frac
              ELSE
                  CALL ERRSYS ('(no valid value for SWiniVB <0,1>', 1 )
              ENDIF

              IF (  (SWRegrVB .NE. 1) .and. (SWRegrVB .ne. 0 ) ) THEN
                  CALL ERRSYS ('(no valid value for SWRegrVB <0,1>', 1 )
              ENDIF

! ---          if the volume is zero, then the calculations below should be avoided altogether

              if ( volume <= 0.0 ) then
                  cycle
              endif
! ---          only process significant coverages
              if (iniCovVB .gt. 0.001) then


                  if (SwVegMod .eq. 0) then

!                     original VEGMOD

!                     convert Ton DM/hac to gC/m2-cohort
                      iniVB = iniVB / dmCfVB * tonha_gm2
                      minVB = minVB / dmCfVB * tonha_gm2
                      maxVB = maxVB / dmCfVB * tonha_gm2
                      NutLimVB = 1

!                     input shape factor (range 1-10) scaled to halfAge
                      sfVB =  sfVB / hlfAgeVB

!                     biomass at age=0 for later use to check if vegetation is dying after mortality
                      VBAge0ha = (( minVB - maxVB) /(1+exp(sfVB * (- hlfAgeVB))) + maxVB  ) / tonha_gm2

!                     Check values growth limitation (only 0 or 1)
                      IF ( (NINT(SWVBGro) .NE. 1) .and. (NINT(SWVBGro) .NE. 0) ) THEN
                          CALL ERRSYS ('(no valid value for SWVBGroVB <0,1>', 1 )
                      ENDIF

!                     Check values mort limitation (only 0 or 1)
                      IF ( (NINT(SWVBMrt) .NE. 1) .and. (NINT(SWVBMrt) .NE. 0) ) THEN
                          CALL ERRSYS ('(no valid value for SWVBMrtVB <0,1>', 1 )
                      ENDIF

!                     Checking for nut availabilithy

                      if ( (F4VB + F5VB) - 1.E-10 .lt. 0.0 ) then
                          CALL ERRSYS ('(no valid values for F4VB and F5VB (allocation factors vegetation  roots)', 1 )
                      else
!                         average Nutrient content of cohort
                          weighCN = F1VB*CNf1VB + F2VB*CNf2VB + F3VB*CNf3VB + F4VB*CNf4VB + F5VB*CNf5VB
                          weighCP = F1VB*CPf1VB + F2VB*CPf2VB + F3VB*CPf3VB + F4VB*CPf4VB + F5VB*CPf5VB
                          weighCS = F1VB*CSf1VB + F2VB*CSf2VB + F3VB*CSf3VB + F4VB*CSf4VB + F5VB*CSf5VB

                          dVBMaxNL = VBFrMaxU * min( VBNavail  * weighCN, VBPavail  * weighCP, VBSavail  * weighCS) * IniCovVB / volume * surf / delt
                          dVBMaxNL = max(0.0,dVBMaxNL)

                      endif

!                     Evaluate initial conditions at start of simulation
                      IF ( SWiniVB .NE. 0 ) THEN

!                         calculate age matching initial biomass
!                         check and maximise age to 2xhalfage

!                         initial biomass exceeds minimum
                          IF ( (iniVB - minVB) .gt. 1.E-10) then
!                             initial biomass less than maximum biomass
                              IF ( (iniVB/maxVB) .lt. 0.99) THEN
                                  ageVB  = hlfAgeVB + LOG((minVB-maxVB) /(iniVB-maxVB) - 1 ) / sfVB
                              ELSE
                                  WRITE (ILUMON, *) 'WARNING : Vegtype ',vbtype, ' init biom .ge. Max: ', &
                                      iniVB*dmcfVB/100, '>=',maxVB*dmcfVB/100
!                                 age representing 99% of initial mass
                                  ageVB= hlfAgeVB + LOG((minVB-maxVB) /( (0.99 - 1) * maxVB) - 1 ) / sfVB
                             ENDIF
                         ELSE
                             ageVB =0.0
                         ENDIF

                         VBAha = (( minVB - maxVB) /(1+exp(sfVB * (ageVB - hlfAgeVB))) + maxVB  ) / tonha_gm2

                         VBha  = VB / iniCovVB / tonha_gm2
                         dVB   = ( VBAha - VBha ) * tonha_gm2 * surf / volume / delt * iniCovVB
                      ELSE

                          VBha  = VB / iniCovVB / tonha_gm2

!                         vegetation is flooded
                          if ( NINT (SWVBMrt) .eq. 1 ) then
                              SWVBDec = 1

                          endif

!                         check if vegetation died long enough - regrowth allowed again
                          if  ( (VBha .lt. VBAge0ha) .and. (SWVBDec .eq. 1) .and.  SWRegrVB .eq. 1 )  then
                              SWVBDec = 0
                              ageVB = 0

                          endif

!                         calculate new age for decaying vegetation based on current biomass
!                         convert VBha (Tc/ha-cohort) to maxVB (gC/m2-cohort)
                          if ( SWVBDec .eq. 1) then
                              if ( VBha .le. VBAge0ha ) then
                                  ageVB = 0
                              elseif ( (VBha* tonha_gm2/maxVB) .lt. 0.99 ) then
                                  ageVB  = hlfAgeVB + LOG((minVB-maxVB) /(VBha*tonha_gm2-maxVB) - 1 ) / sfVB
                              else
!                                 cannot calc age
                              endif
                          endif

!                         calculate attainable biomass per ha using age
                          VBAha = (( minVB - maxVB) /(1+exp(sfVB * (ageVB - hlfAgeVB))) + maxVB  ) / tonha_gm2

!                         no growth for standing stock either
                          IF ( ( NINT(SWVBGro) .EQ. 1) .and. (SWVBDec .eq. 0) .and. SWRegrVB .eq. 1) THEN

                              dVB   = ( VBAha - VBha ) * tonha_gm2 * surf / volume / delt * iniCovVB
!                             growth reduction?
                              if (dVB .gt. 1.E-20) then
                                  NutLimVB = min (1.0, (dVBMaxNL / dVB) )
                              else
                                  NutLimVB = 1
                              endif
                              dVB = min(dVBMaxNL, dVB)

!                             there is (reduced?) growth
                              ageVB = ageVB + NutLimVB * DELT

                          ELSE
!                             no growth flux = 0
                              dVB=0
                          ENDIF
                      ENDIF

                      ! if init then no nutrient uptake, set output to zero
                      if ( SWiniVB .NE. 0 ) then
                          fVB = 0.0
                      else
                          fVB = dVB * volume / surf
                      endif

                      ! end original vegmod
                  else

!                     =========================
!                     Wetland vegetation growth
!                     =========================

!                     Attainable biomass averaged over segment
                      VBAha = TBmWV / dmCfVB * iniCovVB

                      if (ifirst (vbtype) .eq. 0 .and. SWiniVB .eq. 1) then
!                         Initialise biomass using target biomass and % coverage without nutrient uptake
!                         Current density within covered area
                          VBha  = TBmWV / dmCfVB
!                         Initialise by % times TBmWV
                          dVB = (VBAha * tonha_gm2 - VB) / volume * surf / DELT
                          fVB = 0.0
!                         flux for nutrient uptake is zero
                          NutLimVB = 1.0
!                         No actual growth rate can be calulated...
                          rGWV = 0.0
                      else if (VB .lt. MinVBAll) then
!                         Very low/no biomass for species with significant coverage
!                         Current density within covered area
                          VBha  = MinVB / tonha_gm2 / iniCovVB
!                         Initialise biomass using general minimum biomass without nutrient uptake
                          dVB = (MinVBAll - VB) / volume * surf / DELT
                          fVB = 0.0
!                         flux for nutrient uptake is zero
                          NutLimVB = 1.0
!                         No actual growth rate can be calulated...
                          rGWV = 0.0
                      else
!                         Calculate actual growth
!                         Current density within covered area
                          VBha  = VB / tonha_gm2 / iniCovVB

!                         Ratio of current biomass to attainable biomass
                          if ( VBAha > 0.0 ) then
                              rVB = VB / (VBAha * tonha_gm2)
                          else
                              rVB = merge( 1.0, 0.0, VB > 0.0 )
                          endif

                          if (rVB .lt. 1.0 .and. NINT(SWVBGro) .eq. 1 .and. NINT (SWVBMrt) .eq. 0) then

!                             average Nutrient content of vegetation
                              weighCN = F1VB*CNf1VB + F2VB*CNf2VB + F3VB*CNf3VB + F4VB*CNf4VB + F5VB*CNf5VB
                              weighCP = F1VB*CPf1VB + F2VB*CPf2VB + F3VB*CPf3VB + F4VB*CPf4VB + F5VB*CPf5VB
                              weighCS = F1VB*CSf1VB + F2VB*CSf2VB + F3VB*CSf3VB + F4VB*CSf4VB + F5VB*CSf5VB

                              dVBMaxNL = VBFrMaxU * min( VBNavail  * weighCN, VBPavail  * weighCP, VBSavail  * weighCS) &
                                             * IniCovVB / volume * surf / delt
                              dVBMaxNL = max(0.0,dVBMaxNL)

                              TEMP20 = TempAir - 20.0
                              TempCof = TcGWV ** TEMP20

                              if (rVB .lt. MinRWV) then
                                  dVB = VB * AcGWV * Rc0GWV * TempCof / volume * surf
                              else
                                  dVB = VB * Rc0GWV * TempCof / volume * surf
                              endif

!                             growth reduction?
                              if (dVB .gt. 1.E-20) then
                                  NutLimVB = min (1.0, (dVBMaxNL / dVB) )
                              else
                                  NutLimVB = 1.0
                              endif
                              dVB = min(dVBMaxNL, dVB)
!                             flux for nutrient uptake
                              fVB = dVB * volume / surf
                          else
                              dVB = 0.0
                              fVB = 0.0
                              NutLimVB = 1.0
                          endif
                      endif
                      if (VB .gt. MinVBAll) then
                          rGWV = dVB / VB
                      else
!                         No actual growth rate can be calulated...
                          rGWV = 0.0
                      endif
                      VBAge0ha = 0.0
                      SWVBDec = 0

                      ! end wetland vegetation
                  end if
!
                  fl  ( If_dVB + (iseg-1)*noflux ) = dVB                              ! g/m3/d
                  pmsa( ipnt(ip_ageVB)   ) = ageVB
                  pmsa( ipnt(ip_VBha)   ) = VBha
                  pmsa( ipnt(ip_VBAha)   ) = VBAha
                  pmsa( ipnt(ip_rGWV)   ) = rGWV
                  pmsa( ipnt(ip_fVB)   ) = fVB
                  pmsa( ipnt(ip_VBAge0ha)   ) = VBAge0ha
                  pmsa( ipnt(ip_SWVBDec)   ) = real(SWVBDec)
                  pmsa( ipnt(ip_NutLimVB)   ) = NutLimVB

! ---             end check iniCovVB - only significant vegetation

              else
                  fvb = 0.0 ! Possible inconsistency ...
              endif

!*** VBUPT *************************

!
!             all input should be covered by earlier statements
              fNVBup  = 0.0
              fPVBup  = 0.0
              fSVBup  = 0.0

              if ( Nint(SwVBGro) .eq. 1) then

    !             make sure allocation factors for roots > 0

                  if ( (F4VB + F5VB) - 1.E-10 .lt. 0.0 ) then
                      CALL ERRSYS ('(no valid values for F4VB and F5VB (alloction factors vegetation  roots)', 1 )
                  else
    !                 average Nutrient content of cohort
                      weighCN = F1VB*CNf1VB + F2VB*CNf2VB + F3VB*CNf3VB + F4VB*CNf4VB + F5VB*CNf5VB
                      weighCP = F1VB*CPf1VB + F2VB*CPf2VB + F3VB*CPf3VB + F4VB*CPf4VB + F5VB*CPf5VB
                      weighCS = F1VB*CSf1VB + F2VB*CSf2VB + F3VB*CSf3VB + F4VB*CSf4VB + F5VB*CSf5VB

    !                 calculate 2D nutrient uptake flux
                      fNVBup  = fVB / weighCN
                      fPVBup  = fVB / weighCP
                      fSVBup  = fVB / weighCS
                  endif

    !         evaluate SwVBGro
              endif
              pmsa( ipnt(ip_fNVBup)   ) =  fNVBup
              pmsa( ipnt(ip_fPVBup)   ) =  fPVBup
              pmsa( ipnt(ip_fSVBup)   ) =  fSVBup

!*** VBMRT *************************

              !Most input already defined
              FfolPOC1 = pmsa(ipnt(ip_FfolPOC1))
              FfolPOC2 = pmsa(ipnt(ip_FfolPOC2))
              FfrootPOC1 = pmsa(ipnt(ip_FfrootPOC1))
              FfrootPOC2 = pmsa(ipnt(ip_FfrootPOC2))
              RcMrtVB = pmsa(ipnt(ip_RcMrtVB))
              Rc0MSWV = pmsa(ipnt(ip_Rc0MSWV))
              TcMSWV = pmsa(ipnt(ip_TcMSWV))
              RcMGRWV = pmsa(ipnt(ip_RcMGRWV))
              AcMWV = pmsa(ipnt(ip_AcMWV))
              MaxRWV = pmsa(ipnt(ip_MaxRWV))

              if ( ( NINT (SwVBMrt) .eq. 1) .or. ( SWVBDec .eq. 1)) then
    !             inundation mortality
                  rMrtVB = RcMrtVB
              else
    !             no inundation mortality
                  rMrtVB = 0.0
              endif

              if (SwVegMod .eq. 1) then
    !             if wetland vegetation model, also turnover due to senescence mortality and grazing
    !             Ratio of current biomass to attainable biomass
                  rVB = VB / ((TBmWV + tiny(TBmWV)) * tonha_gm2)
                  TEMP20 = TempAir - 20.0
                  TempCof = TcMSWV ** TEMP20

                  If (rVB .gt. MaxRWV) then
    !                 Add accelarated senescence mortality with biomass above target biomass
                      rMrtVB = rMrtVB + AcMWV * Rc0MSWV * TempCof
                  else
    !                 Add normal senescence mortality
                      rMrtVB = rMrtVB + Rc0MSWV * TempCof
                  end if
                  maxfMrtVB = MAX (0.0, (VB - MinVBAll) / DELT )
                  fMrtVB = MIN(rMrtVB * VB + RcMGRWV, maxfMrtVB)
              else
                 fMrtVB = rMrtVB * VB
              end if

              flxsed = 0.0
    !         check if vegetation cohort is dead or still dying off or we use the Wetland Vegetation option
              if ( ( ( NINT (SwVBMrt) .eq. 1) .or. ( SWVBDec .eq. 1) .or. (SwVegMod .eq. 1) ) .and. (fMrtVB .gt. 0.0) ) then

    !             calculate 2D fluxes of vegetation compartments
    !             seems redundant to split C-flux by veg. compartment
    !             when different lag times per compartment are introduced this is needed
    !             moreover useful for balance output

    !             Fluxes for state var VB

    !             C-flux for stem, branch and root
    !             fluxes to sediment can be defined right away
    !             fluxes to water are saved per m2 for later distribution

                  ! C water
                  dMrtC1VB = fMrtVB * F1VB / DEPTH
                  dMrtC2VB = fMrtVB * F2VB / DEPTH
                  dMrtC3VB = fMrtVB * F3VB / DEPTH
                  ! C sediment
                  dMrtC4VB = fMrtVB * F4VB / DEPTH
                  dMrtC5VB = fMrtVB * F5VB / DEPTH

    !             stem all to POX5
                  work( 1,iseg) = fMrtVB * F1VB
                  work( 2,iseg) = fMrtVB * F1VB / CNf1VB
                  work( 3,iseg) = fMrtVB * F1VB / CPf1VB
                  work( 4,iseg) = fMrtVB * F1VB / CSf1VB

    !             from foliage to POX 1-2-3 for C-N-P-S

                  work( 5,iseg) = fMrtVB * F2VB * FfolPOC1
                  work( 6,iseg) = fMrtVB * F2VB * FfolPOC2
                  work( 7,iseg) = fMrtVB * F2VB * (1.0 - FfolPOC1 -FfolPOC2)
                  work( 8,iseg) = fMrtVB * F2VB * FfolPOC1 / CNf2VB
                  work( 9,iseg) = fMrtVB * F2VB * FfolPOC2 / CNf2VB
                  work(10,iseg) = fMrtVB * F2VB * (1 - FfolPOC1 -FfolPOC2) / CNf2VB
                  work(11,iseg) = fMrtVB * F2VB * FfolPOC1 / CPf2VB
                  work(12,iseg) = fMrtVB * F2VB * FfolPOC2 / CPf2VB
                  work(13,iseg) = fMrtVB * F2VB * (1 - FfolPOC1 -FfolPOC2) / CPf2VB
                  work(14,iseg) = fMrtVB * F2VB * FfolPOC1 / CSf2VB
                  work(15,iseg) = fMrtVB * F2VB * FfolPOC2 / CSf2VB
                  work(16,iseg) = fMrtVB * F2VB * (1 - FfolPOC1 -FfolPOC2) / CSf2VB

                  ! branch all to POX5 up to here g/m2/d
                  work(17,iseg) = fMrtVB * F3VB
                  work(18,iseg) = fMrtVB * F3VB / CNf3VB
                  work(19,iseg) = fMrtVB * F3VB / CPf3VB
                  work(20,iseg) = fMrtVB * F3VB / CSf3VB

                  ! roots  all to POX5 from here g/m3/d
                  FlxSed( 1) = dMrtC4VB
                  FlxSed( 2) = dMrtC4VB / CNf4VB
                  FlxSed( 3) = dMrtC4VB / CPf4VB
                  FlxSed( 4) = dMrtC4VB / CSf4VB

                  ! fine rootsto POX 1-2-3 for C-N-P-S
                  FlxSed( 5) = dMrtC5VB * FfrootPOC1
                  FlxSed( 6) = dMrtC5VB * FfrootPOC2
                  FlxSed( 7) = dMrtC5VB * (1.0 - FfrootPOC1 -FfrootPOC2)
                  FlxSed( 8) = dMrtC5VB * FfrootPOC1 / CNf5VB
                  FlxSed( 9) = dMrtC5VB * FfrootPOC2 / CNf5VB
                  FlxSed(10) = dMrtC5VB * (1.0 - FfrootPOC1 -FfrootPOC2) / CNf5VB
                  FlxSed(11) = dMrtC5VB * FfrootPOC1 / CPf5VB
                  FlxSed(12) = dMrtC5VB * FfrootPOC2 / CPf5VB
                  FlxSed(13) = dMrtC5VB * (1.0 - FfrootPOC1 -FfrootPOC2) / CPf5VB
                  FlxSed(14) = dMrtC5VB * FfrootPOC1 / CSf5VB
                  FlxSed(15) = dMrtC5VB * FfrootPOC2 / CSf5VB
                  FlxSed(16) = dMrtC5VB * (1.0 - FfrootPOC1 -FfrootPOC2) / CSf5VB
    !
    !          cohort not dead or decaying
              else
                  dMrtC1VB    = 0.0
                  dMrtC3VB    = 0.0
                  dMrtC4VB    = 0.0
                  dMrtC2VB    = 0.0
                  dMrtC5VB    = 0.0
              endif

              fl  ( if_dMrtC1VB + (iseg-1)*noflux ) = dMrtC1VB
              fl  ( if_dMrtC2VB + (iseg-1)*noflux ) = dMrtC2VB
              fl  ( if_dMrtC3VB + (iseg-1)*noflux ) = dMrtC3VB
              fl  ( if_dMrtC4VB + (iseg-1)*noflux ) = dMrtC4VB
              fl  ( if_dMrtC5VB + (iseg-1)*noflux ) = dMrtC5VB

              do itel = 1,NFlxSed
!                 fl  ( if_firstSed + itel + (iseg-1)*noflux ) = FlxSed(itel)
                  pmsa(ipnt(ip_offsetrfl+itel)) = FlxSed(itel)*depth
              enddo
              pmsa( ipnt(ip_rMrtVB)   ) = rMrtVB
              pmsa( ipnt(ip_fMrtVB)   ) = fMrtVB

!         end bottom and 2d segments only
          endif

          ! update pmsa pointers
          ipnt        = ipnt        + increm(1:nin+nout)
!
!     end MAIN segment loop for BOTTOM SEGMENTS only
      enddo

!**** LOOP TO ORGANISE DISTRIBUTION OF MORTALITY FLUXES OVER THE WATER COLUMN

      ipnt = ipoint(1:nin+nout)
      do iseg = 1 , noseg

!*** VEG3DX ************************

          depth = pmsa(ipnt(ip_depth))
          totaldepth = pmsa(ipnt(ip_totaldepth))
          localdepth = pmsa(ipnt(ip_localdepth))
          SwDisVB = nint(pmsa(ipnt(ip_SwDisVB)))
          VegHeVB = pmsa(ipnt(ip_VegHeVB))
          FFacVB = pmsa(ipnt(ip_FFacVB))

          call dhkmrk(1,iknmrk(iseg),ikmrk1)
          call dhkmrk(2,iknmrk(iseg),ikmrk2)
          if (ikmrk1.lt.3) then ! also when dry!
              if ( VegHeVB .gt. 0.0 ) then

                  ! distribution over the water segments

                  VegHeVB = min(VegHeVB,totaldepth)
                  zm = totaldepth - VegHeVB
                  z1 = localdepth - depth
                  z2 = localdepth

                  ! switch = 1:  constant biomass distribution

                  if (SwDisVB .eq. 1 ) then
                      FFacVB = 1
                  endif

                  a = (2. - (2. * FFacVB)) / (totaldepth - zm) / VegHeVB
                  b = (FFacVB * (zm + totaldepth) - 2. * zm) / (totaldepth - zm) / VegHeVB

                  if (zm .gt. z2) then
                      ! macrophyte is not in segment:
                      frlay = 0
                  elseif (zm .lt. z1 ) then
                      ! macropyhte is partialy in segment:
                      frlay = (a/2)  * (z2*z2 - z1*z1) + b * (z2 - z1)
                  else
                      ! macropyhte is completely in segment:
                      frlay = (a/2)  * (z2*z2 - zm*zm) + b * (z2 - zm)
                  endif

            ! end  Height > 0
              else
                  frlay = 0.0
              endif
              ibotseg = botseg(iseg)
              if (depth.gt.0.0) then
                  do itel = 1, NFlxWat
                      fl(if_firstWat + itel + (iseg-1)*noflux) = work(itel,ibotseg)*frlay/depth
                  enddo
              else
                  do itel = 1, NFlxWat
                      fl(if_firstWat + itel + (iseg-1)*noflux) = 0.0
                  enddo
              endif

          ! end attribute test
          endif
          ipnt  = ipnt  + increm(1:nin+nout)
      enddo

!**** LOOP TO ORGANISE DISTRIBUTION OF UPTAKE FLUXES OVER THE WATER COLUMN AND SEDIMENT
!*** VEG3DU ************************


      ipnt  = ipoint(1:nin+nout)
      do iseg = 1 , noseg

          dN1VBupw = 0.0
          dN2VBupw = 0.0
          dP1VBupw = 0.0
          dP2VBupw = 0.0
          dS1VBupw = 0.0
          dS2VBupw = 0.0
          dN1VBups = 0.0
          dN2VBups = 0.0
          dP1VBups = 0.0
          dP2VBups = 0.0
          dS1VBups = 0.0
          dS2VBups = 0.0

          call dhkmrk(1,iknmrk(iseg),ikmrk1)
          call dhkmrk(2,iknmrk(iseg),ikmrk2)

          depth = pmsa(ipnt(ip_depth))
          totaldepth = pmsa(ipnt(ip_totaldepth))
          localdepth = pmsa(ipnt(ip_localdepth))
          VegHeVB = pmsa(ipnt(ip_VegHeVB))
          delt = pmsa(ipnt(ip_delt))
          nh4 = max( 0.0, pmsa(ipnt(ip_nh4)) )
          no3 = max( 0.0, pmsa(ipnt(ip_no3)) )
          aap = max( 0.0, pmsa(ipnt(ip_aap)) )
          po4 = max( 0.0, pmsa(ipnt(ip_po4)) )
          so4 = max( 0.0, pmsa(ipnt(ip_so4)) )
          sud = max( 0.0, pmsa(ipnt(ip_sud)) )

          ibotseg = botseg(iseg)
          VBNavail = pmsa(ipoint(ip_VBNavail)+(ibotseg-1)*increm(ip_VBNavail))
          VBPavail = pmsa(ipoint(ip_VBPavail)+(ibotseg-1)*increm(ip_VBPavail))
          VBSavail = pmsa(ipoint(ip_VBSavail)+(ibotseg-1)*increm(ip_VBSavail))
          fNVBup   = pmsa(ipoint(ip_fNVBup  )+(ibotseg-1)*increm(ip_fNVBup))
          fPVBup   = pmsa(ipoint(ip_fPVBup  )+(ibotseg-1)*increm(ip_fPVBup))
          fSVBup   = pmsa(ipoint(ip_fSVBup  )+(ibotseg-1)*increm(ip_fSVBup))

          ! copy of earlier code used to derive available nutrients
          if ( VegHeVB .gt. 0.0 ) then

              VegHeVB = min(VegHeVB,totaldepth)
              zm = totaldepth - VegHeVB
              z1 = localdepth - depth
              z2 = localdepth

              ! bug fixed JvG March 2021 while integrating all VB processes
              if (zm .gt. z2) then
                  ! not in segment:
                  frlay = 0.0
              elseif (zm .lt. z1 ) then
                  ! completely in segment:
                  frlay = 1.0
!                 ! partialy in segment:
!                 frlay = (z2-zm)/depth
              else
                  ! partialy in segment:
                  frlay = (z2-zm)/depth
!                 ! completely in segment:
!                 frlay = 1.0
              endif

              if (frlay.gt.0.0) then
                  dN1VBupw =  nh4*depth*frlay/max(tinyavail,vbnavail)*fnvbup/depth
                  dN2VBupw =  no3*depth*frlay/max(tinyavail,vbnavail)*fnvbup/depth
                  dP1VBupw =  aap*depth*frlay/max(tinyavail,vbpavail)*fpvbup/depth
                  dP2VBupw =  po4*depth*frlay/max(tinyavail,vbpavail)*fpvbup/depth
                  dS1VBupw =  so4*depth*frlay/max(tinyavail,vbsavail)*fsvbup/depth
                  dS2VBupw =  sud*depth*frlay/max(tinyavail,vbsavail)*fsVBup/depth
              endif

          ! End Plant height check
          endif

          ! this segment has a bottom
          if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then

              s1_nh4 = max( 0.0, pmsa(ipnt(ip_s1_nh4)) )
              s1_no3 = max( 0.0, pmsa(ipnt(ip_s1_no3)) )
              s1_aap = max( 0.0, pmsa(ipnt(ip_s1_aap)) )
              s1_po4 = max( 0.0, pmsa(ipnt(ip_s1_po4)) )
              s1_so4 = max( 0.0, pmsa(ipnt(ip_s1_so4)) )
              s1_sud = max( 0.0, pmsa(ipnt(ip_s1_sud)) )
              RootDeVB = pmsa(ipnt(ip_RootDeVB))
              hsed = pmsa(ipnt(ip_hsed))
              surf = pmsa(ipnt(ip_surf))

              ! again copy availability calculation

              fbot = min( 1.0, -RootDeVB / hsed )

              dN1VBups =  s1_nh4*fbot/max(tinyavail,vbnavail)*fnvbup/depth
              dN2VBups =  s1_no3*fbot/max(tinyavail,vbnavail)*fnvbup/depth
              dP1VBups =  s1_aap*fbot/max(tinyavail,vbpavail)*fpvbup/depth
              dP2VBups =  s1_po4*fbot/max(tinyavail,vbpavail)*fpvbup/depth
              dS1VBups =  s1_so4*fbot/max(tinyavail,vbsavail)*fsvbup/depth
              dS2VBups =  s1_sud*fbot/max(tinyavail,vbsavail)*fsvbup/depth
          endif

          ! STORE FLUXES
          !
          fl(if_dN1VBupw + (iseg-1)*noflux) = dN1VBupw
          fl(if_dN2VBupw + (iseg-1)*noflux) = dN2VBupw
          fl(if_dP1VBupw + (iseg-1)*noflux) = dP1VBupw
          fl(if_dP2VBupw + (iseg-1)*noflux) = dP2VBupw
          fl(if_dS1VBupw + (iseg-1)*noflux) = dS1VBupw
          fl(if_dS2VBupw + (iseg-1)*noflux) = dS2VBupw
!          fl(if_dN1VBups + (iseg-1)*noflux) = dN1VBups
!          fl(if_dN2VBups + (iseg-1)*noflux) = dN2VBups
!          fl(if_dP1VBups + (iseg-1)*noflux) = dP1VBups
!          fl(if_dP2VBups + (iseg-1)*noflux) = dP2VBups
!          fl(if_dS1VBups + (iseg-1)*noflux) = dS1VBups
!          fl(if_dS2VBups + (iseg-1)*noflux) = dS2VBups
          pmsa(ipnt(ip_offsetufl+1)) = dN1VBups*depth
          pmsa(ipnt(ip_offsetufl+2)) = dN2VBups*depth
          pmsa(ipnt(ip_offsetufl+3)) = dP1VBups*depth
          pmsa(ipnt(ip_offsetufl+4)) = dP2VBups*depth
          pmsa(ipnt(ip_offsetufl+5)) = dS1VBups*depth
          pmsa(ipnt(ip_offsetufl+6)) = dS2VBups*depth

          ipnt  = ipnt  + increm(1:nin+nout)

      enddo

!$omp critical
      if ( first_handled ) then
          first = .false.
      endif
!$omp end critical

      ! THIS IS LTERALLY COPIED FROM PRE_EXISTING CODE, WOULD THIS WORK??
      ! from Status
      if (SwVegMod.eq. 0) then
          ifirst (1) = 1              ! Why??
      else
          ifirst (vbtype) = 1
      end if

      ! From GRO
      pmsa(ipoint(ip_SWiniVB)) = 0.0 ! Dubious ...
      ifirst (vbtype) = 1
      ifirst (vbtype+ncohort) = 1

      return
      end
