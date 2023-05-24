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


  ! 6 char name for process mathc with second line of PDF
subroutine PROSED     ( pmsa   , fl     , ipoint , increm, noseg , &
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &
                            noq3   , noq4   )
!
!*******************************************************************************
!

use m_dhkmrk
    use protist_constants
    IMPLICIT NONE
!
!     Type    Name         I/O Description
!
    real(4) pmsa(*)      ! I/O Process Manager System Array, window of routine to process library
    real(4) fl(*)        ! O  Array of fluxes made by this process in mass/volume/time
    integer ipoint(*)    ! I  Array of pointers in pmsa to get and store the data
    integer increm(*)    ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
    integer noseg        ! I  Number of computational elements in the whole model schematisation
    integer noflux       ! I  Number of fluxes, increment in the fl array
    integer iexpnt(4,*)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
    integer iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
    integer noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
    integer noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
    integer noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
    integer noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
!     support variables
    integer, parameter    :: nrIndInp = 6     !   nr of species independent input items
    integer, parameter    :: nrSpecInp = 7    !   nr of inputs per species
    integer, parameter    :: nrSpecOut = 6    !   nr of outputs per species
    integer               :: nrInputItems     !   nr of input items need for output PMSA
    integer               :: nrOutputItems    !   nr of output items need for output PMSA
    integer               :: ipointLength     !   total length of the PMSA input and output pointer array
    integer, allocatable  :: ipnt(:)          !   Local work array for the pointering

    integer iseg          ! Local loop counter for computational element loop
    integer ioq
    integer iflux
    integer ikmrk1        ! first segment attribute
    integer ikmrk2        ! second segment attribute
    integer ikmrkv
    integer ikmrkn

    integer i_origin, i_dest

    integer iSpec         ! local species number counter
    integer spInc         ! local species PMSA/FL number increment
    integer inpItems      ! nr of input items need for output PMSA

     !input parameters
     integer nrSpec       ! total nr species implemented in process (from proc_def)
     real    DELT, MinDepth, TaucSDiat                  ! segment independent input
     real    Tau, Depth                                 ! segment dependent input
     real    ZSedDiat, VSedDiat                         ! species dependent input
     real    Depth2, MinDepth2

     ! input state variables
     real    diatC, diatChl, diatN, diatP, diatSi        ! protist state variable

     ! auxiliaries
     real     PSed
     real     MaxSed_C, MaxSed_Chl, MaxSed_N, MaxSed_P, MaxSed_Si
     real     PotSed_C, PotSed_Chl, PotSed_N, PotSed_P, PotSed_Si


!
!*******************************************************************************
!
    ! segment and species independent items
    nrSpec = nint(PMSA(ipoint(   1 )))   !   nr of species in the interface                                 (-)

!   nrInputItems  = nrIndInp + nrSpec * nrSpecInp = 6 + 2 * 7 = 20
!   nrOutputItems = nrSpec * nrSpecOut = 2 * 6 = 12
!   ipointLength = nrInputItems + nrOutputItems = 20 + 12 = 32
!   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * 5 = 10

    ! length of the PMSA input pointer array.
    nrInputItems = nrIndInp + nrSpec * nrSpecInp     ! only input on segments, not on exchanges!
    nrOutputItems = nrSpec * nrSpecOut               ! only output on segments, not on exchanges!
    ipointLength = nrInputItems + nrSpec + nrOutputItems + nrSpec ! includes one input and one output per species on exchanges

    allocate (ipnt(ipointLength))
    ipnt(1:ipointLength) = ipoint(1:ipointLength)
    iflux = 0

    ! segment and species independent items
    DELT       = PMSA(ipnt(   2 ))         !   timestep for processes                                 (d)
    MinDepth   = PMSA(ipnt(   3 ))         !   minimum waterdepth for sedimentation/resuspension      (m)
    TaucSDiat  = PMSA(ipnt(   4 ))         !   critical shear stress for sedimentation Diatoms        (N/m2)

    ! length of the PMSA input array (for segment and for exchange).
    ! first and second term = input for segment; third term input for exchange
    inpItems = nrIndInp + nrSpec * nrSpecInp

    ! segment loop
    do iseg = 1 , noseg

        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then
            call dhkmrk(2,iknmrk(iseg),ikmrk2)
            if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
                ! species independent items
                Tau         = PMSA(ipnt(   5 ))  !    total bottom shear stress                              (N/m2)
                Depth       = PMSA(ipnt(   6 ))  !    depth of segment                                       (m)

                ! species loop
                do iSpec = 1, nrSpec

                    spInc = nrIndInp + (iSpec - 1) * nrSpecInp

                    ! species dependent items
                    ! (number of species independent items + location of input item in vector + species loop)
                    ! Protect against negativ values
                    diatC        = max( 0.0 , PMSA(ipnt( spInc + 1 )) )  !      C-biomass                                              (gC m-3)
                    diatChl      = max( 0.0 , PMSA(ipnt( spInc + 2 )) )  !      Chl-biomass                                            (gChl m-3)
                    diatN        = max( 0.0 , PMSA(ipnt( spInc + 3 )) )  !      N-biomass                                              (gN m-3)
                    diatP        = max( 0.0 , PMSA(ipnt( spInc + 4 )) )  !      P-biomass                                              (gP m-3)
                    diatSi       = max( 0.0 , PMSA(ipnt( spInc + 5 )) )  !      Si-biomass                                             (gSi m-3)
                    ZSedDiat     =            PMSA(ipnt( spInc + 6 ))    !      zeroth-order sedimentation flux Diatoms                (gC/m2/d)
                    VSedDiat     = max( 0.0 , PMSA(ipnt( spInc + 7 )) )  !      sedimentation velocity Diatoms                         (m/d)

                    ! Calculate sedimentation probabality-------------------------------------------------------------------------------
                    ! Units: (-)
                    if (Tau .eq. -1.0) then
                        PSed = 1.0
                    elseif (TaucSDiat .lt. 1e-20 )  then
                        PSed = 0.0
                    else
                        ! comapre with critical shear stress
                        PSed = max ( 0.0, (1.0 - Tau/TaucSDiat) )
                    endif
                    ! changed PSedMin to 0.0
                    PSed = max( 0.0, PSed )

                    ! Calculate potential sediment fluxes-------------------------------------------------------------------------------
                    ! Units:

                    spInc = iflux + nrLossFluxes * (iSpec - 1)

                    if (Depth .lt. MinDepth) then
                        MaxSed_C   = 0.0
                        MaxSed_Chl = 0.0
                        MaxSed_N   = 0.0
                        MaxSed_P   = 0.0
                        MaxSed_Si  = 0.0

                        fl( spInc + 1) =  0.0
                        fl( spInc + 2) =  0.0
                        fl( spInc + 3) =  0.0
                        fl( spInc + 4) =  0.0
                        fl( spInc + 5) =  0.0
                    else
                        PotSed_C   = ZSedDiat + ( VSedDiat * diatC   ) * PSed
                        PotSed_Chl = ZSedDiat + ( VSedDiat * diatChl ) * PSed
                        PotSed_N   = ZSedDiat + ( VSedDiat * diatN   ) * PSed
                        PotSed_P   = ZSedDiat + ( VSedDiat * diatP   ) * PSed
                        PotSed_Si  = ZSedDiat + ( VSedDiat * diatSi  ) * PSed

                        ! limit sedimentation to available mass (m/l2/day)
                        MaxSed_C   = min (PotSed_C  , diatC   / DELT * Depth)
                        MaxSed_Chl = min (PotSed_Chl, diatChl / DELT * Depth)
                        MaxSed_N   = min (PotSed_N  , diatN   / DELT * Depth)
                        MaxSed_P   = min (PotSed_P  , diatP   / DELT * Depth)
                        MaxSed_Si  = min (PotSed_Si , diatSi  / DELT * Depth)

                        ! convert sedimentation to flux
                        ! Units: m/l3/day
                        fl( spInc + 1) =  MaxSed_C / Depth
                        fl( spInc + 2) =  MaxSed_Chl / Depth
                        fl( spInc + 3) =  MaxSed_N   / Depth
                        fl( spInc + 4) =  MaxSed_P   / Depth
                        fl( spInc + 5) =  MaxSed_Si  / Depth
                    endif

                    ! Output -------------------------------------------------------------------
                    ! (input items + input exchange + position of specific output item in vector + species loop * total number of output)
                    spInc = inpItems + nrSpec + (iSpec - 1) * nrSpecOut

                    PMSA(ipnt( spInc + 1)) = PSed
                    PMSA(ipnt( spInc + 2)) = MaxSed_C
                    PMSA(ipnt( spInc + 3)) = MaxSed_Chl
                    PMSA(ipnt( spInc + 4)) = MaxSed_N
                    PMSA(ipnt( spInc + 5)) = MaxSed_P
                    PMSA(ipnt( spInc + 6)) = MaxSed_Si

                enddo ! end loop over species

            endif ! end if check for dry cell

        endif ! end if check for dry cell

        !allocate pointers
        iflux = iflux + noflux
        ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

    enddo ! end loop over segments

    ipnt(1:ipointLength) = ipoint(1:ipointLength)
    ! Exchange loop over horizontal direction -------------------------------------------------------------------
    do ioq = 1, noq1 + noq2
        do iSpec = 1, nrSpec
            ! input+ input exchange + output + exchange + loop over species
            PMSA(ipnt( inpItems + nrSpec + nrSpec * nrSpecOut + iSpec )) = 0.0
        enddo
        ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)
    enddo

    ! Exchange loop over vertical direction -------------------------------------------------------------------
    do ioq = noq1+noq2+1 , noq1+noq2+noq3+noq4
        i_origin  = iexpnt(1,ioq)
        i_dest    = iexpnt(2,ioq)

        if ( i_origin .gt. 0 .and. i_dest .gt. 0 ) then
            ! find first index of the origin and destination segment
            call dhkmrk(1,iknmrk(i_origin ),ikmrkv)
            call dhkmrk(1,iknmrk(i_dest),ikmrkn)
            if (ikmrkv.eq.1 .and. ikmrkn.eq.1) then
                ! water-water exchange
                ! convert value from m/d to m/s
                Depth  = PMSA( ipnt(6) + (i_origin - 1) * increm(6) )
                Depth2 = PMSA( ipnt(6) + (i_dest - 1)   * increm(6) )
                MinDepth  = PMSA( ipnt(3) + (i_origin - 1) * increm(3) )
                MinDepth2 = PMSA( ipnt(3) + (i_dest - 1)   * increm(3) )

                do iSpec = 1, nrSpec

                    if ( Depth .gt. MinDepth .and. Depth2 .gt. MinDepth2 ) then
                        PMSA(ipnt( inpItems + nrSpec + nrSpec * nrSpecOut + iSpec)) = PMSA(ipnt(inpItems + iSpec))/numSecPerDay
                    else
                        PMSA(ipnt( inpItems + nrSpec + nrSpec * nrSpecOut + iSpec )) = 0.0
                    endif ! end check if min depth large enough

                enddo ! end loop over species
            endif ! end check water - water
        endif ! end check boundaries
        ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)
    enddo ! end loop of vertical exchange
    deallocate (ipnt)
    return
end ! end subroutine
