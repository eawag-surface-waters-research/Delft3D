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
subroutine PRODIA     ( pmsa   , fl     , ipoint , increm, noseg , &
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &
                            noq3   , noq4   )
!
!*******************************************************************************
!
use m_dhkmrk
use protist_math_functions
use protist_cell_functions
use protist_uptake_functions
use protist_photosynthesis_functions
use protist_constants
use ieee_arithmetic


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
    integer, parameter    :: nrIndInp = 8     !   nr of species independent input items
    integer, parameter    :: nrSpecInp = 37   !   nr of inputs per species
    integer, parameter    :: nrSpecOut = 25   !   nr of outputs per species
    integer, parameter    :: nrSpecFlux = 22  !   nr of fluxes per species
    integer               :: nrInputItems     !   nr of input items need for output PMSA
    integer               :: nrOutputItems    !   nr of output items need for output PMSA
    integer               :: ipointLength     !   total length of the PMSA input and output pointer array
    integer, allocatable  :: ipnt(:)          !   Local work array for the pointering

    integer iseg          ! Local loop counter for computational element loop
    integer ioq
    integer iflux
    integer ikmrk1        ! first segment attribute

    integer iSpec         ! local species number counter
    integer spInc         ! local species PSMA/FL number increment

     ! input parameters
    integer nrSpec       ! total nr species implemented in process (from proc_def)
    real    UmRT, Q10, RT, CR                           ! growth and respiration rate calculation
    real    NCm, NO3Cm, PCm, SiCm, ChlCm                ! maximum NC, PC, ChlC quotas
    real    NCo, PCo, SiCo, ChlCo                       ! minimum NC and PC quotas
    real    NCopt, NO3Copt, PCopt, SiCopt               ! optimal NC and PC quotas
    real    KtSi, KtP, KtNH4, KtNO3                     ! half saturation constants
    real    PCoNCopt, PCoNCm                            ! P status influence on optimum NC
    real    ReUmNH4, ReUmNO3, redco, PSDOC, relPS       ! relative growth rates with specific nutrients
    real    MrtRT, FrAut, FrDet                         ! reference mortality and fractions
    real    alpha                                       ! inital slope

    ! input state variables
    real    protC, protChl, protN, protP, protSi        ! protist state variables
    real    PO4, NH4, NO3, Si                           ! nutrient state variables
    real    Temp                                        ! physical abiotic variables
    real    PFD, atten, exat                            ! available light and extinction


    ! auxiliaries
    real    NC, PC, SC, ChlC                            ! cell nutrient quotas
    real    UmT, BR                                     ! growth and repsiration rates
    real    NCu, PCu, SCu, NPSiCu                       ! nutrient status within the cell
    real    upP, upNH4, upNO3, upSi                     ! nutrient uptake
    real    PSqm, Cfix, synChl, degChl                  ! plateau and Cifx through photosynthesis
    real    maxPSreq, PS                                ! req for C to come from PS (==1 for diatoms)
    real    totR, Cu, NPP                               ! respiration, C-growth and nett primary production
    real    mrt, mrtFrAut, mrtFrDet                     ! mortality to detritus and autolysis

    ! Fluxes
    real    dNH4up, dNO3up, dPup, dSiup                 ! uptake fluxes
    real    dCfix                                       ! photosynthesis flux
    real    dChlsyn, dChldeg                            ! Chl synthesis  and degradation flux
    real    dCresp                                      ! respiration flux
    real    dDOCleak                                    ! C leak through photosynthesis
    real    dDOCvoid, dNH4out, dPout                    ! voiding fluxes
    real    dAutC, dAutN, dAutP, dAutSi, dAutChl        ! autolysis fluxes
    real    dDetC, dDetN, dDetP, dDetSi, dDetChl        ! voiding fluxes

!
!*******************************************************************************
!

    ! segment and species independent items
    nrSpec = nint(PMSA(ipoint(   1 )))   !   nr of species in the interface                                 (-)

!   nrInputItems  = nrIndInp + nrSpec * nrSpecInp = 8 + 2 * 37 = 82
!   nrOutputItems = nrSpec * nrSpecOut = 2 * 25 = 50
!   ipointLength = nrInputItems + nrOutputItems = 82 + 48 = 130
!   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * 22 = 44

    ! length of the PMSA input pointer array.
    nrInputItems = nrIndInp + nrSpec * nrSpecInp
    nrOutputItems = nrSpec * nrSpecOut
    ipointLength = nrInputItems + nrOutputItems

    allocate (ipnt(ipointLength))
    ipnt(1:ipointLength) = ipoint(1:ipointLength)
    iflux = 0

    ! segment loop
    segmentLoop: do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then

        ! species independent items
        PO4          = PMSA(ipnt(   2 ))  !    initial external DIP                                   (gP m-3)
        NH4          = PMSA(ipnt(   3 ))  !    initial external NH4                                   (gN m-3)
        NO3          = PMSA(ipnt(   4 ))  !    initial external NO3                                   (gN m-3)
        Si           = PMSA(ipnt(   5 ))  !    initial external Si                                    (gSi m-3)
        Temp         = PMSA(ipnt(   6 ))  !    ambient water temperature                              (oC)
        PFD          = PMSA(ipnt(   7 ))  !    from rad to photon flux density                        (umol photon m-2)
        atten        = PMSA(ipnt(   8 ))  !    attenuation of light by water + plankton Chl           (-)
        exat         = EXP(-atten)        !    -ve exponent of attenuation                            (-)

        ! species loop
        speciesLoop: do iSpec = 1, nrSpec

            spInc = nrIndInp + (iSpec - 1) * nrSpecInp

            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( spInc +  1 ))   !      C-biomass                                              (gC m-3)

            ! skip if biomass is below threshold
            if (protC <= threshCmass) then
                cycle speciesLoop
            end if

            protChl      = PMSA(ipnt( spInc +  2 ))   !      Chl-biomass                                            (gChl m-3)
            protN        = PMSA(ipnt( spInc +  3 ))   !      N-biomass                                              (gN m-3)
            protP        = PMSA(ipnt( spInc +  4 ))   !      P-biomass                                              (gP m-3)
            protSi       = PMSA(ipnt( spInc +  5 ))   !      Si-biomass                                             (gSi m-3)
            alpha        = PMSA(ipnt( spInc +  6 ))   !      alpha for photosynthesis in protist                    (Figure this out!)
            ChlCm        = PMSA(ipnt( spInc +  7 ))   !      maximum cellular Chl:C ratio                           (gChl gC-1)
            ChlCo        = PMSA(ipnt( spInc +  8 ))   !      minimum cellular Chl:C ratio                           (gChl gC-1)
            CR           = PMSA(ipnt( spInc +  9 ))   !      catabolic respiration quotient                         (-)
            FrAut        = PMSA(ipnt( spInc + 10 ))   !      fraction of mortality to autolysis                     (-)
            FrDet        = PMSA(ipnt( spInc + 11 ))   !      fraction of mortality to detritus                      (-)
            KtNH4        = PMSA(ipnt( spInc + 12 ))   !      Kt for NH4 transport                                   (gN m-3)
            KtNO3        = PMSA(ipnt( spInc + 13 ))   !      Kt for NO3 transport                                   (gN m-3)
            KtP          = PMSA(ipnt( spInc + 14 ))   !      Kt for DIP transport                                   (gP m-3)
            KtSi         = PMSA(ipnt( spInc + 15 ))   !      Kt for Si transport                                    (gSi m-3)
            MrtRT        = PMSA(ipnt( spInc + 16 ))   !      mortality at reference temperature                     (-)
            NCm          = PMSA(ipnt( spInc + 17 ))   !      N:C that totally represses NH4 transport               (gN gC-1)
            NCo          = PMSA(ipnt( spInc + 18 ))   !      minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( spInc + 19 ))   !      N:C for growth under optimal conditions                (gN gC-1)
            NO3Cm        = PMSA(ipnt( spInc + 20 ))   !      N:C that totally represses NO3 transport               (gN gC-1)
            NO3Copt      = PMSA(ipnt( spInc + 21 ))   !      N:C for growth on NO3 under optimal conditions         (gN gC-1)
            PCm          = PMSA(ipnt( spInc + 22 ))   !      PC maximum quota                                       (gP gC-1)
            PCo          = PMSA(ipnt( spInc + 23 ))   !      PC minimum quota                                       (gP gC-1)
            PCoNCm       = PMSA(ipnt( spInc + 24 ))   !      maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCoNCopt     = PMSA(ipnt( spInc + 25 ))   !      optimum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCopt        = PMSA(ipnt( spInc + 26 ))   !      PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( spInc + 27 ))   !      proportion of current PS being leaked as DOC           (-)
            Q10          = PMSA(ipnt( spInc + 28 ))   !      Q10 for UmRT                                           (-)
            redco        = PMSA(ipnt( spInc + 29 ))   !      C respired to support nitrate reduction for NH4        (gC gN-1)
            relPS        = PMSA(ipnt( spInc + 30 ))   !      relative PSmax:Umax on phototrophy                     (-)
            ReUmNH4      = PMSA(ipnt( spInc + 31 ))   !      max. growth rate supported by NH4-N:Umax               (-)
            ReUmNO3      = PMSA(ipnt( spInc + 32 ))   !      max. growth rate supported by NO3-N:Umax               (-)
            RT           = PMSA(ipnt( spInc + 33 ))   !      reference temperature for UmRT                         (deg C)
            SiCm         = PMSA(ipnt( spInc + 34 ))   !      absolute maximum Si:C (diatom)                         (gSi gC-1)
            SiCo         = PMSA(ipnt( spInc + 35 ))   !      optimum Si:C for (diatom) growth                       (gSi gC-1)
            SiCopt       = PMSA(ipnt( spInc + 36 ))   !      minimum Si:C (diatom)                                  (gSi gC-1)
            UmRT         = PMSA(ipnt( spInc + 37 ))   !      maximum growth rate at reference T                     (d-1)


            ! Calculate the nutrient quota of the cell-------------------------------------------------------------------------------
            ! Units: gNut gC-1
            NC   = quota(protN, protC)
            PC   = quota(protP, protC)
            SC   = quota(protSi, protC)
            ChlC = quota(protChl, protC)

            ! Calculate maximum growth and respiration -------------------------------------------------------------------------------
            ! Units: gC gC-1 d-1
            UmT = Q10rate(UmRT, Q10, Temp, RT)
            BR  = basal_respiration(UmT, CR)

            ! Calculate nutrient status within cell compared to ideal status (nutrient status = 1) ---------------------------------------
            ! Determine minimum of N-P-Si limitation; Liebig-style limitation of growth (NPSiCu)
            ! Units: (-)
            NCu = statusNC(NC, NCo, NCopt)
            PCu = statusPC(PC, PCo, PCopt)
            SCu = statusSC(SiCopt, SiCo, Si, KtSi)
            NPSiCu = min(NCu, PCu, SCu)

            ! Calculate uptake for the nutrients ---------------------------------------
            ! Units: gNut gC-1 d-1
            upP = uptakeP(PC, PCo, PCopt, PCm, UmT, PO4, KtP)
            upNH4 = uptakeNH4(PCoNCopt, PCoNCm, PCu, NCu, NC, NCo, NCopt, NCm, UmT, ReUmNH4, NH4, KtNH4)
            upNO3 = uptakeNO3(PCoNCm, PCu, NCu, NC, NCo, NO3Copt, NO3Cm, UmT, ReUmNO3, NO3, KtNO3)
            upSi  = uptakeSi(SC, SiCo, SiCopt, SiCm, UmT, Si, KtSi)

            ! Calculate photosynthesis related equation ---------------------------------------
            ! Units: gC gC-1 d-1
            maxPSreq = 1.0  ! need to cover all C through photosynthesis
            PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPSiCu, BR, PSDOC)
            PS   = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
            Cfix = netPS(PS, PSDOC)

            ! Calculate chlorophyll synthesis and degradation ---------------------------------------
            ! Units: gChl gC-1 d-1
            synChl = synthesisChl(ChlC, ChlCo, ChlCm, UmT, maxPSreq, NPSiCu, Cfix, PSqm)
            degChl = degradeChl(ChlC, ChlCm, UmT, NPSiCu)

            ! Calculate respiration and C-growth  ---------------------------------------
            ! Units: gC gC-1 d-1
            ! 0.0 because it cannot assimilate prey
            if (protC >= 1.0E-5) then
                totR = totalRespiration(redco, upNO3, upNH4, 0.0, 0.0, 0.0, BR)
            else
                totR = 0.0
            end if
            !totR = totalRespiration(redco, upNO3, upNH4, 0.0, 0.0, 0.0, BR)
            Cu   = Cfix - totR
            
            ! Calculate nett primary production per m3 ---------------------------------------
            ! Units: gC m-3 d-1
            NPP = Cu * protC

            ! Calculate mortality  ---------------------------------------
            ! Units: gC gC-1 d-1
            if (protC >= 1.0E-5) then
                mrt = Q10rate(MrtRT, Q10, Temp, RT)
            else
                mrt = 0.0
            end if
            !mrt = Q10rate(MrtRT, Q10, Temp, RT)
            mrtFrAut = mortality(mrt, FrAut)
            mrtFrDet = mortality(mrt, FrDet)

            ! Output -------------------------------------------------------------------

            ! (input items + position of specific output item in vector + species loop * total number of output)
            spInc = nrInputItems +(iSpec - 1) * nrSpecOut

            PMSA(ipnt( spInc +  1 )) = NC
            PMSA(ipnt( spInc +  2 )) = PC
            PMSA(ipnt( spInc +  3 )) = SC
            PMSA(ipnt( spInc +  4 )) = ChlC
            PMSA(ipnt( spInc +  5 )) = UmT
            PMSA(ipnt( spInc +  6 )) = BR
            PMSA(ipnt( spInc +  7 )) = NCu
            PMSA(ipnt( spInc +  8 )) = PCu
            PMSA(ipnt( spInc +  9 )) = SCu
            PMSA(ipnt( spInc + 10 )) = NPSiCu
            PMSA(ipnt( spInc + 11 )) = upP
            PMSA(ipnt( spInc + 12 )) = upNH4
            PMSA(ipnt( spInc + 13 )) = upNO3
            PMSA(ipnt( spInc + 14 )) = upSi
            PMSA(ipnt( spInc + 15 )) = PSqm
            PMSA(ipnt( spInc + 16 )) = PS
            PMSA(ipnt( spInc + 17 )) = Cfix
            PMSA(ipnt( spInc + 18 )) = NPP
            PMSA(ipnt( spInc + 19 )) = synChl
            PMSA(ipnt( spInc + 20 )) = degChl
            PMSA(ipnt( spInc + 21 )) = totR
            PMSA(ipnt( spInc + 22 )) = Cu
            PMSA(ipnt( spInc + 23 )) = mrt
            PMSA(ipnt( spInc + 24 )) = mrtFrAut
            PMSA(ipnt( spInc + 25 )) = mrtFrDet

            ! FLUXES -------------------------------------------------------------------
            ! Protist gains------------------------------------------------------------
            ! gNut m-3 d-1   uptake of nutrients into algal biomass
            dNH4up = protC * upNH4
            dNO3up = protC * upNO3
            dPup   = protC * upP
            dSiup  = protC * upSi

            ! gC m-3 d-1   total contribution to biomass growth from C-fixation
            dCfix = protC * Cfix

            ! gChl m-3 d-1 Chl synthesis or degradation
            dChlsyn = protC * synChl
            dChldeg = protC * degChl

            ! Protist losses-----------------------------------------------------------
            ! gC m-3 d-1   total respiration rate
            dCresp = protC * totR

            ! gC m-3 d-1   release of DOC
            dDOCleak = protC * (PS - Cfix)

            ! gC m-3 d-1   voiding of C as DOC if NC falls below NCo
            if (NC < NCo) then
                dDOCvoid = protC - protN / NCo
            else
                dDOCvoid = 0.0
            end if

            ! gNut m-3 d-1 voiding of nutrient P and N if interanl maximum is reached
            dNH4out = voiding(protN, protC, NCm)
            dPout   = voiding(protP, protC, PCm)

            ! gNut m-3 d-1 mortality
            dAutC       = protC * mrtFrAut
            dDetC       = protC * mrtFrDet
            dAutN       = protN * mrtFrAut
            dDetN       = protN * mrtFrDet
            dAutP       = protP * mrtFrAut
            dDetP       = protP * mrtFrDet
            dAutSi      = protSi * mrtFrAut
            dDetSi      = protSi * mrtFrDet
            dAutChl     = protChl * mrtFrAut
            dDetChl     = protChl * mrtFrDet

            ! (1 + SpeciesLoop * (nr of fluxes per individual species) + total number of fluxes)
            spInc = iflux + (iSpec - 1) * nrSpecFlux

            fl ( spInc +  1 )  = dNH4up
            fl ( spInc +  2 )  = dNO3up
            fl ( spInc +  3 )  = dPup
            fl ( spInc +  4 )  = dSiup
            fl ( spInc +  5 )  = dCfix
            fl ( spInc +  6 )  = dChlsyn
            fl ( spInc +  7 )  = dChldeg
            fl ( spInc +  8 )  = dCresp
            fl ( spInc +  9 )  = dDOCleak
            fl ( spInc + 10 )  = dDOCvoid
            fl ( spInc + 11 )  = dNH4out
            fl ( spInc + 12 )  = dPout
            fl ( spInc + 13 )  = dAutC
            fl ( spInc + 14 )  = dDetC
            fl ( spInc + 15 )  = dAutN
            fl ( spInc + 16 )  = dDetN
            fl ( spInc + 17 )  = dAutP
            fl ( spInc + 18 )  = dDetP
            fl ( spInc + 19 )  = dAutSi
            fl ( spInc + 20 )  = dDetSi
            fl ( spInc + 21 )  = dAutChl
            fl ( spInc + 22 )  = dDetChl

            if ( ieee_is_nan(protC) ) write (*,*) 'ERROR: in ProtistDiat, NaN in protC in segment:', iseg
            if ( ieee_is_nan(Cfix) )  write (*,*) 'ERROR: in ProtistDiat, NaN in Cfix in segment:' , iseg
            if ( ieee_is_nan(totR) )  write (*,*) 'ERROR: in ProtistDiat, NaN in totR in segment:' , iseg
            if ( ieee_is_nan(mrt) )   write (*,*) 'ERROR: in ProtistDiat, NaN in mrt in segment:'  , iseg
            if ( ieee_is_nan(NC) )    write (*,*) 'ERROR: in ProtistDiat, NaN in NC in segment:'   , iseg
            if ( ieee_is_nan(PC) )    write (*,*) 'ERROR: in ProtistDiat, NaN in PC in segment:'   , iseg
            if ( ieee_is_nan(ChlC) )  write (*,*) 'ERROR: in ProtistDiat, NaN in ChlC in segment:' , iseg
            if ( ieee_is_nan(SC) )    write (*,*) 'ERROR: in ProtistDiat, NaN in SC in segment:'   , iseg


        enddo speciesLoop ! end loop over species

        endif ! end if check for dry cell

        !allocate pointers
        iflux = iflux + noflux
        ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

    enddo segmentLoop ! end loop over segments
    deallocate (ipnt)
    return
end ! end subroutine
