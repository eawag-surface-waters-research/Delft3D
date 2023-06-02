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
subroutine PROTCM     ( pmsa   , fl     , ipoint , increm, noseg , &
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &
                            noq3   , noq4   )
!
!*******************************************************************************
!
use m_dhkmrk
use protist_math_functions
use protist_cell_functions
use protist_types
use protist_uptake_functions
use protist_photosynthesis_functions
use protist_phagotrophy_functions
use protist_food_functions
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
    integer, parameter    :: nrIndInp = 8    !   nr of species independent input items
    integer, parameter    :: nrSpecInp = 41  !   nr of inputs per species
    integer, parameter    :: nrSpecOut = 42  !   nr of outputs per species
    integer, parameter    :: nrSpecFlux = 25 !   nr of fluxes per species
    integer, parameter    :: nrPreyInp = 8   !   nr of inputs per prey
    integer               :: nrInputItems     !   nr of input items need for output PMSA
    integer               :: nrOutputItems    !   nr of output items need for output PMSA
    integer               :: ipointLength     !   total length of the PMSA input and output pointer array
    integer, allocatable  :: ipnt(:)          !   Local work array for the pointering

    integer iseg          ! Local loop counter for computational element loop
    integer ioq
    integer iflux
    integer ikmrk1        ! first segment attribute

    integer iSpec         ! local species number counter
    integer iPrey         ! local prey number counter
    integer spInc         ! local species PMSA/FL number increment
    integer prInc         ! local pray FL number increment

     !input parameters
     integer    nrSpec        ! total nr species implemented in process (from proc_def)
     integer    nrPrey        ! total nr prey implemented in process (from proc_def)
     real    relPhag                                     ! relative phagotrophy night:day
     real    UmRT, Q10, RT, CR                           ! growth and respiration rate calculation
     real    NCm, NO3Cm, PCm, ChlCm                      ! maximum NC, PC, ChlC quotas
     real    NCo, PCo, ChlCo                             ! minimum NC and PC quotas
     real    NCopt, NO3Copt, PCopt                       ! optimal NC and PC quotas
     real    KtP, KtNH4, KtNO3                           ! half saturation constants
     real    PCoNCopt, PCoNCm                            ! P status influence on optimum NC
     real    ReUmNH4, ReUmNO3, redco, PSDOC, maxPSreq, relPS     ! relative growth rates with specific nutrients
     real    CcellProt, rProt                            ! parameters for protozooplankton cell
     real    optCR                                       ! parameters for encounter
     real    kAE, AEm, AEo                               ! parameters for assimilation efficiency
     real    SDA                                         ! specific dynamic action
     real    MrtRT, FrAut, FrDet                         ! reference mortality and fractions
     real    alpha                                       ! inital slope

     ! input state variables
     real    protC, protChl, protN, protP                ! protist state variables
     real    PO4, NH4, NO3                               ! nutrient state variables
     real    Temp                                        ! physical abiotic variables
     real    PFD, atten, exat                            ! available light and extinction


     ! auxiliaries
     real    lightInh                                    ! inhibtion of feeding in dark
     real    NC, PC, ChlC                                ! cell nutrient quotas
     real    UmT, BR                                     ! growth and repsiration rates
     real    NCu, PCu, NPCu                              ! nutrient status within the cell
     real    mot                                         ! motility
     real    upP, upNH4, upNO3                           ! nutrient uptake
     real    PSqm, Cfix, synChl, degChl                  ! plateau and Cifx through photosynthesis
     real    CfixPS                                      ! C fix minus phototsynthesis related respiration
     real    PS                                          ! req for C to come from PS
     ! food quantity
     real    sumCP        ! total captured prey
     real    ingNC, ingPC ! total ingested N and P
     real    preyFlag     ! sum of preyFlag (can be 0 = both low, 1 = 1 ok, 2 = both ok)

     ! food quality
     real    stoichP, ppNC, ppPC                    ! stoichiometry comparison
     real    opAE                                   ! assimilation efficiency
     ! ingestion and assimilation
     real    reqPred                                ! required Predation
     real    maxIng, ingSat, ingC, ingN, ingP, KI   ! ingestion
     real    assC, assN, assP                       ! assimilation
     ! respiration, Cu and mortality
     real    totR, Cu, NPP                               ! respiration, C-growth and nett primary production
     real    mrt, mrtFrAut, mrtFrDet                     ! mortality to detritus and autolysis

     ! other parameters
     real, parameter :: wTurb = 0.0 ! this needs to be an input!!!!

     ! Fluxes
     real    dNH4up, dNO3up, dPup                        ! uptake fluxes
     real    dCfix                                       ! photosynthesis flux
     real    dChlsyn, dChldeg                            ! Chl synthesis  and degradation flux
     real    dCresp                                      ! respiration flux
     real    dDOCleak                                    ! C leak through photosynthesis
     real    dDOCvoid, dNH4out, dPout                    ! voiding fluxes
     real    dAutC, dAutN, dAutP, dAutChl                ! autolysis fluxes
     real    dDetC, dDetN, dDetP, dDetChl                ! voiding fluxes
     real    dCeat, dNeat, dPeat                         ! assimilation fluxes
     real    dPOCout, dPONout, dPOPout                   ! voiding fluxes

     ! Protist arrays
     type(protist_array)   :: prot_array                 ! type containing all protist specific arrays


!
!*******************************************************************************
!
    ! segment and species independent items
    nrSpec    = nint(PMSA(ipoint(   1 )))   !   total nr species implemented in process                (-)
    nrPrey    = nint(PMSA(ipoint(   2 )))   !   nr of prey species implemented                         (-)

!   nrInputItems  = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec) = 8 + 2 * 41 + 4 * (8 + 2) = 130
!   nrOutputItems = nrSpec * nrSpecOut = 2 * 42 = 84
!   ipointLength = nrInputItems + nrOutputItems = 176
!   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * (25 + 4 * 5) = 90

    ! length of the PMSA input pointer array.
    nrInputItems  = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec)
    nrOutputItems = nrSpec * nrSpecOut
    ipointLength = nrInputItems + nrOutputItems

    allocate (ipnt(ipointLength))
    ipnt(1:ipointLength) = ipoint(1:ipointLength)
    iflux = 0


    ! allocation of prey input array
    call allocate_prot_array(prot_array,nrPrey)


    ! segment loop
    segmentLoop: do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then

        ! species independent items
        PO4          = PMSA(ipnt(  3 ))  !    initial external DIP                                   (gP m-3)
        NH4          = PMSA(ipnt(  4 ))  !    initial external NH4                                   (gN m-3)
        NO3          = PMSA(ipnt(  5 ))  !    initial external NO3                                   (gN m-3)
        Temp         = PMSA(ipnt(  6 ))  !    ambient water temperature                              (oC)
        PFD          = PMSA(ipnt(  7 ))  !    from rad to photon flux density                        (umol photon m-2)
        atten        = PMSA(ipnt(  8 ))  !    attenuation of light by water + plankton Chl           (-)
        exat         = EXP(-atten)       !    -ve exponent of attenuation                            (-)

        ! species loop
        speciesLoop: do iSpec = 1, nrSpec

            spInc = nrIndInp + (iSpec - 1) * nrSpecInp

            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( spInc +  1 ))   !     C-biomass                                              (gC m-3)

            ! skip if biomass is below threshold
            if (protC <= threshCmass) then
                cycle speciesLoop
            end if

            protChl      = PMSA(ipnt( spInc +  2 ))   !     Chl-biomass                                            (gChl m-3)
            protN        = PMSA(ipnt( spInc +  3 ))   !     N-biomass                                              (gN m-3)
            protP        = PMSA(ipnt( spInc +  4 ))   !     P-biomass                                              (gP m-3)
            AEm          = PMSA(ipnt( spInc +  5 ))   !     maximum assimilation efficiency (AE)                   (-)
            AEo          = PMSA(ipnt( spInc +  6 ))   !     minimum AE                                             (-)
            alpha        = PMSA(ipnt( spInc +  7 ))   !     alpha for photosynthesis in protist                    (Figure this out!)
            CcellProt    = PMSA(ipnt( spInc +  8 ))   !     C content of protist cell                              (pgC cell-1)
            ChlCm        = PMSA(ipnt( spInc +  9 ))   !     maximum cellular Chl:C ratio                           (gChl gC-1)
            ChlCo        = PMSA(ipnt( spInc + 10 ))   !     minimum cellular Chl:C ratio                           (gChl gC-1)
            CR           = PMSA(ipnt( spInc + 11 ))   !     catabolic respiration quotient                         (-)
            FrAut        = PMSA(ipnt( spInc + 12 ))   !     fraction of mortality to autolysis                     (-)
            FrDet        = PMSA(ipnt( spInc + 13 ))   !     fraction of mortality to detritus                      (-)
            kAE          = PMSA(ipnt( spInc + 14 ))   !     Control of AE in response to prey quality              (-)
            KtNH4        = PMSA(ipnt( spInc + 15 ))   !     Kt for NH4 transport                                   (gN m-3)
            KtNO3        = PMSA(ipnt( spInc + 16 ))   !     Kt for NO3 transport                                   (gN m-3)
            KtP          = PMSA(ipnt( spInc + 17 ))   !     Kt for DIP transport                                   (gP m-3)
            MrtRT        = PMSA(ipnt( spInc + 18 ))   !     mortality at reference temperature                     (-)
            maxPSreq     = PMSA(ipnt( spInc + 19 ))   !     maximum C to come from PS                              (-)
            NCm          = PMSA(ipnt( spInc + 20 ))   !     N:C that totally represses NH4 transport               (gN gC-1)
            NCo          = PMSA(ipnt( spInc + 21 ))   !     minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( spInc + 22 ))   !     N:C for growth under optimal conditions                (gN gC-1)
            NO3Cm        = PMSA(ipnt( spInc + 23 ))   !     N:C that totally represses NO3 transport               (gN gC-1)
            NO3Copt      = PMSA(ipnt( spInc + 24 ))   !     N:C for growth on NO3 under optimal conditions         (gN gC-1)
            optCR        = PMSA(ipnt( spInc + 25 ))   !     proportion of prey captured by starved Prot            (-)
            PCm          = PMSA(ipnt( spInc + 26 ))   !     PC maximum quota                                       (gP gC-1)
            PCo          = PMSA(ipnt( spInc + 27 ))   !     PC minimum quota                                       (gP gC-1)
            PCoNCm       = PMSA(ipnt( spInc + 28 ))   !     maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCoNCopt     = PMSA(ipnt( spInc + 29 ))   !     optimum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCopt        = PMSA(ipnt( spInc + 30 ))   !     PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( spInc + 31 ))   !     proportion of current PS being leaked as DOC           (-)
            Q10          = PMSA(ipnt( spInc + 32 ))   !     Q10 for UmRT                                           (-)
            rProt        = PMSA(ipnt( spInc + 33 ))   !     radius of nutrient repleted protist cell               (um)
            redco        = PMSA(ipnt( spInc + 34 ))   !     C respired to support nitrate reduction for NH4        (gC gN-1)
            relPhag      = PMSA(ipnt( spInc + 35 ))   !     rel. phagotrophy in dark : in light                    (-)
            relPS        = PMSA(ipnt( spInc + 36 ))   !     relative PSmax:Umax on phototrophy                     (-)
            ReUmNH4      = PMSA(ipnt( spInc + 37 ))   !     max. growth rate supported by NH4-N:Umax               (-)
            ReUmNO3      = PMSA(ipnt( spInc + 38 ))   !     max. growth rate supported by NO3-N:Umax               (-)
            RT           = PMSA(ipnt( spInc + 39 ))   !     reference temperature for UmRT                         (deg C)
            SDA          = PMSA(ipnt( spInc + 40 ))   !     specific dynamic action                                (-)
            UmRT         = PMSA(ipnt( spInc + 41 ))   !     maximum growth rate at reference T                     (d-1)


            ! Calculate the nutrient quota of the cell-------------------------------------------------------------------------------
            ! Units: gNut gC-1
            NC   = quota(protN, protC)
            PC   = quota(protP, protC)
            ChlC = quota(protChl, protC)



            ! Calculate maximum growth and respiration -------------------------------------------------------------------------------
            ! Units: gC gC-1 d-1
            UmT = Q10rate(UmRT, Q10, Temp, RT)
            BR  = basal_respiration(UmT, CR)

            !! PHOTOTROPHY -------------------------------------------------------------------------------

            ! Calculate nutrient status within cell compared to ideal status (nutrient status = 1) ---------------------------------------
            ! Determine minimum of N-P-Si limitation; Liebig-style limitation of growth (NPCu)
            ! Units: (-)
            NCu = statusNC(NC, NCo, NCopt)
            PCu = statusPC(PC, PCo, PCopt)
            NPCu = min(NCu, PCu)

            ! swimming speed -------------------------------------------------------------------------------
            ! Units: m s-1
            mot = motility(rProt)

            ! Calculate uptake for the nutrients ---------------------------------------
            ! Units: gNut gC-1 d-1
            upP = uptakeP(PC, PCo, PCopt, PCm, UmT, PO4, KtP)
            upNH4 = uptakeNH4(PCoNCopt, PCoNCm, PCu, NCu, NC, NCo, NCopt, NCm, UmT, ReUmNH4, NH4, KtNH4)
            upNO3 = uptakeNO3(PCoNCm, PCu, NCu, NC, NCo, NO3Copt, NO3Cm, UmT, ReUmNO3, NO3, KtNO3)

            ! Calculate photosynthesis related equation ---------------------------------------
            ! Units: gC gC-1 d-1
            ! I do not like the variable maxPSreq. Not measureable and pretty "strong" influence
            PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPCu, BR, PSDOC)
            PS   = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
            Cfix = netPS(PS, PSDOC)

            ! rate of (positive) net phototrophy
            ! Units: gC gC-1 d-1
            CfixPS = Cfix - totalRespiration(redco, upNO3, upNH4, 0.0, 0.0, 0.0, 0.0)
            
            ! Calculate nett primary production per m3 ---------------------------------------
            ! Units: gC m-3 d-1
            NPP = CfixPS * protC

            ! Calculate chlorophyll synthesis and degradation ---------------------------------------
            ! Units: gChl gC-1 d-1
            synChl = synthesisChl(ChlC, ChlCo, ChlCm, UmT, maxPSreq, NPCu, Cfix, PSqm)
            degChl = degradeChl(ChlC, ChlCm, UmT, NPCu)

            !! PHAGOTROHY -------------------------------------------------------------------------------

            call initialize_prot_array(prot_array,nrPrey, PMSA, ipnt, nrIndInp, nrSpec, nrSpecInp, iSpec, nrPreyInp)


            ! for output (-)
            preyFlag = sum(prot_array%preyFlag)


            !! FOOD QUANTITY -------------------------------------------------------------------------------
            ! reduction of phagotrophy during night
            ! Units: (-)
            relPhag = 1.0 - relPhag
            lightInh = lightInhibition(PFD, relPhag)

            ! cell abundance of prey per m3
            ! Units: nr cells m-3 (1e12: transform between g (preyC) and pg (CcontentPrey))
            prot_array%nrPrey = lightInh * prot_array%preyFlag * 1e12 * prot_array%preyC / prot_array%CcellPrey

            call protistFoodQuantity(prot_array, rProt, wTurb, CcellProt, optCR, mot, sumCP, ingNC, ingPC)


            !! FOOD QUALITY -------------------------------------------------------------------------------
            call protistFoodQuality(ingNC, ingPC, NCopt, PCopt, kAE, AEm, AEo, ppNC, ppPC, stoichP, opAE)


            ! INGESTION  -------------------------------------------------------------------------------
            ! required predation
            ! can also fall below 0.0 if CfixPS is larger than Umt+BR
            ! of course the question remains what UmT is worth if Cfix > UmT
            ! Units: gC gC-1 d-1
            ! LS: I removed the CfixPS part for now because it produced jumps which didn't look nice.
            ! LS: I need to rethink this part
            !reqPred = ((UmT + BR - CfixPS) / (1.0 - SDA)) / opAE ! MDK 23-11-2021: remove?
            reqPred = ((UmT + BR - 0.0) / (1.0 - SDA)) / opAE

            ! maximum ingestion if needed
            ! if 0.0 then there is no need for ingestion because of high Cfix
            ! Units: gC gC-1 d-1
            ! with the upper adjustment (CfixPS == 0) this line becomes obsolete e.g. reqPred > 0.0 => reqPred = maxIng
            maxIng = max(0.0, reqPred)


            call protistIngestion(maxIng, sumCP, ingNC, ingPC, KI, ingSat, ingC, ingN, ingP)


            ! ASSIMILATION -------------------------------------------------------------------------------
            ! assimilation of ingested prey
            ! Units: gC gC-1 d-1 / gNut gC-1 d-1
            assC = ingC * opAE
            assN = assC * NCopt
            assP = assC * PCopt

            ! Calculate respiration and C-growth  ---------------------------------------
            ! Units: gC gC-1 d-1
            ! 0.0 because it cannot assimilate prey
            if (protC > 1.0E-5) then
                 totR = totalRespiration(redco, upNO3, upNH4, assC, assN, SDA, BR)
            else
                 totR = 0.0
            end if
            !totR = totalRespiration(redco, upNO3, upNH4, assC, assN, SDA, BR)
            Cu   = Cfix + assC - totR

            ! Calculate mortality  ---------------------------------------
            call protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut, mrtFrDet)



            ! Output -------------------------------------------------------------------

            ! (input items + position of specific output item in vector + species loop * total number of output)
            spInc = nrInputItems + (iSpec - 1) * nrSpecOut

            PMSA(ipnt( spInc +  1 )) = NC
            PMSA(ipnt( spInc +  2 )) = PC
            PMSA(ipnt( spInc +  3 )) = ChlC
            PMSA(ipnt( spInc +  4 )) = UmT
            PMSA(ipnt( spInc +  5 )) = BR
            PMSA(ipnt( spInc +  6 )) = NCu
            PMSA(ipnt( spInc +  7 )) = PCu
            PMSA(ipnt( spInc +  8 )) = NPCu
            PMSA(ipnt( spInc +  9 )) = mot
            PMSA(ipnt( spInc + 10 )) = upP
            PMSA(ipnt( spInc + 11 )) = upNH4
            PMSA(ipnt( spInc + 12 )) = upNO3
            PMSA(ipnt( spInc + 13 )) = PSqm
            PMSA(ipnt( spInc + 14 )) = PS
            PMSA(ipnt( spInc + 15 )) = Cfix
            PMSA(ipnt( spInc + 16 )) = CfixPS
            PMSA(ipnt( spInc + 17 )) = NPP
            PMSA(ipnt( spInc + 18 )) = synChl
            PMSA(ipnt( spInc + 19 )) = degChl
            PMSA(ipnt( spInc + 20 )) = sumCP
            PMSA(ipnt( spInc + 21 )) = ingNC
            PMSA(ipnt( spInc + 22 )) = ingPC
            PMSA(ipnt( spInc + 23 )) = ppNC
            PMSA(ipnt( spInc + 24 )) = ppPC
            PMSA(ipnt( spInc + 25 )) = stoichP
            PMSA(ipnt( spInc + 26 )) = opAE
            PMSA(ipnt( spInc + 27 )) = reqPred
            PMSA(ipnt( spInc + 28 )) = maxIng
            PMSA(ipnt( spInc + 29 )) = ingSat
            PMSA(ipnt( spInc + 30 )) = ingC
            PMSA(ipnt( spInc + 31 )) = assC
            PMSA(ipnt( spInc + 32 )) = ingN
            PMSA(ipnt( spInc + 33 )) = ingP
            PMSA(ipnt( spInc + 34 )) = assN
            PMSA(ipnt( spInc + 35 )) = assP
            PMSA(ipnt( spInc + 36 )) = totR
            PMSA(ipnt( spInc + 37 )) = Cu
            PMSA(ipnt( spInc + 38 )) = mrt
            PMSA(ipnt( spInc + 39 )) = mrtFrAut
            PMSA(ipnt( spInc + 40 )) = mrtFrDet
            PMSA(ipnt( spInc + 41 )) = preyFlag
            PMSA(ipnt( spInc + 42 )) = lightInh

            ! FLUXES -------------------------------------------------------------------
            ! Protist gains------------------------------------------------------------
            ! Protist growth through assimilation -----------------------------------------------------
            ! gX m-3 d-1 assimilation of X from prey
            dCeat = protC * assC
            dNeat = protC * assN
            dPeat = protC * assP

            ! gNut m-3 d-1   uptake of nutrients into algal biomass
            dNH4up = protC * upNH4
            dNO3up = protC * upNO3
            dPup   = protC * upP

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

            ! gX m-3 d-1  rate of voiding of X as particulates
            dPOCout = protC * (ingC - assC)
            dPONout = protC * (ingN - assN)
            dPOPout = protC * (ingP - assP)

            ! gNut m-3 d-1 mortality
            dAutC       = protC * mrtFrAut
            dDetC       = protC * mrtFrDet
            dAutN       = protN * mrtFrAut
            dDetN       = protN * mrtFrDet
            dAutP       = protP * mrtFrAut
            dDetP       = protP * mrtFrDet
            dAutChl     = protChl * mrtFrAut
            dDetChl     = protChl * mrtFrDet



            ! (1 + SpeciesLoop * (nr of fluxes per individual species) + total number of fluxes)
            spInc = iflux + (iSpec - 1) * (nrSpecFlux + nrPrey * nrLossFluxes)

            fl ( spInc +  1 )  = dNH4up
            fl ( spInc +  2 )  = dNO3up
            fl ( spInc +  3 )  = dPup
            fl ( spInc +  4 )  = dCfix
            fl ( spInc +  5 )  = dChlsyn
            fl ( spInc +  6 )  = dChldeg
            fl ( spInc +  7 )  = dCresp
            fl ( spInc +  8 )  = dDOCleak
            fl ( spInc +  9 )  = dDOCvoid
            fl ( spInc + 10 )  = dNH4out
            fl ( spInc + 11 )  = dPout
            fl ( spInc + 12 )  = dCeat
            fl ( spInc + 13 )  = dNeat
            fl ( spInc + 14 )  = dPeat
            fl ( spInc + 15 )  = dPOCout
            fl ( spInc + 16 )  = dPONout
            fl ( spInc + 17 )  = dPOPout
            fl ( spInc + 18 )  = dAutC
            fl ( spInc + 19 )  = dDetC
            fl ( spInc + 20 )  = dAutN
            fl ( spInc + 21 )  = dDetN
            fl ( spInc + 22 )  = dAutP
            fl ( spInc + 23 )  = dDetP
            fl ( spInc + 24 )  = dAutChl
            fl ( spInc + 25 )  = dDetChl

            ! Prey losses through pred ing. ----------------------------------------------------

            ! ingestion of nut of iPrey through iPred gNut m-3 d-1
            prot_array%dPreyC    = protC * (ingC * prot_array%propPrey)
            prot_array%dPreyChl  = prot_array%dPreyC * (prot_array%preyChl / prot_array%preyC)
            prot_array%dPreyN    = prot_array%dPreyC * (prot_array%preyN / prot_array%preyC)
            prot_array%dPreyP    = prot_array%dPreyC * (prot_array%preyP / prot_array%preyC)
            prot_array%dPreySi   = prot_array%dPreyC * (prot_array%preySi / prot_array%preyC)

            ! loop over prey ingestion fluxes
            do iPrey = 1, nrPrey
                ! (nr prey independent fluxes + prey Flux # + loop) + (move on to next predator) + total number of fluxes
                prInc = spInc + nrSpecFlux + (iPrey - 1) * nrLossFluxes

                fl ( prInc + 1 ) = prot_array%dPreyC(iPrey)
                fl ( prInc + 2 ) = prot_array%dPreyChl(iPrey)
                fl ( prInc + 3 ) = prot_array%dPreyN(iPrey)
                fl ( prInc + 4 ) = prot_array%dPreyP(iPrey)
                fl ( prInc + 5 ) = prot_array%dPreySi(iPrey)
            end do

            if ( ieee_is_nan(protC) ) write (*,*) 'ERROR: in ProtistCM, NaN in protC in segment:', iseg
            if ( ieee_is_nan(Cfix) )  write (*,*) 'ERROR: in ProtistCM, NaN in Cfix in segment:' , iseg
            if ( ieee_is_nan(totR) )  write (*,*) 'ERROR: in ProtistCM, NaN in totR in segment:' , iseg
            if ( ieee_is_nan(mrt) )   write (*,*) 'ERROR: in ProtistCM, NaN in mrt in segment:'  , iseg
            if ( ieee_is_nan(NC) )    write (*,*) 'ERROR: in ProtistCM, NaN in NC in segment:'   , iseg
            if ( ieee_is_nan(PC) )    write (*,*) 'ERROR: in ProtistCM, NaN in PC in segment:'   , iseg
            if ( ieee_is_nan(ChlC) )  write (*,*) 'ERROR: in ProtistCM, NaN in ChlC in segment:' , iseg
            if ( ieee_is_nan(ingC) )  write (*,*) 'ERROR: in ProtistCM, NaN in ingC in segment:' , iseg

        enddo speciesLoop ! end loop over species

        endif ! end if check for dry cell

        !allocate pointers
        iflux = iflux + noflux
        ipnt(1:ipointLength) = ipnt(1:ipointLength) + increm(1:ipointLength)

    enddo segmentLoop ! end loop over segments


    ! deallocation of prey input array
    call deallocate_prot_array(prot_array)
    deallocate (ipnt)
    return
end ! end subroutine
