!!  Copyright (C)  Stichting Deltares, 2012-2022.
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
    integer, parameter :: nrIndInp = 8    !   nr of species independent input items
    integer, parameter :: nrSpecInp = 41  !   nr of inputs per species
    integer, parameter :: nrSpecOut = 41  !   nr of outputs per species
    integer, parameter :: nrSpecFlux = 25 !   nr of fluxes per species
    integer, parameter :: nrPreyInp = 8   !   nr of inputs per prey

!   nrInputs  = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec) = 8 + 2 * 41 + 4 * (8 + 2) = 130
!   nrOutputs = nrSpec * nrSpecOut = 2 * 22 = 44
!   plen = nrInputs + nrOutputs = 176
!   nrFluxes  = nrSpec * (nrSpexFlx + nrPrey * nrLossFluxes) = 2 * (25 + 4 * 5) = 90

    integer, parameter :: plen = 176 ! total length of the PMSA input and output array
    integer ipnt(plen)    ! Local work array for the pointering                                    
    integer iseg          ! Local loop counter for computational element loop                      
    integer ioq
    integer iflux   
    integer ikmrk1        ! first segment attribute
          
    integer ispec         ! local species number counter
    integer spInc         ! local species PMSA number counter
    integer inpItems      ! nr of input items need for output PMSA
    
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
     real    totR, Cu                                    ! respiration and C-growth
     real    mrt, mrtFrAut, mrtFrDet                     ! mortality to detritus and autolysis
     
     ! other parameters
     real, parameter :: wTurb = 0.0 ! this needs to be an input!!!!
     
     ! loop counter 
     integer iPrey      ! counter for loops

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
    ipnt(1:plen) = ipoint(1:plen)
           
    iflux = 0
    
    ! segment and species independent items
    nrSpec    = nint(PMSA(ipnt(   1 )))   !   total nr species implemented in process                (-)
    nrPrey    = nint(PMSA(ipnt(   2 )))   !   nr of prey species implemented                         (-)


    ! allocation of prey input array
    call allocate_prot_array(prot_array,nrPrey)


      
    ! length of the PMSA input array. 
    inpItems = nrIndInp + nrSpec * nrSpecInp + nrPrey * (nrPreyInp + nrSpec)


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
        speciesLoop: do iSpec = 0, (nrSpec-1)

            spInc = nrSpecInp * iSpec

            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( nrIndInp +  1 + spInc ))   !     C-biomass                                              (gC m-3)
            
            ! skip if biomass is below threshold
            if (protC <= threshCmass) then 
                cycle speciesLoop
            end if

            protChl      = PMSA(ipnt( nrIndInp +  2 + spInc ))   !     Chl-biomass                                            (gChl m-3)   
            protN        = PMSA(ipnt( nrIndInp +  3 + spInc ))   !     N-biomass                                              (gN m-3)   
            protP        = PMSA(ipnt( nrIndInp +  4 + spInc ))   !     P-biomass                                              (gP m-3)
            AEm          = PMSA(ipnt( nrIndInp +  5 + spInc ))   !     maximum assimilation efficiency (AE)                   (-)
            AEo          = PMSA(ipnt( nrIndInp +  6 + spInc ))   !     minimum AE                                             (-)
            alpha        = PMSA(ipnt( nrIndInp +  7 + spInc ))   !     alpha for photosynthesis in protist                    (Figure this out!)  
            CcellProt    = PMSA(ipnt( nrIndInp +  8 + spInc ))   !     C content of protist cell                              (pgC cell-1) 
            ChlCm        = PMSA(ipnt( nrIndInp +  9 + spInc ))   !     maximum cellular Chl:C ratio                           (gChl gC-1)
            ChlCo        = PMSA(ipnt( nrIndInp + 10 + spInc ))   !     minimum cellular Chl:C ratio                           (gChl gC-1)
            CR           = PMSA(ipnt( nrIndInp + 11 + spInc ))   !     catabolic respiration quotient                         (-)
            FrAut        = PMSA(ipnt( nrIndInp + 12 + spInc ))   !     fraction of mortality to autolysis                     (-)
            FrDet        = PMSA(ipnt( nrIndInp + 13 + spInc ))   !     fraction of mortality to detritus                      (-)
            kAE          = PMSA(ipnt( nrIndInp + 14 + spInc ))   !     Control of AE in response to prey quality              (-)
            KtNH4        = PMSA(ipnt( nrIndInp + 15 + spInc ))   !     Kt for NH4 transport                                   (gN m-3)
            KtNO3        = PMSA(ipnt( nrIndInp + 16 + spInc ))   !     Kt for NO3 transport                                   (gN m-3)
            KtP          = PMSA(ipnt( nrIndInp + 17 + spInc ))   !     Kt for DIP transport                                   (gP m-3)
            MrtRT        = PMSA(ipnt( nrIndInp + 18 + spInc ))   !     mortality at reference temperature                     (-)
            maxPSreq     = PMSA(ipnt( nrIndInp + 19 + spInc ))   !     maximum C to come from PS                              (-)
            NCm          = PMSA(ipnt( nrIndInp + 20 + spInc ))   !     N:C that totally represses NH4 transport               (gN gC-1)
            NCo          = PMSA(ipnt( nrIndInp + 21 + spInc ))   !     minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( nrIndInp + 22 + spInc ))   !     N:C for growth under optimal conditions                (gN gC-1)
            NO3Cm        = PMSA(ipnt( nrIndInp + 23 + spInc ))   !     N:C that totally represses NO3 transport               (gN gC-1)
            NO3Copt      = PMSA(ipnt( nrIndInp + 24 + spInc ))   !     N:C for growth on NO3 under optimal conditions         (gN gC-1)
            optCR        = PMSA(ipnt( nrIndInp + 25 + spInc ))   !     proportion of prey captured by starved Prot            (-)
            PCm          = PMSA(ipnt( nrIndInp + 26 + spInc ))   !     PC maximum quota                                       (gP gC-1)
            PCo          = PMSA(ipnt( nrIndInp + 27 + spInc ))   !     PC minimum quota                                       (gP gC-1)
            PCoNCm       = PMSA(ipnt( nrIndInp + 28 + spInc ))   !     maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCoNCopt     = PMSA(ipnt( nrIndInp + 29 + spInc ))   !     optimum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCopt        = PMSA(ipnt( nrIndInp + 30 + spInc ))   !     PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( nrIndInp + 31 + spInc ))   !     proportion of current PS being leaked as DOC           (-)
            Q10          = PMSA(ipnt( nrIndInp + 32 + spInc ))   !     Q10 for UmRT                                           (-)
            rProt        = PMSA(ipnt( nrIndInp + 33 + spInc ))   !     radius of nutrient repleted protist cell               (um)
            redco        = PMSA(ipnt( nrIndInp + 34 + spInc ))   !     C respired to support nitrate reduction for NH4        (gC gN-1)
            relPhag      = PMSA(ipnt( nrIndInp + 35 + spInc ))   !     rel. phagotrophy in dark : in light                    (-)
            relPS        = PMSA(ipnt( nrIndInp + 36 + spInc ))   !     relative PSmax:Umax on phototrophy                     (-)
            ReUmNH4      = PMSA(ipnt( nrIndInp + 37 + spInc ))   !     max. growth rate supported by NH4-N:Umax               (-)
            ReUmNO3      = PMSA(ipnt( nrIndInp + 38 + spInc ))   !     max. growth rate supported by NO3-N:Umax               (-)
            RT           = PMSA(ipnt( nrIndInp + 39 + spInc ))   !     reference temperature for UmRT                         (deg C)
            SDA          = PMSA(ipnt( nrIndInp + 40 + spInc ))   !     specific dynamic action                                (-)
            UmRT         = PMSA(ipnt( nrIndInp + 41 + spInc ))   !     maximum growth rate at reference T                     (d-1) 
            
                       
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
            upNO3 = uptakeNO3(PCoNCm, PCu, NC, NC, NCo, NO3Copt, NO3Cm, UmT, ReUmNO3, NO3, KtNO3) 

            ! Calculate photosynthesis related equation --------------------------------------- 
            ! Units: gC gC-1 d-1
            ! I do not like the variable maxPSreq. Not measureable and pretty "strong" influence
            PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPCu, BR, PSDOC)
            PS   = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
            Cfix = netPS(PS, PSDOC)
            
            ! rate of (positive) net phototrophy
            ! Units: gC gC-1 d-1     
            CfixPS = Cfix - totalRespiration(redco, upNO3, upNH4, 0.0, 0.0, 0.0, 0.0)
                        
            ! Calculate chlorophyll synthesis and degradation --------------------------------------- 
            ! Units: gChl gC-1 d-1          
            synChl = synthesisChl(ChlC, ChlCo, ChlCm, UmT, maxPSreq, NPCu, Cfix, PSqm)
            degChl = degradeChl(ChlC, ChlCm, UmT, NPCu)

            !! PHAGOTROHY -------------------------------------------------------------------------------    
            
            call initialize_prot_array(prot_array,nrPrey, PMSA, plen, ipnt, nrIndInp, nrSpec, nrSpecInp, iSpec, (nrPreyInp + nrSpec))

            
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
            PMSA(ipnt( inpItems +  1 + iSpec * nrSpecOut )) = NC 
            PMSA(ipnt( inpItems +  2 + iSpec * nrSpecOut )) = PC 
            PMSA(ipnt( inpItems +  3 + iSpec * nrSpecOut )) = ChlC 
            PMSA(ipnt( inpItems +  4 + iSpec * nrSpecOut )) = UmT 
            PMSA(ipnt( inpItems +  5 + iSpec * nrSpecOut )) = BR
            PMSA(ipnt( inpItems +  6 + iSpec * nrSpecOut )) = NCu 
            PMSA(ipnt( inpItems +  7 + iSpec * nrSpecOut )) = PCu 
            PMSA(ipnt( inpItems +  8 + iSpec * nrSpecOut )) = NPCu
            PMSA(ipnt( inpItems +  9 + iSpec * nrSpecOut )) = mot
            PMSA(ipnt( inpItems + 10 + iSpec * nrSpecOut )) = upP 
            PMSA(ipnt( inpItems + 11 + iSpec * nrSpecOut )) = upNH4 
            PMSA(ipnt( inpItems + 12 + iSpec * nrSpecOut )) = upNO3 
            PMSA(ipnt( inpItems + 13 + iSpec * nrSpecOut )) = PSqm 
            PMSA(ipnt( inpItems + 14 + iSpec * nrSpecOut )) = PS
            PMSA(ipnt( inpItems + 15 + iSpec * nrSpecOut )) = Cfix 
            PMSA(ipnt( inpItems + 16 + iSpec * nrSpecOut )) = CfixPS
            PMSA(ipnt( inpItems + 17 + iSpec * nrSpecOut )) = synChl
            PMSA(ipnt( inpItems + 18 + iSpec * nrSpecOut )) = degChl
            PMSA(ipnt( inpItems + 19 + iSpec * nrSpecOut )) = sumCP
            PMSA(ipnt( inpItems + 20 + iSpec * nrSpecOut )) = ingNC
            PMSA(ipnt( inpItems + 21 + iSpec * nrSpecOut )) = ingPC
            PMSA(ipnt( inpItems + 22 + iSpec * nrSpecOut )) = ppNC
            PMSA(ipnt( inpItems + 23 + iSpec * nrSpecOut )) = ppPC
            PMSA(ipnt( inpItems + 24 + iSpec * nrSpecOut )) = stoichP
            PMSA(ipnt( inpItems + 25 + iSpec * nrSpecOut )) = opAE
            PMSA(ipnt( inpItems + 26 + iSpec * nrSpecOut )) = reqPred
            PMSA(ipnt( inpItems + 27 + iSpec * nrSpecOut )) = maxIng
            PMSA(ipnt( inpItems + 28 + iSpec * nrSpecOut )) = ingSat
            PMSA(ipnt( inpItems + 29 + iSpec * nrSpecOut )) = ingC  
            PMSA(ipnt( inpItems + 30 + iSpec * nrSpecOut )) = assC  
            PMSA(ipnt( inpItems + 31 + iSpec * nrSpecOut )) = ingN
            PMSA(ipnt( inpItems + 32 + iSpec * nrSpecOut )) = ingP
            PMSA(ipnt( inpItems + 33 + iSpec * nrSpecOut )) = assN
            PMSA(ipnt( inpItems + 34 + iSpec * nrSpecOut )) = assP
            PMSA(ipnt( inpItems + 35 + iSpec * nrSpecOut )) = totR 
            PMSA(ipnt( inpItems + 36 + iSpec * nrSpecOut )) = Cu
            PMSA(ipnt( inpItems + 37 + iSpec * nrSpecOut )) = mrt 
            PMSA(ipnt( inpItems + 38 + iSpec * nrSpecOut )) = mrtFrAut 
            PMSA(ipnt( inpItems + 39 + iSpec * nrSpecOut )) = mrtFrDet
            PMSA(ipnt( inpItems + 40 + iSpec * nrSpecOut )) = preyFlag
            PMSA(ipnt( inpItems + 41 + iSpec * nrSpecOut )) = lightInh

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
            fl (  1 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dNH4up    
            fl (  2 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dNO3up    
            fl (  3 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dPup      
            fl (  4 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dCfix     
            fl (  5 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dChlsyn   
            fl (  6 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dChldeg   
            fl (  7 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dCresp    
            fl (  8 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dDOCleak    
            fl (  9 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dDOCvoid    
            fl ( 10 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dNH4out   
            fl ( 11 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dPout     
            fl ( 12 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dCeat     
            fl ( 13 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dNeat     
            fl ( 14 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dPeat     
            fl ( 15 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dPOCout   
            fl ( 16 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dPONout   
            fl ( 17 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dPOPout   
            fl ( 18 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dAutC     
            fl ( 19 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dDetC     
            fl ( 20 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dAutN     
            fl ( 21 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dDetN     
            fl ( 22 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dAutP     
            fl ( 23 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dDetP     
            fl ( 24 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dAutChl   
            fl ( 25 + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux )  = dDetChl   
            
            ! Prey losses through pred ing. ----------------------------------------------------  
                            
            ! ingestion of nut of iPrey through iPred gNut m-3 d-1  
            prot_array%dPreyC    = protC * (ingC * prot_array%propPrey)   
            prot_array%dPreyChl  = prot_array%dPreyC * (prot_array%preyChl / prot_array%preyC)            
            prot_array%dPreyN    = prot_array%dPreyC * (prot_array%preyN / prot_array%preyC) 
            prot_array%dPreyP    = prot_array%dPreyC * (prot_array%preyP / prot_array%preyC) 
            prot_array%dPreySi   = prot_array%dPreyC * (prot_array%preySi / prot_array%preyC)   

            ! loop over prey ingestion fluxes
            do iPrey = 0, (nrPrey - 1)                                
                ! (nr prey independent fluxes + prey Flux # + loop) + (move on to next predator) + total number of fluxes
                fl ( (nrSpecFlux + 1 + iPrey * nrLossFluxes) + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux ) = prot_array%dPreyC(iPrey + 1)  
                fl ( (nrSpecFlux + 2 + iPrey * nrLossFluxes) + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux ) = prot_array%dPreyChl(iPrey + 1)
                fl ( (nrSpecFlux + 3 + iPrey * nrLossFluxes) + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux ) = prot_array%dPreyN(iPrey + 1)  
                fl ( (nrSpecFlux + 4 + iPrey * nrLossFluxes) + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux ) = prot_array%dPreyP(iPrey + 1)  
                fl ( (nrSpecFlux + 5 + iPrey * nrLossFluxes) + (nrSpecFlux + nrPrey * nrLossFluxes) * iSpec + iflux ) = prot_array%dPreySi(iPrey + 1) 
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
        ipnt(1:plen) = ipnt(1:plen) + increm(1:plen)

    enddo segmentLoop ! end loop over segments
    
    
    ! deallocation of prey input array
    call deallocate_prot_array(prot_array)
    return
end ! end subroutine 
