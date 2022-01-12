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
    integer, parameter :: plen = 205 ! total length of the PMSA input and output array
    real(4) pmsa(*)      ! I/O Process Manager System Array, window of routine to process library     
    real(4) fl(*)        ! O  Array of fluxes made by this process in mass/volume/time               
    integer ipoint(plen) ! I  Array of pointers in pmsa to get and store the data                    
    integer increm(plen) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying 
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
    integer ipnt(plen)    ! Local work array for the pointering                                    
    integer iseg          ! Local loop counter for computational element loop                      
    integer ioq
    integer iflux   
    integer ikmrk1        ! first segment attribute
          
    integer ispec         ! local species number counter
    integer spInc         ! local species PMSA number counter
    integer inpItems      ! nr of input items need for output PMSA
    integer nrSp_par
    
     !input parameters
     integer    maxNrSp, nrSp, nrSpCon, nrSpInd     ! constant and species numbers   
     integer    maxNrPr                             ! maxNrPrey
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
    ipnt        = ipoint
           
    iflux = 0
    
    ! segment and species independent items
    maxNrSp   = PMSA(ipnt(   1 ))   !   total nr species implemented in process                (dl)
    nrSp      = PMSA(ipnt(   2 ))   !   nr of species to be modelled                           (dl)                
    nrSpCon   = PMSA(ipnt(   3 ))   !   nr of species dependent items                          (dl)                
    nrSpInd   = PMSA(ipnt(   4 ))   !   nr of species independent items                        (dl)  
    maxNrPr   = PMSA(ipnt(   5 ))   !   nr of prey species implemented                         (dl)
    nrSp_par  = PMSA(ipnt(   6 ))   !   nr of parameters per species       
    
    
    ! allocation of prey input array
    call allocate_prot_array(prot_array,maxNrPr)


      
    ! length of the PMSA input array. 
    inpItems = nrSpInd   + maxNrSp * nrSpCon + maxNrPr * nrSp_par


    ! segment loop
    segmentLoop: do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then
            
        ! species independent items
        PO4          = PMSA(ipnt(  7 ))  !    initial external DIP                                   (gP m-3)
        NH4          = PMSA(ipnt(  8 ))  !    initial external NH4                                   (gN m-3)
        NO3          = PMSA(ipnt(  9 ))  !    initial external NO3                                   (gN m-3)
        Temp         = PMSA(ipnt( 10 ))  !    ambient water temperature                              (oC)               
        PFD          = PMSA(ipnt( 11 ))  !    from rad to photon flux density                        (umol photon m-2)           
        atten        = PMSA(ipnt( 12 ))  !    attenuation of light by water + plankton Chl           (dl)                            
        exat         = PMSA(ipnt( 13 ))  !    -ve exponent of attenuation                            (dl)    
                      
        ! species loop
        speciesLoop: do iSpec = 0, (nrSp-1)

            spInc = nrSpCon * iSpec

            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( nrSpInd +  1 + spInc ))   !     C-biomass                                              (gC m-3)  
            protChl      = PMSA(ipnt( nrSpInd +  2 + spInc ))   !     Chl-biomass                                            (gChl m-3)   
            protN        = PMSA(ipnt( nrSpInd +  3 + spInc ))   !     N-biomass                                              (gN m-3)   
            protP        = PMSA(ipnt( nrSpInd +  4 + spInc ))   !     P-biomass                                              (gP m-3)
            AEm          = PMSA(ipnt( nrSpInd +  5 + spInc ))   !     maximum assimilation efficiency (AE)                   (dl)
            AEo          = PMSA(ipnt( nrSpInd +  6 + spInc ))   !     minimum AE                                             (dl)
            alpha        = PMSA(ipnt( nrSpInd +  7 + spInc ))   !     alpha for photosynthesis in protist                    (Figure this out!)  
            CcellProt    = PMSA(ipnt( nrSpInd +  8 + spInc ))   !     C content of protist cell                              (pgC cell-1) 
            ChlCm        = PMSA(ipnt( nrSpInd +  9 + spInc ))   !     maximum cellular Chl:C ratio                           (gChl gC-1)
            ChlCo        = PMSA(ipnt( nrSpInd + 10 + spInc ))   !     minimum cellular Chl:C ratio                           (gChl gC-1)
            CR           = PMSA(ipnt( nrSpInd + 11 + spInc ))   !     catabolic respiration quotient                         (dl)
            FrAut        = PMSA(ipnt( nrSpInd + 12 + spInc ))   !     fraction of mortality to autolysis                     (dl)
            FrDet        = PMSA(ipnt( nrSpInd + 13 + spInc ))   !     fraction of mortality to detritus                      (dl)
            kAE          = PMSA(ipnt( nrSpInd + 14 + spInc ))   !     Control of AE in response to prey quality              (dl)
            KtNH4        = PMSA(ipnt( nrSpInd + 15 + spInc ))   !     Kt for NH4 transport                                   (gN m-3)
            KtNO3        = PMSA(ipnt( nrSpInd + 16 + spInc ))   !     Kt for NO3 transport                                   (gN m-3)
            KtP          = PMSA(ipnt( nrSpInd + 17 + spInc ))   !     Kt for DIP transport                                   (gP m-3)
            MrtRT        = PMSA(ipnt( nrSpInd + 18 + spInc ))   !     mortality at reference temperature                     (dl)
            maxPSreq     = PMSA(ipnt( nrSpInd + 19 + spInc ))   !     maximum C to come from PS                              (dl)
            NCm          = PMSA(ipnt( nrSpInd + 20 + spInc ))   !     N:C that totally represses NH4 transport               (gN gC-1)
            NCo          = PMSA(ipnt( nrSpInd + 21 + spInc ))   !     minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( nrSpInd + 22 + spInc ))   !     N:C for growth under optimal conditions                (gN gC-1)
            NO3Cm        = PMSA(ipnt( nrSpInd + 23 + spInc ))   !     N:C that totally represses NO3 transport               (gN gC-1)
            NO3Copt      = PMSA(ipnt( nrSpInd + 24 + spInc ))   !     N:C for growth on NO3 under optimal conditions         (gN gC-1)
            optCR        = PMSA(ipnt( nrSpInd + 25 + spInc ))   !     proportion of prey captured by starved Prot            (dl)
            PCm          = PMSA(ipnt( nrSpInd + 26 + spInc ))   !     PC maximum quota                                       (gP gC-1)
            PCo          = PMSA(ipnt( nrSpInd + 27 + spInc ))   !     PC minimum quota                                       (gP gC-1)
            PCoNCm       = PMSA(ipnt( nrSpInd + 28 + spInc ))   !     maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCoNCopt     = PMSA(ipnt( nrSpInd + 29 + spInc ))   !     optimum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCopt        = PMSA(ipnt( nrSpInd + 30 + spInc ))   !     PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( nrSpInd + 31 + spInc ))   !     proportion of current PS being leaked as DOC           (dl)
            Q10          = PMSA(ipnt( nrSpInd + 32 + spInc ))   !     Q10 for UmRT                                           (dl)
            rProt        = PMSA(ipnt( nrSpInd + 33 + spInc ))   !     radius of nutrient repleted protist cell               (um)
            redco        = PMSA(ipnt( nrSpInd + 34 + spInc ))   !     C respired to support nitrate reduction for NH4        (gC gN-1)
            relPhag      = PMSA(ipnt( nrSpInd + 35 + spInc ))   !     rel. phagotrophy in dark : in light                    (dl)
            relPS        = PMSA(ipnt( nrSpInd + 36 + spInc ))   !     relative PSmax:Umax on phototrophy                     (dl)
            ReUmNH4      = PMSA(ipnt( nrSpInd + 37 + spInc ))   !     max. growth rate supported by NH4-N:Umax               (dl)
            ReUmNO3      = PMSA(ipnt( nrSpInd + 38 + spInc ))   !     max. growth rate supported by NO3-N:Umax               (dl)
            RT           = PMSA(ipnt( nrSpInd + 39 + spInc ))   !     reference temperature for UmRT                         (deg C)
            SDA          = PMSA(ipnt( nrSpInd + 40 + spInc ))   !     specific dynamic action                                (dl)
            UmRT         = PMSA(ipnt( nrSpInd + 41 + spInc ))   !     maximum growth rate at reference T                     (d-1) 
            
            if (protC <= threshCmass) then 
                cycle speciesLoop
            end if
                        
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
            ! Units: dl
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
            
            call initialize_prot_array(prot_array,maxNrPr, PMSA, plen, ipnt, nrSpInd, maxNrSp, nrSpCon, iSpec, nrSp_par)

            
            ! for output [dl]
            preyFlag = sum(prot_array%preyFlag)
            
            
            !! FOOD QUANTITY -------------------------------------------------------------------------------    
            ! reduction of phagotrophy during night 
            ! Units: dl
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
            PMSA(ipnt( inpItems +  1 + iSpec * nrSpCon )) = NC 
            PMSA(ipnt( inpItems +  2 + iSpec * nrSpCon )) = PC 
            PMSA(ipnt( inpItems +  3 + iSpec * nrSpCon )) = ChlC 
            PMSA(ipnt( inpItems +  4 + iSpec * nrSpCon )) = UmT 
            PMSA(ipnt( inpItems +  5 + iSpec * nrSpCon )) = BR
            PMSA(ipnt( inpItems +  6 + iSpec * nrSpCon )) = NCu 
            PMSA(ipnt( inpItems +  7 + iSpec * nrSpCon )) = PCu 
            PMSA(ipnt( inpItems +  8 + iSpec * nrSpCon )) = NPCu
            PMSA(ipnt( inpItems +  9 + iSpec * nrSpCon )) = mot
            PMSA(ipnt( inpItems + 10 + iSpec * nrSpCon )) = upP 
            PMSA(ipnt( inpItems + 11 + iSpec * nrSpCon )) = upNH4 
            PMSA(ipnt( inpItems + 12 + iSpec * nrSpCon )) = upNO3 
            PMSA(ipnt( inpItems + 13 + iSpec * nrSpCon )) = PSqm 
            PMSA(ipnt( inpItems + 14 + iSpec * nrSpCon )) = PS
            PMSA(ipnt( inpItems + 15 + iSpec * nrSpCon )) = Cfix 
            PMSA(ipnt( inpItems + 16 + iSpec * nrSpCon )) = CfixPS
            PMSA(ipnt( inpItems + 17 + iSpec * nrSpCon )) = synChl
            PMSA(ipnt( inpItems + 18 + iSpec * nrSpCon )) = degChl
            PMSA(ipnt( inpItems + 19 + iSpec * nrSpCon )) = sumCP
            PMSA(ipnt( inpItems + 20 + iSpec * nrSpCon )) = ingNC
            PMSA(ipnt( inpItems + 21 + iSpec * nrSpCon )) = ingPC
            PMSA(ipnt( inpItems + 22 + iSpec * nrSpCon )) = ppNC
            PMSA(ipnt( inpItems + 23 + iSpec * nrSpCon )) = ppPC
            PMSA(ipnt( inpItems + 24 + iSpec * nrSpCon )) = stoichP
            PMSA(ipnt( inpItems + 25 + iSpec * nrSpCon )) = opAE
            PMSA(ipnt( inpItems + 26 + iSpec * nrSpCon )) = reqPred
            PMSA(ipnt( inpItems + 27 + iSpec * nrSpCon )) = maxIng
            PMSA(ipnt( inpItems + 28 + iSpec * nrSpCon )) = ingSat
            PMSA(ipnt( inpItems + 29 + iSpec * nrSpCon )) = ingC  
            PMSA(ipnt( inpItems + 30 + iSpec * nrSpCon )) = assC  
            PMSA(ipnt( inpItems + 31 + iSpec * nrSpCon )) = ingN
            PMSA(ipnt( inpItems + 32 + iSpec * nrSpCon )) = ingP
            PMSA(ipnt( inpItems + 33 + iSpec * nrSpCon )) = assN
            PMSA(ipnt( inpItems + 34 + iSpec * nrSpCon )) = assP
            PMSA(ipnt( inpItems + 35 + iSpec * nrSpCon )) = totR 
            PMSA(ipnt( inpItems + 36 + iSpec * nrSpCon )) = Cu
            PMSA(ipnt( inpItems + 37 + iSpec * nrSpCon )) = mrt 
            PMSA(ipnt( inpItems + 38 + iSpec * nrSpCon )) = mrtFrAut 
            PMSA(ipnt( inpItems + 39 + iSpec * nrSpCon )) = mrtFrDet
            PMSA(ipnt( inpItems + 40 + iSpec * nrSpCon )) = preyFlag
            PMSA(ipnt( inpItems + 41 + iSpec * nrSpCon )) = lightInh

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
            fl (  1 + (25 + maxNrPr * 5) * iSpec + iflux )  = dNH4up    
            fl (  2 + (25 + maxNrPr * 5) * iSpec + iflux )  = dNO3up    
            fl (  3 + (25 + maxNrPr * 5) * iSpec + iflux )  = dPup      
            fl (  4 + (25 + maxNrPr * 5) * iSpec + iflux )  = dCfix     
            fl (  5 + (25 + maxNrPr * 5) * iSpec + iflux )  = dChlsyn   
            fl (  6 + (25 + maxNrPr * 5) * iSpec + iflux )  = dChldeg   
            fl (  7 + (25 + maxNrPr * 5) * iSpec + iflux )  = dCresp    
            fl (  8 + (25 + maxNrPr * 5) * iSpec + iflux )  = dDOCleak    
            fl (  9 + (25 + maxNrPr * 5) * iSpec + iflux )  = dDOCvoid    
            fl ( 10 + (25 + maxNrPr * 5) * iSpec + iflux )  = dNH4out   
            fl ( 11 + (25 + maxNrPr * 5) * iSpec + iflux )  = dPout     
            fl ( 12 + (25 + maxNrPr * 5) * iSpec + iflux )  = dCeat     
            fl ( 13 + (25 + maxNrPr * 5) * iSpec + iflux )  = dNeat     
            fl ( 14 + (25 + maxNrPr * 5) * iSpec + iflux )  = dPeat     
            fl ( 15 + (25 + maxNrPr * 5) * iSpec + iflux )  = dPOCout   
            fl ( 16 + (25 + maxNrPr * 5) * iSpec + iflux )  = dPONout   
            fl ( 17 + (25 + maxNrPr * 5) * iSpec + iflux )  = dPOPout   
            fl ( 18 + (25 + maxNrPr * 5) * iSpec + iflux )  = dAutC     
            fl ( 19 + (25 + maxNrPr * 5) * iSpec + iflux )  = dDetC     
            fl ( 20 + (25 + maxNrPr * 5) * iSpec + iflux )  = dAutN     
            fl ( 21 + (25 + maxNrPr * 5) * iSpec + iflux )  = dDetN     
            fl ( 22 + (25 + maxNrPr * 5) * iSpec + iflux )  = dAutP     
            fl ( 23 + (25 + maxNrPr * 5) * iSpec + iflux )  = dDetP     
            fl ( 24 + (25 + maxNrPr * 5) * iSpec + iflux )  = dAutChl   
            fl ( 25 + (25 + maxNrPr * 5) * iSpec + iflux )  = dDetChl   
            !iSPec * 25
            
            ! Prey losses through pred ing. ----------------------------------------------------  
                            
            ! ingestion of nut of iPrey through iPred gNut m-3 d-1  
            prot_array%dPreyC    = protC * (ingC * prot_array%propPrey)   
            prot_array%dPreyChl  = prot_array%dPreyC * (prot_array%preyChl / prot_array%preyC)            
            prot_array%dPreyN    = prot_array%dPreyC * (prot_array%preyN / prot_array%preyC) 
            prot_array%dPreyP    = prot_array%dPreyC * (prot_array%preyP / prot_array%preyC) 
            prot_array%dPreySi   = prot_array%dPreyC * (prot_array%preySi / prot_array%preyC)   

            ! loop over prey ingestion fluxes
            do iPrey = 0, (maxNrPr - 1)                                
                ! (nr prey independent fluxes + prey Flux # + loop) + (move on to next predator) + total number of fluxes
                fl ( (25 + 1 + iPrey * 5) + (25 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyC(iPrey + 1)  
                fl ( (25 + 2 + iPrey * 5) + (25 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyChl(iPrey + 1)
                fl ( (25 + 3 + iPrey * 5) + (25 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyN(iPrey + 1)  
                fl ( (25 + 4 + iPrey * 5) + (25 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyP(iPrey + 1)  
                fl ( (25 + 5 + iPrey * 5) + (25 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreySi(iPrey + 1) 
            end do  
            
            if ( ieee_is_nan(protC) ) write (*,*) '(''ERROR: NaN in protC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(Cfix) )  write (*,*) '(''ERROR: NaN in Cfix in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(totR) )  write (*,*) '(''ERROR: NaN in totR in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(mrt) )   write (*,*) '(''ERROR: NaN in mrt in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(NC) )    write (*,*) '(''ERROR: NaN in NC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(PC) )    write (*,*) '(''ERROR: NaN in PC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(ChlC) )  write (*,*) '(''ERROR: NaN in ChlC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(ingC) )  write (*,*) '(''ERROR: NaN in ingC in segment:'', i10)' ,    iseg
               
        enddo speciesLoop ! end loop over species 

        endif ! end if check for dry cell 

        !allocate pointers
        iflux = iflux + noflux
        ipnt = ipnt + increm

    enddo segmentLoop ! end loop over segments
    
    
    ! deallocation of prey input array
    call deallocate_prot_array(prot_array)
    return
end ! end subroutine 
