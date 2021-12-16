!!  Copyright (C)  Stichting Deltares, 2012-2021.
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
subroutine PRONCM     ( pmsa   , fl     , ipoint , increm, noseg , &                            
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &                            
                            noq3   , noq4   )     
! dont touch next line, replace name though
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'PRONCM' :: PRONCM                                     
!                                                                                                     
!*******************************************************************************                      
!  
use protist_math_functions
use protist_cell_functions
use protist_types
use protist_phagotrophy_functions
use protist_photosynthesis_functions
use protist_food_functions


    IMPLICIT NONE                                                                                   
!                                                                                                     
!     Type    Name         I/O Description                                                            
!          
    integer, parameter :: plen = 157 ! total length of the PMSA input and output array
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
    integer iSp
    
     ! INPUT PARAMETERS  
     integer    maxNrSp, nrSp, nrSpCon, nrSpInd     ! constant and species numbers   
     integer    maxNrPr                             ! maxNrPrey
     real       relPhag                             ! feeding night:day
     real       UmRT, Q10, RT, CR                   ! growth and respiration rate calculation
     real       NCm, PCm                            ! maximum NC and PC quotas
     real       NCo, PCo                            ! minimum NC and PC quotas
     real       NCopt, PCopt                        ! optimal NC and PC quotas
     real       ChlCm, degChl
     real       CcellProt, rProt                      ! parameters for protozooplankton cell
     real       optCR                               ! parameters for encounter
     real       kAE, AEm, AEo                       ! parameters for assimilation efficiency
     real       SDA                                 ! specific dynamic action
     real       MrtRT, FrAut, FrDet                 ! reference mortality and fractions
     real       redco, PSDOC, maxPSreq, relPS       ! photosynthesis related input
     real       alpha                               ! inital slope

     ! INPUT STATE VARIABLES
     real    protC, protN, protP, protChl           ! protist state variables
     real    Temp                                   ! physical abiotic variables

     real    PFD, atten, exat                      ! available light and extinction
         
     ! AUXILIARIES
     real    lightInh        ! inhibtion of feeding in dark 
     real    NC, PC, ChlC    ! nutrient quotas
     real    UmT, BR         ! growth and repsiration rates
     real    NCu, PCu, NPCu  ! nutrient limitations
     real    mot             ! motility
     ! food quantity
     real    sumCP        ! total captured prey
     real    ingNC, ingPC ! total ingested N and P 
     real    preyFlag     ! sum of preyFlag (can be 0 = both low, 1 = 1 ok, 2 = both ok)
     ! food quality
     real    stoichP, ppNC, ppPC                    ! stoichiometry comparison                 
     real    opAE                                   ! assimilation efficiency
     real    maxIng, ingSat, ingC, ingN, ingP, KI   ! ingestion
     real    assC, assN, assP                       ! assimilation 
     ! ingestion and assimilation
     real    totR, Cu                               ! respiration and C-growth
     real    mrt, mrtFrAut, mrtFrDet                ! mortality to detritus and autolysis
     ! photosynthesis
     real    PSqm, PS, Cfix
            
     ! other parameters
     real, parameter :: wTurb = 0.0 ! this needs to be an input from model eventually!!!!
     real(8),  parameter :: PI_8  = 4 * atan (1.0_8) 

     ! loop counter 
     integer iPrey      ! counter for loops

     ! Fluxes
     real   dCeat, dNeat, dPeat                         ! assimilation fluxes
     real   dCresp                                      ! respiration flux
     real   dCfix, dChldeg, dChlout                     ! photosynthesis and Chl degradation fluxes
     real   dDOCleak, dDOCvoid                          ! DOC leakage
     real   dPOCout, dPONout, dPOPout                   ! voiding organic fluxes
     real   dNH4out, dPout                              ! voding inorganic fluxes
     real   dAutC, dAutN, dAutP, dAutChl                ! autolysis fluxes
     real   dDetC, dDetN, dDetP, dDetChl                ! detritus fluxes
     real   dChlup                                      ! uptake of prey chlorophyll

     
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
    nrSp_par  = PMSA(ipnt(   10 ))   !   nr of parameters per species                           (dl)
    iSp       = PMSA(ipnt(   11 ))   !   selector of species parameter (needed to share code with for protistCM)   (dl)
    
    
    ! allocation of prey input array
    call allocate_prot_array(prot_array,maxNrPr)

    
    
               
    ! length of the PMSA input array.         
    inpItems = nrSpInd   + maxNrSp * nrSpCon + maxNrPr * nrSp_par
         
    ! segment loop
    do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then    
            
            Temp      = PMSA(ipnt(   6 ))  !    temperature                                            (C)      
            PFD       = PMSA(ipnt(   7 ))  !    from rad to photon flux density                        (umol photon m-2)           
            atten     = PMSA(ipnt(   8 ))  !    attenuation of light by water + plankton Chl           (dl)                            
            exat      = PMSA(ipnt(   9 ))  !    -ve exponent of attenuation                            (dl) 
      
        ! species loop
        do iSpec = 0, (nrSp-1)

            spInc = nrSpCon * iSpec
               
            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( nrSpInd   +  1 + spInc ))  ! C-biomass                                              (gC m-3)  
            protChl      = PMSA(ipnt( nrSpInd   +  2 + spInc ))  ! Chl-biomass                                            (gChl m-3)   
            protN        = PMSA(ipnt( nrSpInd   +  3 + spInc ))  ! N-biomass                                              (gN m-3)   
            protP        = PMSA(ipnt( nrSpInd   +  4 + spInc ))  ! P-biomass                                              (gP m-3)
            AEm          = PMSA(ipnt( nrSpInd   +  5 + spInc ))  ! maximum assimilation efficiency (AE)                   (dl)
            AEo          = PMSA(ipnt( nrSpInd   +  6 + spInc ))  ! minimum AE                                             (dl)
            alpha        = PMSA(ipnt( nrSpInd   +  7 + spInc ))  ! alpha for photosynthesis in protist                    (Figure this out!)   
            CcellProt    = PMSA(ipnt( nrSpInd   +  8 + spInc ))  ! C content of protist cell                              (pgC cell-1)
            ChlCm        = PMSA(ipnt( nrSpInd   +  9 + spInc ))  ! maximum cellular Chl:C ratio                           (gChl gC-1)
            CR           = PMSA(ipnt( nrSpInd   + 10 + spInc ))  ! catabolic respiration quotient                         (dl)
            degChl       = PMSA(ipnt( nrSpInd   + 11 + spInc ))  ! Chl degradation see Ghyoot 2017                        (d-1)
            FrAut        = PMSA(ipnt( nrSpInd   + 12 + spInc ))  ! fraction of mortality to autolysis                     (dl)
            FrDet        = PMSA(ipnt( nrSpInd   + 13 + spInc ))  ! fraction of mortality to detritus                      (dl)
            kAE          = PMSA(ipnt( nrSpInd   + 14 + spInc ))  ! Control of AE in response to prey quality              (dl)
            MrtRT        = PMSA(ipnt( nrSpInd   + 15 + spInc ))  ! mortality at reference temperature                     (dl)    
            maxPSreq     = PMSA(ipnt( nrSpInd   + 16 + spInc ))  ! maximum C to come from PS                              (dl)        
            NCm          = PMSA(ipnt( nrSpInd   + 17 + spInc ))  ! N:C that totally represses NH4 transport               (gN gC-1)
            NCo          = PMSA(ipnt( nrSpInd   + 18 + spInc ))  ! minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( nrSpInd   + 19 + spInc ))  ! N:C for growth under optimal conditions                (gN gC-1)    
            optCR        = PMSA(ipnt( nrSpInd   + 20 + spInc ))  ! proportion of prey captured by starved Zoo             (dl)        
            PCm          = PMSA(ipnt( nrSpInd   + 21 + spInc ))  ! PC maximum quota                                       (gP gC-1) 
            PCo          = PMSA(ipnt( nrSpInd   + 22 + spInc ))  ! PC minimum quota                                       (gP gC-1)
            PCopt        = PMSA(ipnt( nrSpInd   + 23 + spInc ))  ! PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( nrSpInd   + 24 + spInc ))  ! proportion of current PS being leaked as DOC           (dl)
            Q10          = PMSA(ipnt( nrSpInd   + 25 + spInc ))  ! Q10 for UmRT                                           (dl)
            rProt        = PMSA(ipnt( nrSpInd   + 26 + spInc ))  ! radius of nutrient repleted protist cell               (um)
            redco        = PMSA(ipnt( nrSpInd   + 27 + spInc ))  ! C respired to support nitrate reduction for NH4        (gC gN-1)
            relPhag      = PMSA(ipnt( nrSpInd   + 28 + spInc ))  ! rel. phagotrophy in dark : in light                    (dl)
            relPS        = PMSA(ipnt( nrSpInd   + 29 + spInc ))  ! relative PSmax:Umax on phototrophy                     (dl)
            RT           = PMSA(ipnt( nrSpInd   + 30 + spInc ))  ! reference temperature for UmRT                         (deg C)
            SDA          = PMSA(ipnt( nrSpInd   + 31 + spInc ))  ! specific dynamic action                                (dl)
            UmRT         = PMSA(ipnt( nrSpInd   + 32 + spInc ))  ! maximum growth rate at reference T                     (d-1) 
                       
            if (protC <= 1.0E-9) then 
                cycle
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
            
            ! Calculate nutrient status within cell compared to ideal status (nutrient status = 1) --------------------------------------- 
            ! Determine minimum of N-P-Si limitation; Liebig-style limitation of growth (NPCu)
            ! Units: dl
            NCu = statusNC(NC, NCo, NCopt)                        
            PCu = statusPC(PC, PCo, PCopt)
            NPCu = min(NCu, PCu)            
                                                
            ! swimming speed -------------------------------------------------------------------------------    
            ! Units: m s-1
            mot = motility(rProt) 
            

            
            call initialize_prot_array(prot_array,maxNrPr, PMSA, plen, ipnt, nrSpInd, maxNrSp, nrSpCon, iSp, nrSp_par)

            
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
            ! maximum ingestion 
            ! Units: gC gC-1 d-1
            maxIng = ((UmT + BR) / (1.0 - SDA)) / opAE
            call protistIngestion(maxIng, sumCP, ingNC, ingPC, KI, ingSat, ingC, ingN, ingP)

            
            
            ! ASSIMILATION ------------------------------------------------------------------------------- 
            ! assimilation of ingested prey
            ! Units: gC gC-1 d-1 / gNut gC-1 d-1
            assC = ingC * opAE
            assN = assC * NCopt            
            assP = assC * PCopt
                   
            ! Calculate photosynthesis related equation --------------------------------------- 
            ! Units: gC gC-1 d-1
            ! LS: I do not like the variable maxPSreq. Not measureable and pretty "strong" influence
            ! at the moment they keep all the chl from prey.... is this correct... ???
            ! only keep if C is low??? 
            ! NPCu set to 1 following Flynn and Mitra 2009 logic 
            ! can only use ingested Chl in next timestep
            PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPCu, BR, PSDOC)
            PS   = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
            Cfix = netPS(PS, PSDOC)
                                
            ! Calculate respiration   --------------------------------------- 
            ! Units: gC gC-1 d-1             
            ! protzoo cannot recover loss N
            if (protC >= 1.0E-5) then 
                totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)      
            else
                totR = 0.0
            end if
            !totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)    
            Cu   = CgrowthRate(Cfix, assC, totR)
                    
            ! Calculate mortality  --------------------------------------- 
            call protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut,mrtFrDet)
     

 
            
  
                   
            ! Output -------------------------------------------------------------------
               
            ! (input items + position of specific output item in vector + species loop * total number of output) 
            PMSA(ipnt( inpItems +   1 + iSpec * 34 )) = NC
            PMSA(ipnt( inpItems +   2 + iSpec * 34 )) = PC
            PMSA(ipnt( inpItems +   3 + iSpec * 34 )) = ChlC
            PMSA(ipnt( inpItems +   4 + iSpec * 34 )) = UmT
            PMSA(ipnt( inpItems +   5 + iSpec * 34 )) = BR 
            PMSA(ipnt( inpItems +   6 + iSpec * 34 )) = NCu
            PMSA(ipnt( inpItems +   7 + iSpec * 34 )) = PCu
            PMSA(ipnt( inpItems +   8 + iSpec * 34 )) = NPCu
            PMSA(ipnt( inpItems +   9 + iSpec * 34 )) = mot
            PMSA(ipnt( inpItems +  10 + iSpec * 34 )) = sumCP
            PMSA(ipnt( inpItems +  11 + iSpec * 34 )) = ingNC
            PMSA(ipnt( inpItems +  12 + iSpec * 34 )) = ingPC
            PMSA(ipnt( inpItems +  13 + iSpec * 34 )) = ppNC
            PMSA(ipnt( inpItems +  14 + iSpec * 34 )) = ppPC
            PMSA(ipnt( inpItems +  15 + iSpec * 34 )) = stoichP
            PMSA(ipnt( inpItems +  16 + iSpec * 34 )) = opAE
            PMSA(ipnt( inpItems +  17 + iSpec * 34 )) = maxIng
            PMSA(ipnt( inpItems +  18 + iSpec * 34 )) = ingSat
            PMSA(ipnt( inpItems +  19 + iSpec * 34 )) = ingC  
            PMSA(ipnt( inpItems +  20 + iSpec * 34 )) = assC  
            PMSA(ipnt( inpItems +  21 + iSpec * 34 )) = ingN
            PMSA(ipnt( inpItems +  22 + iSpec * 34 )) = ingP
            PMSA(ipnt( inpItems +  23 + iSpec * 34 )) = assN
            PMSA(ipnt( inpItems +  24 + iSpec * 34 )) = assP
            PMSA(ipnt( inpItems +  25 + iSpec * 34 )) = PSqm 
            PMSA(ipnt( inpItems +  26 + iSpec * 34 )) = PS   
            PMSA(ipnt( inpItems +  27 + iSpec * 34 )) = Cfix 
            PMSA(ipnt( inpItems +  28 + iSpec * 34 )) = totR
            PMSA(ipnt( inpItems +  29 + iSpec * 34 )) = Cu
            PMSA(ipnt( inpItems +  30 + iSpec * 34 )) = mrt
            PMSA(ipnt( inpItems +  31 + iSpec * 34 )) = mrtFrAut
            PMSA(ipnt( inpItems +  32 + iSpec * 34 )) = mrtFrDet
            PMSA(ipnt( inpItems +  33 + iSpec * 34 )) = preyFlag
            PMSA(ipnt( inpItems +  34 + iSpec * 34 )) = lightInh
            
            ! FLUXES -------------------------------------------------------------------   
            ! Protist gains------------------------------------------------------------                                 
            ! Protist growth through assimilation -----------------------------------------------------
            ! gX m-3 d-1 assimilation of X from prey
            dCeat = protC * assC
            dNeat = protC * assN    
            dPeat = protC * assP
                        
            ! gC m-3 d-1   total contribution to biomass growth from C-fixation
            dCfix = protC * Cfix
                                    
            ! Protist losses-----------------------------------------------------------            
            ! gChl m-3 d-1 Chl synthesis or degradation
            dChldeg = protChl * degChl
                        
            ! gChl m-3 d-1 voiding of protist Chl if interanl maximum is reached
            dChlout = voiding(protChl, protC, ChlCm)           
            
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
            
            ! gX m-3 d-1  rate of voiding of X as particulates
            dPOCout = protC * (ingC - assC)
            dPONout = protC * (ingN - assN) 
            dPOPout = protC * (ingP - assP) 
            
            ! gNut m-3 d-1 voiding of nutrient P and N if interanl maximum is reached
            dNH4out = voiding(protN, protC, NCopt)
            dPout   = voiding(protP, protC, PCopt)
                                    
            ! gNut m-3 d-1 mortality
            dAutC = protC * protC * mrtFrAut
            dDetC = protC * protC * mrtFrDet    
            dAutN = protN * protN * mrtFrAut
            dDetN = protN * protN * mrtFrDet            
            dAutP = protP * protP * mrtFrAut
            dDetP = protP * protP * mrtFrDet       
            dAutChl = protChl * protChl * mrtFrAut
            dDetChl = protChl * protChl * mrtFrDet
                        
   !         dAutC = protC * mrtFrAut
   !         dDetC = protC * mrtFrDet
   !         dAutN = protN * mrtFrAut
   !         dDetN = protN * mrtFrDet
   !         dAutP = protP * mrtFrAut
   !         dDetP = protP * mrtFrDet           
   !         dAutChl = protChl * mrtFrAut
            !dDetChl = protChl * mrtFrDet
                        
            ! Prey losses through pred ing. ----------------------------------------------------         
            ! ingestion of nut of iPrey through iPred gNut m-3 d-1  
            prot_array%dPreyC    = protC * (ingC * prot_array%propPrey)               
            prot_array%dPreyChl  = prot_array%dPreyC * (prot_array%preyChl / prot_array%preyC)            
            prot_array%dPreyN    = prot_array%dPreyC * (prot_array%preyN / prot_array%preyC) 
            prot_array%dPreyP    = prot_array%dPreyC * (prot_array%preyP / prot_array%preyC) 
            prot_array%dPreySi   = prot_array%dPreyC * (prot_array%preySi / prot_array%preyC)   
            
            ! Chlorophyll uptake ----------------------------------------------------  
            ! acquistion of prey chlorphyll gChl m-3 d-1
            dChlup = sum(prot_array%dPreyChl) * upChl(ChlC, ChlCm)
                          
            ! (1 + SpeciesLoop * (nr of fluxes per individual species + total prey fluxes) + total number of fluxes
            fl (   1 + iSpec * (23 + maxNrPr * 5) + iflux) = dCeat
            fl (   2 + iSpec * (23 + maxNrPr * 5) + iflux) = dNeat
            fl (   3 + iSpec * (23 + maxNrPr * 5) + iflux) = dPeat
            fl (   4 + iSpec * (23 + maxNrPr * 5) + iflux) = dCfix
            fl (   5 + iSpec * (23 + maxNrPr * 5) + iflux) = dChldeg
            fl (   6 + iSpec * (23 + maxNrPr * 5) + iflux) = dChlout
            fl (   7 + iSpec * (23 + maxNrPr * 5) + iflux) = dCresp
            fl (   8 + iSpec * (23 + maxNrPr * 5) + iflux) = dDOCleak
            fl (   9 + iSpec * (23 + maxNrPr * 5) + iflux) = dDOCvoid
            fl (  10 + iSpec * (23 + maxNrPr * 5) + iflux) = dPOCout
            fl (  11 + iSpec * (23 + maxNrPr * 5) + iflux) = dPONout
            fl (  12 + iSpec * (23 + maxNrPr * 5) + iflux) = dPOPout
            fl (  13 + iSpec * (23 + maxNrPr * 5) + iflux) = dNH4out
            fl (  14 + iSpec * (23 + maxNrPr * 5) + iflux) = dPout  
            fl (  15 + iSpec * (23 + maxNrPr * 5) + iflux) = dAutC
            fl (  16 + iSpec * (23 + maxNrPr * 5) + iflux) = dDetC
            fl (  17 + iSpec * (23 + maxNrPr * 5) + iflux) = dAutN
            fl (  18 + iSpec * (23 + maxNrPr * 5) + iflux) = dDetN
            fl (  19 + iSpec * (23 + maxNrPr * 5) + iflux) = dAutP
            fl (  20 + iSpec * (23 + maxNrPr * 5) + iflux) = dDetP
            fl (  21 + iSpec * (23 + maxNrPr * 5) + iflux) = dAutChl
            fl (  22 + iSpec * (23 + maxNrPr * 5) + iflux) = dDetChl            
            fl (  23 + iSpec * (23 + maxNrPr * 5) + iflux) = dChlup
                                    
            ! loop over prey ingestion fluxes
            do iPrey = 0, (maxNrPr - 1)                                
                ! (nr prey independent fluxes + prey Flux # + loop) + (move on to next predator) + total number of fluxes
                fl ( (23 + 1 + iPrey * 5) + iSpec * (23 + maxNrPr * 5) + iflux ) = prot_array%dPreyC(iPrey + 1)  
                fl ( (23 + 2 + iPrey * 5) + iSpec * (23 + maxNrPr * 5) + iflux ) = prot_array%dPreyChl(iPrey + 1)
                fl ( (23 + 3 + iPrey * 5) + iSpec * (23 + maxNrPr * 5) + iflux ) = prot_array%dPreyN(iPrey + 1)  
                fl ( (23 + 4 + iPrey * 5) + iSpec * (23 + maxNrPr * 5) + iflux ) = prot_array%dPreyP(iPrey + 1)  
                fl ( (23 + 5 + iPrey * 5) + iSpec * (23 + maxNrPr * 5) + iflux ) = prot_array%dPreySi(iPrey + 1) 
            end do 
            
            if ( isnan(protC) ) write (*,*) '(''ERROR: NaN in protC in segment:'', i10)' ,    iseg
            if ( isnan(Cfix) )  write (*,*) '(''ERROR: NaN in Cfix in segment:'', i10)' ,    iseg
            if ( isnan(totR) )  write (*,*) '(''ERROR: NaN in totR in segment:'', i10)' ,    iseg
            if ( isnan(mrt) )   write (*,*) '(''ERROR: NaN in mrt in segment:'', i10)' ,    iseg
            if ( isnan(NC) )    write (*,*) '(''ERROR: NaN in NC in segment:'', i10)' ,    iseg
            if ( isnan(PC) )    write (*,*) '(''ERROR: NaN in PC in segment:'', i10)' ,    iseg
            if ( isnan(ChlC) )  write (*,*) '(''ERROR: NaN in ChlC in segment:'', i10)' ,    iseg
            if ( isnan(ingC) )  write (*,*) '(''ERROR: NaN in ingC in segment:'', i10)' ,    iseg
                           
        enddo ! end loop over species 

        endif ! end if check for dry cell 

        !allocate pointers
        iflux = iflux + noflux
        ipnt = ipnt + increm

    enddo ! end loop over segments
    
    
    ! deallocation of prey input array
    call deallocate_prot_array(prot_array)
    return
end ! end subroutine 
