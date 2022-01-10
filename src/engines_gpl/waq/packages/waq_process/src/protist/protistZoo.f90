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
subroutine PROZOO     ( pmsa   , fl     , ipoint , increm, noseg , &                            
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &                            
                            noq3   , noq4   )     
!                                                                                                     
!*******************************************************************************                      
!  
use protist_math_functions
use protist_cell_functions
use protist_phagotrophy_functions
use protist_types
use protist_food_functions
use protist_constants
use ieee_arithmetic

    IMPLICIT NONE                                                                                   
!                                                                                                     
!     Type    Name         I/O Description                                                            
!          
    integer, parameter :: plen = 123 ! total length of the PMSA input and output array
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
     real       UmRT, Q10, RT, CR                   ! growth and respiration rate calculation      
     real       NCm, PCm                            ! maximum NC and PC quotas
     real       NCo, PCo                            ! minimum NC and PC quotas
     real       NCopt, PCopt                        ! optimal NC and PC quotas
     real       CcellZoo, rZoo                      ! parameters for protozooplankton cell
     real       optCR                               ! parameters for encounter
     real       kAE, AEm, AEo                       ! parameters for assimilation efficiency
     real       SDA                                 ! specific dynamic action
     real       MrtRT, FrAut, FrDet                 ! reference mortality and fractions

     ! INPUT STATE VARIABLES
     real    protC, protN, protP                    ! protist state variables
     real    Temp                                   ! physical abiotic variables
         
     ! AUXILIARIES
     real    NC, PC          ! nutrient quotas
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
            
     ! other parameters
     real, parameter :: wTurb = 0.0 ! this needs to be an input from model eventually!!!!

     ! loop counter 
     integer iPrey      ! counter for loops

     ! Fluxes
     real   dCeat, dNeat, dPeat                         ! assimilation fluxes
     real   dCresp                                      ! respiration flux
     real   dPOCout, dPONout, dPOPout                   ! voiding organic fluxes
     real   dNH4out, dPout                              ! voding inorganic fluxes
     real   dAutC, dAutN, dAutP                         ! autolysis fluxes
     real   dDetC, dDetN, dDetP                         ! detritus fluxes

     
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
    nrSp_par  = PMSA(ipnt(   7 ))   !   nr of parameters per species                           (dl)
    iSp       = PMSA(ipnt(   8 ))   !   selector of species parameter (needed to share code with for protistCM)   (dl)
    
    ! allocation of prey input array
    call allocate_prot_array(prot_array,maxNrPr)
    
    
    
    
    
    
    ! length of the PMSA input array.         
    inpItems = nrSpInd   + maxNrSp * nrSpCon + maxNrPr * nrSp_par
         
    ! segment loop
    segmentLoop: do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then    
            
            Temp      = PMSA(ipnt(   6 ))   !   temperature                                            (C)
      
        ! species loop
        speciesLoop: do iSpec = 0, (nrSp-1)

            spInc = nrSpCon * iSpec
               
            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( nrSpInd   +  1 + spInc ))   !      C-biomass                                              (gC m-3)  
            protN        = PMSA(ipnt( nrSpInd   +  2 + spInc ))   !      N-biomass                                              (gN m-3)   
            protP        = PMSA(ipnt( nrSpInd   +  3 + spInc ))   !      P-biomass                                              (gP m-3)   
            AEm          = PMSA(ipnt( nrSpInd   +  4 + spInc ))   !      maximum assimilation efficiency (AE)                   (dl)
            AEo          = PMSA(ipnt( nrSpInd   +  5 + spInc ))   !      minimum AE                                             (dl)
            CcellZoo     = PMSA(ipnt( nrSpInd   +  6 + spInc ))   !      C content of protist cell                              (pgC cell-1)
            CR           = PMSA(ipnt( nrSpInd   +  7 + spInc ))   !      catabolic respiration quotient                         (dl)
            FrAut        = PMSA(ipnt( nrSpInd   +  8 + spInc ))   !      fraction of mortality to autolysis                     (dl)   
            FrDet        = PMSA(ipnt( nrSpInd   +  9 + spInc ))   !      fraction of mortality to detritus                      (dl)   
            kAE          = PMSA(ipnt( nrSpInd   + 10 + spInc ))   !      Control of AE in response to prey quality              (dl)
            MrtRT        = PMSA(ipnt( nrSpInd   + 11 + spInc ))   !      mortality at reference temperature                     (dl)      
            NCm          = PMSA(ipnt( nrSpInd   + 12 + spInc ))   !      N:C that totally represses NH4 transport               (gN gC-1) 
            NCo          = PMSA(ipnt( nrSpInd   + 13 + spInc ))   !      minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( nrSpInd   + 14 + spInc ))   !      N:C for growth under optimal conditions                (gN gC-1)
            optCR        = PMSA(ipnt( nrSpInd   + 15 + spInc ))   !      proportion of prey captured by starved Zoo             (dl)       
            PCm          = PMSA(ipnt( nrSpInd   + 16 + spInc ))   !      PC maximum quota                                       (gP gC-1) 
            PCo          = PMSA(ipnt( nrSpInd   + 17 + spInc ))   !      PC minimum quota                                       (gP gC-1)
            PCopt        = PMSA(ipnt( nrSpInd   + 18 + spInc ))   !      PC optimum quota                                       (gP gC-1)
            Q10          = PMSA(ipnt( nrSpInd   + 19 + spInc ))   !      Q10 for UmRT                                           (dl)
            RT           = PMSA(ipnt( nrSpInd   + 20 + spInc ))   !      reference temperature for UmRT                         (deg C)
            rZoo         = PMSA(ipnt( nrSpInd   + 21 + spInc ))   !      radius of nutrient repleted protist cell               (um)
            SDA          = PMSA(ipnt( nrSpInd   + 22 + spInc ))   !      specific dynamic action                                (dl)
            UmRT         = PMSA(ipnt( nrSpInd   + 23 + spInc ))   !      maximum growth rate using NH4-N at reference T         (d-1) 
                        
            if (protC <= threshCmass) then 
                cycle speciesLoop
            end if

            ! Calculate the nutrient quota of the cell-------------------------------------------------------------------------------                            
            ! Units: gNut gC-1  
            NC   = quota(protN, protC)
            PC   = quota(protP, protC)
            
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
            mot = motility(rZoo)

            
            call initialize_prot_array(prot_array,maxNrPr, PMSA, plen, ipnt, nrSpInd, maxNrSp, nrSpCon, iSp, nrSp_par)

            
            ! for output [dl]
            preyFlag = sum(prot_array%preyFlag)


            !! FOOD QUANTITY -------------------------------------------------------------------------------    
            ! cell abundance of prey per m3
            ! Units: nr cells m-3 (1e12: transform between g (preyC) and pg (CcontentPrey))
            prot_array%nrPrey = prot_array%preyFlag * 1e12 * prot_array%preyC / prot_array%CcellPrey
            
            call protistFoodQuantity(prot_array, rZoo, wTurb, CcellZoo, optCR, mot, sumCP, ingNC, ingPC)


            
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
                        
            ! Calculate respiration   --------------------------------------- 
            ! Units: gC gC-1 d-1             
            ! protzoo cannot recover loss N
            if (protC >= 1.0E-5) then 
                totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)     
            else
                totR = 0.0
            end if  
            !totR = totalRespiration(0.0, 0.0, 0.0, assC, assN, SDA, BR)    
            Cu   = CgrowthRate(0.0, assC, totR)
    
            ! Calculate mortality  --------------------------------------- 
            call protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut, mrtFrDet)
            
            
                   
            ! Output -------------------------------------------------------------------
               
            ! (input items + position of specific output item in vector + species loop * total number of output) 
            PMSA(ipnt( inpItems +   1 + iSpec * 29 )) = NC
            PMSA(ipnt( inpItems +   2 + iSpec * 29 )) = PC
            PMSA(ipnt( inpItems +   3 + iSpec * 29 )) = UmT
            PMSA(ipnt( inpItems +   4 + iSpec * 29 )) = BR 
            PMSA(ipnt( inpItems +   5 + iSpec * 29 )) = NCu
            PMSA(ipnt( inpItems +   6 + iSpec * 29 )) = PCu
            PMSA(ipnt( inpItems +   7 + iSpec * 29 )) = NPCu
            PMSA(ipnt( inpItems +   8 + iSpec * 29 )) = mot
            PMSA(ipnt( inpItems +   9 + iSpec * 29 )) = sumCP
            PMSA(ipnt( inpItems +  10 + iSpec * 29 )) = ingNC
            PMSA(ipnt( inpItems +  11 + iSpec * 29 )) = ingPC
            PMSA(ipnt( inpItems +  12 + iSpec * 29 )) = ppNC
            PMSA(ipnt( inpItems +  13 + iSpec * 29 )) = ppPC
            PMSA(ipnt( inpItems +  14 + iSpec * 29 )) = stoichP
            PMSA(ipnt( inpItems +  15 + iSpec * 29 )) = opAE
            PMSA(ipnt( inpItems +  16 + iSpec * 29 )) = maxIng
            PMSA(ipnt( inpItems +  17 + iSpec * 29 )) = ingSat
            PMSA(ipnt( inpItems +  18 + iSpec * 29 )) = ingC  
            PMSA(ipnt( inpItems +  19 + iSpec * 29 )) = assC  
            PMSA(ipnt( inpItems +  20 + iSpec * 29 )) = ingN
            PMSA(ipnt( inpItems +  21 + iSpec * 29 )) = ingP
            PMSA(ipnt( inpItems +  22 + iSpec * 29 )) = assN
            PMSA(ipnt( inpItems +  23 + iSpec * 29 )) = assP
            PMSA(ipnt( inpItems +  24 + iSpec * 29 )) = totR
            PMSA(ipnt( inpItems +  25 + iSpec * 29 )) = Cu
            PMSA(ipnt( inpItems +  26 + iSpec * 29 )) = mrt
            PMSA(ipnt( inpItems +  27 + iSpec * 29 )) = mrtFrAut
            PMSA(ipnt( inpItems +  28 + iSpec * 29 )) = mrtFrDet
            PMSA(ipnt( inpItems +  29 + iSpec * 29 )) = preyFlag
            
            ! FLUXES -------------------------------------------------------------------   
            ! Protist gains------------------------------------------------------------                                 
            ! Protist growth through assimilation -----------------------------------------------------
            ! gX m-3 d-1 assimilation of X from prey
            dCeat = protC * assC
            dNeat = protC * assN    
            dPeat = protC * assP
            
            ! Protist losses-----------------------------------------------------------
            ! gC m-3 d-1   total respiration rate   
            dCresp = protC * totR   
            
            ! gX m-3 d-1  rate of voiding of X as particulates
            dPOCout = protC * (ingC - assC)
            dPONout = protC * (ingN - assN) 
            dPOPout = protC * (ingP - assP) 
                        
            ! gNut m-3 d-1 voiding of nutrient P and N if interanl maximum is reached
            dNH4out = voiding(protN, protC, NCopt)
            dPout   = voiding(protP, protC, PCopt)
                                    
            ! gNut m-3 d-1 mortality
            dAutC = protC **2 * mrtFrAut
            dDetC = protC **2 * mrtFrDet    
            dAutN = protN **2 * mrtFrAut
            dDetN = protN **2 * mrtFrDet            
            dAutP = protP **2 * mrtFrAut
            dDetP = protP **2 * mrtFrDet            
            
              
            ! (1 + SpeciesLoop * (nr of fluxes per individual species + total prey fluxes) + total number of fluxes
            fl (   1 + iSpec * (15 + maxNrPr * 5) + iflux) = dCeat
            fl (   2 + iSpec * (15 + maxNrPr * 5) + iflux) = dNeat
            fl (   3 + iSpec * (15 + maxNrPr * 5) + iflux) = dPeat
            fl (   4 + iSpec * (15 + maxNrPr * 5) + iflux) = dCresp
            fl (   5 + iSpec * (15 + maxNrPr * 5) + iflux) = dPOCout
            fl (   6 + iSpec * (15 + maxNrPr * 5) + iflux) = dPONout
            fl (   7 + iSpec * (15 + maxNrPr * 5) + iflux) = dPOPout
            fl (   8 + iSpec * (15 + maxNrPr * 5) + iflux) = dNH4out
            fl (   9 + iSpec * (15 + maxNrPr * 5) + iflux) = dPout  
            fl (  10 + iSpec * (15 + maxNrPr * 5) + iflux) = dAutC
            fl (  11 + iSpec * (15 + maxNrPr * 5) + iflux) = dDetC
            fl (  12 + iSpec * (15 + maxNrPr * 5) + iflux) = dAutN
            fl (  13 + iSpec * (15 + maxNrPr * 5) + iflux) = dDetN
            fl (  14 + iSpec * (15 + maxNrPr * 5) + iflux) = dAutP
            fl (  15 + iSpec * (15 + maxNrPr * 5) + iflux) = dDetP
            
            if ( ieee_is_nan(protC) ) write (*,*) '(''ERROR: NaN in protC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(ingC) )  write (*,*) '(''ERROR: NaN in ingC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(assC) )  write (*,*) '(''ERROR: NaN in assC in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(totR) )  write (*,*) '(''ERROR: NaN in totR in segment:'', i10)' ,    iseg
            if ( ieee_is_nan(mrt) )   write (*,*) '(''ERROR: NaN in mrt in segment:'', i10)' ,    iseg
            
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
                fl ( (15 + 1 + iPrey * 5) + (15 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyC(iPrey + 1)  
                fl ( (15 + 2 + iPrey * 5) + (15 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyChl(iPrey + 1)
                fl ( (15 + 3 + iPrey * 5) + (15 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyN(iPrey + 1)  
                fl ( (15 + 4 + iPrey * 5) + (15 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreyP(iPrey + 1)  
                fl ( (15 + 5 + iPrey * 5) + (15 + maxNrPr * 5) * iSpec + iflux ) = prot_array%dPreySi(iPrey + 1) 
            end do 
            
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

  