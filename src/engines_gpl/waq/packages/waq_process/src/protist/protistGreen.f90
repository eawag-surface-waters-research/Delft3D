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
subroutine PROGRE     ( pmsa   , fl     , ipoint , increm, noseg , &                            
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &                            
                            noq3   , noq4   )     
! dont touch next line, replace name though
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'PROGRE' :: PROGRE                                     
!                                                                                                     
!*******************************************************************************                      
!  
use protist_math_functions
use protist_cell_functions
use protist_uptake_functions
use protist_photosynthesis_functions

    IMPLICIT NONE                                                                                   
!                                                                                                     
!     Type    Name         I/O Description                                                            
!          
    integer, parameter :: plen = 121 ! total length of the PMSA input and output array
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
    
     !input parameters
     real    maxNrSp, nrSp, nrSpCon, nrInd               ! constant and species numbers       
     real    UmRT, Q10, RT, CR                           ! growth and respiration rate calculation 
     real    NCm, NO3Cm, PCm, ChlCm                      ! maximum NC, PC, ChlC quotas
     real    NCo, PCo, ChlCo                             ! minimum NC and PC quotas
     real    NCopt, NO3Copt, PCopt                       ! optimal NC and PC quotas
     real    KtP, KtNH4, KtNO3                           ! half saturation constants
     real    PCoNCopt, PCoNCm                            ! P status influence on optimum NC
     real    ReUmNH4, ReUmNO3, redco, PSDOC, relPS       ! relative growth rates with specific nutrients  
     real    MrtRT, FrAut, FrDet                         ! reference mortality and fractions
     real    rProt                                       ! radius of cell
     real    alpha                                       ! inital slope
 
     ! input state variables
     real    protC, protChl, protN, protP                ! protist state variables
     real    PO4, NH4, NO3                               ! nutrient state variables
     real    Temp                                        ! physical abiotic variables      
     real    PFD, atten, exat                            ! available light and extinction

         
     ! auxiliaries
     real    NC, PC, ChlC                                ! cell nutrient quotas
     real    UmT, BR                                     ! growth and repsiration rates
     real    NCu, PCu, NPCu                              ! nutrient status within the cell
     real    mot                                         ! motility 
     real    upP, upNH4, upNO3                           ! nutrient uptake
     real    PSqm, Cfix, synChl, degChl                  ! plateau and Cifx through photosynthesis
     real    maxPSreq, PS                                ! req for C to come from PS (==1 for diatoms)
     real    totR, Cu                                    ! respiration and C-growth
     real    mrt, mrtFrAut, mrtFrDet                     ! mortality to detritus and autolysis
     
     ! additional protection parameters
     real, parameter :: threshVal = 1.0E-10              ! protection against too small nutrient values     

     ! Fluxes
     real    dNH4up, dNO3up, dPup                        ! uptake fluxes
     real    dCfix                                       ! photosynthesis flux
     real    dChlsyn, dChldeg                            ! Chl synthesis  and degradation flux
     real    dCresp                                      ! respiration flux
     real    dDOCleak                                    ! C leak through photosynthesis 
     real    dDOCvoid, dNH4out, dPout                    ! voiding fluxes
     real    dAutC, dAutN, dAutP, dAutChl                ! autolysis fluxes                          
     real    dDetC, dDetN, dDetP, dDetChl                ! voiding fluxes

!                                                                                                     
!******************************************************************************* 
!                                                                                                     
    ipnt        = ipoint
           
    iflux = 0
    
    ! segment and species independent items
    maxNrSp   = PMSA(ipnt(   1 ))   !   total nr species implemented in process                (dl)
    nrSp      = PMSA(ipnt(   2 ))   !   nr of species to be modelled                           (dl)                
    nrSpCon   = PMSA(ipnt(   3 ))   !   nr of species dependent items                          (dl)                
    nrInd     = PMSA(ipnt(   4 ))   !   nr of species independent items                        (dl)  
      
    ! length of the PMSA input array. 
    inpItems = maxNrSp * nrSpCon + nrInd
   
    ! segment loop
    do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then
            
        ! species independent items
        PO4          = PMSA(ipnt(   5 ))  !    initial external DIP                                   (gP m-3)
        NH4          = PMSA(ipnt(   6 ))  !    initial external NH4                                   (gN m-3)
        NO3          = PMSA(ipnt(   7 ))  !    initial external NO3                                   (gN m-3)
        Temp         = PMSA(ipnt(   8 ))  !    ambient water temperature                              (oC)               
        PFD          = PMSA(ipnt(   9 ))  !    from rad to photon flux density                        (umol photon m-2)           
        atten        = PMSA(ipnt(  10 ))  !    attenuation of light by water + plankton Chl           (dl)                            
        exat         = PMSA(ipnt(  11 ))  !    -ve exponent of attenuation                            (dl)              
      
        ! species loop
        do iSpec = 0, (nrSp-1)

            spInc = nrSpCon * iSpec
               
            ! species dependent items
            ! (number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( nrInd +  1 + spInc ))   !      C-biomass                                              (gC m-3)  
            protChl      = PMSA(ipnt( nrInd +  2 + spInc ))   !      Chl-biomass                                            (gChl m-3)   
            protN        = PMSA(ipnt( nrInd +  3 + spInc ))   !      N-biomass                                              (gN m-3)   
            protP        = PMSA(ipnt( nrInd +  4 + spInc ))   !      P-biomass                                              (gP m-3)   
            alpha        = PMSA(ipnt( nrInd +  5 + spInc ))   !      alpha for photosynthesis in protist                    (Figure this out!)
            ChlCm        = PMSA(ipnt( nrInd +  6 + spInc ))   !      maximum cellular Chl:C ratio                           (gChl gC-1)
            ChlCo        = PMSA(ipnt( nrInd +  7 + spInc ))   !      minimum cellular Chl:C ratio                           (gChl gC-1)
            CR           = PMSA(ipnt( nrInd +  8 + spInc ))   !      catabolic respiration quotient                         (dl)
            FrAut        = PMSA(ipnt( nrInd +  9 + spInc ))   !      fraction of mortality to autolysis                     (dl)
            FrDet        = PMSA(ipnt( nrInd + 10 + spInc ))   !      fraction of mortality to detritus                      (dl)
            KtNH4        = PMSA(ipnt( nrInd + 11 + spInc ))   !      Kt for NH4 transport                                   (gN m-3)
            KtNO3        = PMSA(ipnt( nrInd + 12 + spInc ))   !      Kt for NO3 transport                                   (gN m-3) 
            KtP          = PMSA(ipnt( nrInd + 13 + spInc ))   !      Kt for DIP transport                                   (gP m-3) 
            MrtRT        = PMSA(ipnt( nrInd + 14 + spInc ))   !      mortality at reference temperature                     (dl)     
            NCm          = PMSA(ipnt( nrInd + 15 + spInc ))   !      N:C that totally represses NH4 transport               (gN gC-1) 
            NCo          = PMSA(ipnt( nrInd + 16 + spInc ))   !      minimum N-quota                                        (gN gC-1) 
            NCopt        = PMSA(ipnt( nrInd + 17 + spInc ))   !      N:C for growth under optimal conditions                (gN gC-1) 
            NO3Cm        = PMSA(ipnt( nrInd + 18 + spInc ))   !      N:C that totally represses NO3 transport               (gN gC-1)
            NO3Copt      = PMSA(ipnt( nrInd + 19 + spInc ))   !      N:C for growth on NO3 under optimal conditions         (gN gC-1) 
            PCm          = PMSA(ipnt( nrInd + 20 + spInc ))   !      PC maximum quota                                       (gP gC-1) 
            PCo          = PMSA(ipnt( nrInd + 21 + spInc ))   !      PC minimum quota                                       (gP gC-1) 
            PCoNCm       = PMSA(ipnt( nrInd + 22 + spInc ))   !      maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCoNCopt     = PMSA(ipnt( nrInd + 23 + spInc ))   !      optimum NC when PC is minimum (PCu = 0)                (gN gC-1) 
            PCopt        = PMSA(ipnt( nrInd + 24 + spInc ))   !      PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( nrInd + 25 + spInc ))   !      proportion of current PS being leaked as DOC           (dl)
            Q10          = PMSA(ipnt( nrInd + 26 + spInc ))   !      Q10 for UmRT                                           (dl) 
            redco        = PMSA(ipnt( nrInd + 27 + spInc ))   !      C respired to support nitrate reduction for NH4        (gC gN-1) 
            relPS        = PMSA(ipnt( nrInd + 28 + spInc ))   !      relative PSmax:Umax on phototrophy                     (dl)
            ReUmNH4      = PMSA(ipnt( nrInd + 29 + spInc ))   !      max. growth rate supported by NH4-N:Umax               (dl) 
            ReUmNO3      = PMSA(ipnt( nrInd + 30 + spInc ))   !      max. growth rate supported by NO3-N:Umax               (dl)       
            RT           = PMSA(ipnt( nrInd + 31 + spInc ))   !      reference temperature for UmRT                         (deg C)   
            rProt        = PMSA(ipnt( nrInd + 32 + spInc ))   !      radius of nutrient repleted protist cell               (um)   
            UmRT         = PMSA(ipnt( nrInd + 33 + spInc ))   !      maximum growth rate at reference T                     (d-1) 
                        
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
            mot =  motility(rProt) ! 1.00E-12 ! 
            !mot = (1.0 - NPCu) * motility(rProt) + 1.00E-12

            ! Calculate uptake for the nutrients --------------------------------------- 
            ! Units: gNut gC-1 d-1   
            upP = uptakeP(PC, PCo, PCopt, PCm, UmT, PO4, KtP)
            upNH4 = uptakeNH4(PCoNCopt, PCoNCm, PCu, NCu, NC, NCo, NCopt, NCm, UmT, ReUmNH4, NH4, KtNH4)             
            upNO3 = uptakeNO3(PCoNCm, PCu, NC, NC, NCo, NO3Copt, NO3Cm, UmT, ReUmNO3, NO3, KtNO3)   
                   
            ! Calculate photosynthesis related equation --------------------------------------- 
            ! Units: gC gC-1 d-1
            maxPSreq = 1.0 ! need to cover all C through photosynthesis
            PSqm = plateauPS(UmT, maxPSreq, relPS, NCopt, redco, NPCu, BR, PSDOC)
            PS   = grossPS(ChlC, PFD, exat, atten, PSqm, alpha)
            Cfix = netPS(PS, PSDOC)
                        
            ! Calculate chlorophyll synthesis and degradation --------------------------------------- 
            ! Units: gChl gC-1 d-1          
            synChl = synthesisChl(ChlC, ChlCo, ChlCm, UmT, maxPSreq, NPCu, Cfix, PSqm)
            degChl = degradeChl(ChlC, ChlCm, UmT, NPCu)
                        
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
            PMSA(ipnt( inpItems +  1 + iSpec * 22 )) = NC 
            PMSA(ipnt( inpItems +  2 + iSpec * 22 )) = PC 
            PMSA(ipnt( inpItems +  3 + iSpec * 22 )) = ChlC 
            PMSA(ipnt( inpItems +  4 + iSpec * 22 )) = UmT 
            PMSA(ipnt( inpItems +  5 + iSpec * 22 )) = BR
            PMSA(ipnt( inpItems +  6 + iSpec * 22 )) = NCu 
            PMSA(ipnt( inpItems +  7 + iSpec * 22 )) = PCu 
            PMSA(ipnt( inpItems +  8 + iSpec * 22 )) = NPCu
            PMSA(ipnt( inpItems +  9 + iSpec * 22 )) = mot
            PMSA(ipnt( inpItems + 10 + iSpec * 22 )) = upP 
            PMSA(ipnt( inpItems + 11 + iSpec * 22 )) = upNH4 
            PMSA(ipnt( inpItems + 12 + iSpec * 22 )) = upNO3 
            PMSA(ipnt( inpItems + 13 + iSpec * 22 )) = PSqm 
            PMSA(ipnt( inpItems + 14 + iSpec * 22 )) = PS
            PMSA(ipnt( inpItems + 15 + iSpec * 22 )) = Cfix 
            PMSA(ipnt( inpItems + 16 + iSpec * 22 )) = synChl
            PMSA(ipnt( inpItems + 17 + iSpec * 22 )) = degChl
            PMSA(ipnt( inpItems + 18 + iSpec * 22 )) = totR 
            PMSA(ipnt( inpItems + 19 + iSpec * 22 )) = Cu
            PMSA(ipnt( inpItems + 20 + iSpec * 22 )) = mrt 
            PMSA(ipnt( inpItems + 21 + iSpec * 22 )) = mrtFrAut 
            PMSA(ipnt( inpItems + 22 + iSpec * 22 )) = mrtFrDet


            ! FLUXES -------------------------------------------------------------------   
            ! Protist gains------------------------------------------------------------                                 
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
            fl (  1 +  iSpec * 19  + iflux )  = dNH4up
            fl (  2 +  iSpec * 19  + iflux )  = dNO3up
            fl (  3 +  iSpec * 19  + iflux )  = dPup  
            fl (  4 +  iSpec * 19  + iflux )  = dCfix
            fl (  5 +  iSpec * 19  + iflux )  = dChlsyn
            fl (  6 +  iSpec * 19  + iflux )  = dChldeg
            fl (  7 +  iSpec * 19  + iflux )  = dCresp
            fl (  8 +  iSpec * 19  + iflux )  = dDOCleak
            fl (  9 +  iSpec * 19  + iflux )  = dDOCvoid
            fl ( 10 +  iSpec * 19  + iflux )  = dNH4out
            fl ( 11 +  iSpec * 19  + iflux )  = dPout     
            fl ( 12 +  iSpec * 19  + iflux )  = dAutC     
            fl ( 13 +  iSpec * 19  + iflux )  = dDetC    
            fl ( 14 +  iSpec * 19  + iflux )  = dAutN    
            fl ( 15 +  iSpec * 19  + iflux )  = dDetN    
            fl ( 16 +  iSpec * 19  + iflux )  = dAutP    
            fl ( 17 +  iSpec * 19  + iflux )  = dDetP    
            fl ( 18 +  iSpec * 19  + iflux )  = dAutChl  
            fl ( 19 +  iSpec * 19  + iflux )  = dDetChl  
            
            if ( isnan(protC) ) write (*,*) '(''ERROR: NaN in protC in segment:'', i10)' ,    iseg
            if ( isnan(Cfix) )  write (*,*) '(''ERROR: NaN in Cfix in segment:'', i10)' ,    iseg
            if ( isnan(totR) )  write (*,*) '(''ERROR: NaN in totR in segment:'', i10)' ,    iseg
            if ( isnan(mrt) )   write (*,*) '(''ERROR: NaN in mrt in segment:'', i10)' ,    iseg
            if ( isnan(NC) )    write (*,*) '(''ERROR: NaN in NC in segment:'', i10)' ,    iseg
            if ( isnan(PC) )    write (*,*) '(''ERROR: NaN in PC in segment:'', i10)' ,    iseg
            if ( isnan(ChlC) )  write (*,*) '(''ERROR: NaN in ChlC in segment:'', i10)' ,    iseg

                        
               
        enddo ! end loop over species 

        endif ! end if check for dry cell 

        !allocate pointers
        iflux = iflux + noflux
        ipnt = ipnt + increm

    enddo ! end loop over segments
    return
end ! end subroutine 
