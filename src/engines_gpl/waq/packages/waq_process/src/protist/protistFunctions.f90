!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

!!  *********************************************************************
!!  *    Modules containing all the functions for PROTIST                *
!!  *********************************************************************

!!   contains the following functions:
!!    - protist_math_functions            
!!              contains all the mathematical functions needed to run the             
!!              other modules e.g. normalize, sigmoidLogistic
!!    - protist_cell_functions
!!              contains all the functions needed to determine the status
!!              and maintanence of the cell, e.g. cell quota, nutrient status,
!!              respiration, growth and mortality    
!!    - protist_uptake_functions
!!              contains all the functions needed for uptake of nutrients
!!    - protist_photosynthesis_functions
!!              contains all the functions needed to calculated photosynthesis
!!              and chlorophyll synthesis!!    
!!    
    
module protist_math_functions  
    use protist_types
    contains
    
   ! normalize between two values
   real function normalize(x, lowerVal, upperVal) result(normX)
      real x, lowerVal, upperVal
      normX = (x - lowerVal) / (upperVal - lowerVal)   
   end function normalize 
   
   ! Gompertz sigmoidal function
   real function sigmoidGompertz(L, b, k, x) result(y)
      real L  ! upper asymptote      
      real b  ! displacement along the x-axis
      real k  ! growth rate of curve
      real x
      y = L * exp(-b * exp(-k * x))
   end function sigmoidGompertz
   
   ! monod function 
   real function monod(resource, halfSat) result(y)
      real resource, halfSat
      y = resource / (resource + halfSat)
   end function monod  
   
   ! Logistic sigmoidal function
   real function sigmoidLogistic(L, k, b, x) result(y)
      real L                  ! upper asymptote
      real k                  ! growth rate curve     
      real b                  ! displacement along the x-axis
      real x
      y = L / (1.0 + exp(-k * (x - b)))
   end function sigmoidLogistic
    
end module protist_math_functions
    
    
module protist_cell_functions
use protist_math_functions
    contains
    
    ! calculate the internal nutrient quotas
    ! Units: gNut gC-1 
    real function quota(protNut, protC) result(NutC)
       real protNut, protC
       NutC = protNut / protC
    end function quota    
   
   ! calculate Q10 rate, i.e. rate at current temperature using the Q10 approach          
   ! Units: gC gC-1 d-1   
   real function Q10rate(referenceRate, Q10, Temp, referenceTemp) result(rate)
      real  referenceRate, Q10, Temp, referenceTemp
      rate = referenceRate * Q10**((Temp - referenceTemp) / 10.0)      
   end function Q10rate
   
   ! basal respiration rate
   ! Units: gC gC-1 d-1     
   real function basal_respiration(maxUmT, CR) result(BR)
      real  maxUmT
      real  CR      ! catabolic respiration quotient    (dl)
      BR = maxUmT * CR
   end function basal_respiration
   
   ! calculates voiding of nutrients if the maximum/minimum is reached
   ! Units: gNut m-3 d-1   
   real function voiding(protNut, protC, maxNutC) result(NutRegen)
      real protNut, protC, maxNutC
      NutRegen = max(0.0, protNut - protC * maxNutC)
   end function voiding
   
   ! Nitrogen to Carbon nutrient status of protist
   ! Units: gN gC-1
   real function statusNC(NutC, NutCmin, NutCopt) result(NutStat)
      real NutC, NutCmin, NutCopt   
      real nNutC, NutStat_lowBound
      nNutC = normalize(NutC, NutCmin, NutCopt) 
      ! makes lower Bound
      NutStat_lowBound = max(0.0, nNutC)
      ! makes upper Bound
      NutStat = min(1.0, NutStat_lowBound)
      !if (nNutC > 0.0) then 
      !    if (nNutC < 1.0) then
      !        NutStat = nNutC
      !    else 
      !        NutStat = 1.0
      !    end if 
      !else 
      !    NutStat = 0.0
      !end if       
   end function statusNC
   
   ! Phosphate to Carbon nutrient status of protist
   ! Units: gP gC-1
   real function statusPC(NutC, NutCmin, NutCmax) result(NutStat)
      real NutC, NutCmin, NutCmax   
      real, parameter :: L = 1.0   ! upper asymptote      
      real, parameter :: b = 6.0   ! displacement along the x-axis
      real, parameter :: k = 10.0  ! growth rate to form the curve
      real normX
      normX = normalize(NutC, NutCmin, NutCmax)
      NutStat = sigmoidGompertz(L, b, k, normX)
   end function statusPC 
   
   !! Silica to Carbon nutrient status of protist
   !! Units: gSi gC-1
   !real function statusSC(NutCopt, NutCmin, resource, halfSat) result(NutStat)
   !   real NutCopt, NutCmin, resource, halfSat
   !   real, parameter :: factor = 4.0 ! factor to form the curve
   !   real halfSat_new
   !   halfSat_new = halfSat / ((NutCopt / NutCmin) * factor)
   !   NutStat = monod(resource, halfSat_new) 
   !end function statusSC
   
   ! Silica to Carbon nutrient status of protist
   ! Units: gSi gC-1
   real function statusSC(NutCopt, NutCmin, resource, halfSat) result(NutStat)
      real NutCopt, NutCmin, resource, halfSat      
      NutStat = min(((resource / (resource + halfSat)) * (NutCopt / NutCmin)), 1.0)
   end function statusSC
      
   ! calculate total respiration
   ! should I put anaResp and SDA as input parameters or are they unchangable, physilogical constants
   ! the same for all species ??? (on average of course!)
   ! Units: gC gC-1 d-1
   real function totalRespiration(redco, upNO3, upNH4, assC, assN, propLostIngC, BR) result(totR)
      real upNO3, upNH4    ! uptake of NH4 and NO3
      real assC, assN      ! assimilation of prey C
      real propLostIngC    ! SDA GIVE EXPLANATION
      real BR              ! basal respiration
      real redco           ! C respired to support nitrate reduction for NH4        (gC gN-1)
      real, parameter :: anaResp = 1.5       ! anabolic respiration cost in terms of C                (gC gN-1 d-1)      
      totR = (redco * upNO3) + anaResp * (upNH4 + upNO3 + assN * propLostIngC) + (assC * propLostIngC) + BR      
   end function totalRespiration
   
   ! calculate instantaneous C-specific growth rate 
   ! Units: gC gC-1 d-1
   real function CgrowthRate(Cfix, assC, totR) result(Cu)
      real Cfix, assC, totR
      Cu = Cfix + assC - totR
   end function CgrowthRate
   
   ! calculate mortality
   ! Units: gC gC-1 d-1
   real function mortality(mortQ10, frac) result(mortFrac)
      real mortQ10
      real frac   ! fraction towards detritus/autolysis
      mortFrac = mortQ10 * frac
   end function mortality
   
   ! calculates motility
   ! Citation: Flynn, K. J. and Mitra, A. (2016). Why Plankton Modelers Should Reconsider Using Rectangular Hyperbolic 
   ! (Michaelis-Menten, Monod) Descriptions of Predator-Prey Interactions. Frontiers in Marine Science
   ! valid for organisms in the size range of 1.2 - 1900 um
   ! Units: m/s
   real function motility(radiusProtist) result(velProt)
      real radiusProtist                   ! radius of the protist cell  in um
      real ESD                             ! equivalend sperical diameter
      real, parameter :: a = 38.542        ! constant
      real, parameter :: k = 0.5424        ! exponent
      ! Unit: um
      ESD = (radiusProtist * 2)
      ! Units: m/s
      velProt = 1e-6 * (a * (ESD)**k) + 1e-12
   end function motility
   
end module protist_cell_functions
    
    
module protist_uptake_functions
use protist_math_functions
    contains
    
   ! function to calculate the influence of p status on uptake of NH4 and NO3
   ! NutC4Pval varies bettwen PinteractionValue and NutCvaluedepending on the status of PC
   ! Units: gN gC-1 
   real function interactionP(PinteractionValue, PCstatus, NCstatus, NutCvalue) result(NutC4Pval)
      real PinteractionValue   ! either optimum or maximum PC value for either opt or max NC value
      real PCstatus, NCstatus  ! nutrient status of the cell
      real NutCvalue           ! either optimum or maximum NC value
      
      ! only if P status worse then N status then NutCvalue changed
      if (PCstatus < NCstatus) then 
          NutC4Pval =  PinteractionValue + PCstatus * (NutCvalue - PinteractionValue)
      else 
          NutC4Pval = NutCvalue
      end if 
   end function interactionP
   
   ! uptake phosphate
   ! Units: gP gC-1 d-1
   real function uptakeP(NutC, NutCmin, NutCopt, NutCmax, maxUmT, resource, halfSat) result(up)
      real NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
      real maxUmT                  ! species specific maximum growth rate
      real resource                ! nutrient
      real halfSat                 ! species specific monod half saturation constant
      ! curve forming parameters for increase sigmoid
      real, parameter :: L_in = 1.0                  ! upper asymptote
      real, parameter :: k_in = -16.0                ! growth rate of the curve     
      real, parameter :: b_in = 0.7                  ! displacement along the x-axis
      real, parameter :: yFactor = 10.0              ! multiplies the plateau of the curve
      ! curve forming parameters for decrease sigmoid
      real, parameter :: L_de = 1.0                  ! upper asymptote
      real, parameter :: k_de = -40.0                ! growth rate of the curve     
      real, parameter :: b_de = 0.9                  ! displacement along the x-axis
      ! function auxiliaries
      real nNutC_below, nNutC_above     ! normalized NutC ratio between minVal and optVal/max
      real APincrease,  APdecrease      ! acquistion potential as a sigmoidal curve [0,1]
      real optUptake                    ! uptake at optimal nutrient quota
      real resource_check               ! check that resource is not negative
      
      ! increase of acquisition potential when NutC < NutCopt
      nNutC_below = normalize(NutC, NutCmin, NutCopt)
      APincrease = sigmoidLogistic(L_in, k_in, b_in, nNutC_below)
 
      ! decrease of acquisition potential when NutC > NutCopt    
      nNutC_above = normalize(NutC, NutCmin, NutCmax)
      APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutC_above)
      
      ! check that resource if not negative
      resource_check = max(0.0, resource)
      
      ! species specific uptake at optimum nutrient ratio
      optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt      
      ! uptake at current nutrient ratio
      up = optUptake * APincrease * yFactor + optUptake * APdecrease
   end function uptakeP
   
   ! uptake NH4
   ! Units: gN gC-1 d-1
   real function uptakeNH4(minPC4optNC, minPC4maxNC, PCstatus, NCstatus, NutC, NutCmin, NutCopt, NutCmax, maxUmT, relGrowthRate, resource, halfSat) result(up)
      real NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
      real maxUmT                           ! species specific maximum growth rate
      real resource                         ! nutrient
      real halfSat                          ! species specific monod half saturation constant     
      real PCstatus, NCstatus               ! cellular nutrient status 
      real minPC4optNC, minPC4maxNC         ! minimum PC status for optimal/maximum NC status
      real relGrowthRate                    ! relative max growth rate on that nutrient
      ! curve forming parameters for increase sigmoid
      real, parameter :: L_in = 1.0                 ! upper asymptote
      real, parameter :: k_in = -24.0               ! growth rate of the curve   
      real, parameter :: b_in = 0.85                ! displacement along the x-axis
      real, parameter :: yFactor = 3.0           ! multiplies the plateau of the curve      
      ! curve forming parameters for decrease sigmoid      
      real, parameter :: L_de = 1.0                 ! upper asymptote
      real, parameter :: k_de = -40.0               ! growth rate of the curve   
      real, parameter :: b_de = 0.85                ! displacement along the x-axis
      real NutCPopt, NutCPmax       ! N interaction with P status
      real nNutCopt, nNutCmax       ! normailzed quotas with P interaction
      real APincrease, APdecrease   ! acquistion potential as a sigmoidal curve [0,1]
      real optUptake                ! uptake at optimal nutrient quota
      real resource_check               ! check that resource is not negative
      
      ! increase of acquisition potential when NutC < NutCopt
      NutCPopt = interactionP(minPC4optNC, PCstatus, NCstatus, NutCopt)
      nNutCopt = normalize(NutC, NutCmin, NutCPopt)  
      APincrease = sigmoidLogistic(L_in, k_in, b_in, nNutCopt)
            
      ! decrease of acquisition potential when NutC > NutCopt
      NutCPmax = interactionP(minPC4maxNC, PCstatus, NCstatus, NutCmax)
      nNutCmax = normalize(NutC, NutCmin, NutCPmax)  
      APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutCmax)
      
      ! check that resource if not negative
      resource_check = max(0.0, resource)
      
      ! species specific uptake at optimum nutrient ratio
      optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt * relGrowthRate      
      ! uptake at current nutrient ratio
      up = optUptake * APincrease * yFactor + optUptake * APdecrease
   end function uptakeNH4
   
   ! uptake NO3
   ! Units: gN gC-1 d-1
   real function uptakeNO3(minPC4maxNC, PCstatus, NCstatus, NutC, NutCmin, NutCopt, NutCmax, maxUmT, relGrowthRate, resource, halfSat) result(up)
      real NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
      real maxUmT                           ! species specific maximum growth rate
      real resource                         ! nutrient
      real halfSat                          ! species specific monod half saturation constant     
      real PCstatus, NCstatus               ! cellular nutrient status 
      real minPC4maxNC                      ! minimum PC status for optimal/maximum NC status
      real relGrowthRate                    ! relative max growth rate on that nutrient      
      ! curve forming parameters for decrease sigmoid      
      real, parameter :: L_de = 1.0                 ! upper asymptote
      real, parameter :: k_de = -55.0               ! growth rate to form the curve 
      real, parameter :: b_de = 0.9                 ! displacement along the x-axis
      real NutCPmax       ! N interaction with P status
      real nNutCmax       ! normailzed quotas with P interaction
      real APdecrease     ! acquistion potential as a sigmoidal curve [0,1]
      real optUptake      ! uptake at optimal nutrient quota
      real resource_check               ! check that resource is not negative
        
      ! decrease of acquisition potential when NutC = NutCopt
      NutCPmax = interactionP(minPC4maxNC, PCstatus, NCstatus, NutCmax)
      nNutCmax = normalize(NutC, NutCmin, NutCPmax)
      APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutCmax)
      
      ! check that resource if not negative
      resource_check = max(0.0, resource)
      
      ! species specific uptake at optimum nutrient ratio
      optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt * relGrowthRate      
      ! uptake at current nutrient ratio
      up = optUptake * APdecrease      
   end function uptakeNO3
   
   ! uptake silica
   ! Units: gSi gC-1 d-1
   real function uptakeSi(NutC, NutCmin, NutCopt, NutCmax, maxUmT, resource, halfSat) result(up)
      real NutC, NutCmin, NutCopt, NutCmax  ! cellular quotas (current, minimum, optimum, maximum)
      real maxUmT                           ! species specific maximum growth rate
      real resource                         ! nutrient
      real halfSat                          ! species specific monod half saturation constant    
      ! curve forming parameters for decrease sigmoid            
      real, parameter :: L_de = 1.0               ! upper asymptote
      real, parameter :: k_de = -80.0             ! growth rate to form the curve 
      real, parameter :: b_de = 0.95              ! displacement along the x-axis
      real nNutC              ! normalized NutC ratio between minVal and optVal
      real APdecrease         ! acquistion potential as a sigmoidal curve [0,1]
      real optUptake          ! uptake at quota below the optimal nutrient quota
      real resource_check               ! check that resource is not negative
          
      ! decrease of acquisition potential when NutC = NutCopt
      nNutC = normalize(NutC, NutCmin, NutCmax)
      APdecrease = sigmoidLogistic(L_de, k_de, b_de, nNutC)
      
      ! check that resource if not negative
      resource_check = max(0.0, resource)
      
      ! species specific uptake at optimum nutrient ratio
      optUptake = monod(resource_check, halfSat) * maxUmT * NutCopt
      ! uptake at current nutrient ratio
      up = optUptake * APdecrease
   end function uptakeSi
   
end module protist_uptake_functions
    
    
module protist_photosynthesis_functions
    use protist_math_functions
    contains
       
   ! photosynthesis function, but it does not have a status related function included that would lower the playeau (f[1:4))
   ! Units: gC gC-1 d-1
   real function plateauPS(maxUmT, maxPSreq, relPS, NCopt, redco, NPSiCu, BR, PSDOC) result(plateau)
      real maxUmT, NCopt
      real maxPSreq                          ! fraction of C through photosynthesis (diatom = 1; MX < 1)
      real NPSiCu                            ! limiting internal nutrient value
      real PSDOC                             ! proportion of photosynthesis leaked as C
      real BR                                ! basal respiration
      real redco                             ! C respired to support nitrate reduction for NH4        (gC gN-1)
      real relPS                             ! relative PSmax:Umax on phototrophy                     (dl)
      ! real, parameter :: relPS = 4.0         ! relative PSmax:Umax on phototrophy                     (dl)
      real, parameter :: anaResp = 1.5       ! anabolic respiration cost in terms of C                (gC gN-1 d-1)
      
      !gC gC-1 d-1 plateau of the gross PE curve. USE EQUATION FROM FLYNN MAP 2001??
      ! added/changed compared to Flynn map 2001:
      ! PSDOC (I guess this is ok, but it is taken away is PS anyway again, to be honest i don't really understand) 
      ! relPS (don't understand why this is added)
      ! BR (not influence by NPSiCu)
      ! mixotrophy account through maxUmT* maxPSreq (1 for phytoplankton; <1 for mixoplankton)
      ! I do not like maxPSreq. I think it has too much influence and it is not measureable. and aren't CMs obligate phototroph
      ! if yes then they shoudl PS at max rate and only ing when limited. 
      !plateau = (maxUmT * maxPSreq * relPS * (1.0 + NCopt * (redco + anaResp)) * NPSiCu) + BR + 1e-6      
      plateau = ((1.0 + PSDOC) * maxUmT * maxPSreq * relPS * (1.0 + NCopt * (redco + anaResp)) * NPSiCu) + BR + 1e-6          

   end function plateauPS
   
   ! gross PS 
   ! Units: gC gC-1 d-1
   real function grossPS(ChlC, PFD, exat, atten, plateau, alpha) result(grossPhotoRate)
      real ChlC
      real plateau                           ! see function plateau above
      real PFD                               ! Photon flux density
      real exat                              ! extinction
      real atten                             ! attenuation      
      real alpha                             ! alpha for photosynthesis in protist in PE curve        ()
      real intermediateVal
        
      ! dl   intermediate in depth-integrated photosynthesis calculation according to the Smith equation
      intermediateVal   = (alpha * ChlC * PFD * 24.0 * 60.0 * 60.0) / plateau           
      ! gC gC-1 d-1   gross photosynthesis rate
      grossPhotoRate = plateau * (log(intermediateVal + sqrt(1.0 + intermediateVal**2)) - log(intermediateVal * exat + sqrt(1.0 + (intermediateVal * exat)**2))) / atten 
   end function grossPS
      
   ! net PS 
   ! Units: gC gC-1 d-1
   real function netPS(grossPhotoRate, PSDOC) result(netPhotoRate)
      real grossPhotoRate    ! see function grossPS
      real PSDOC             ! proportion of photosynthesis leaked as C     
      !gC gC-1 d-1   net photosynthesis rate retained for physiology
      netPhotoRate = grossPhotoRate * (1.0 - PSDOC)  
   end function netPS
   
   ! Chl synthesis 
   ! it also used a sigmoidal curve instead of a hyperbolic curve as a safetly net.... thus it needs to normalize ChlC between 0 and ChlCm 
   ! Units: gChl gC-1 d-1
   real function synthesisChl(ChlC, ChlCmin, ChlCmax, maxUmT, maxPSreq, NPSiCu, netPhotoRate, plateau) result(synChl)
      real ChlC, ChlCmin, ChlCmax, maxUmT
      real maxPSreq                        ! maximum requirement to cover C through photosynthesis (diatom = 1; MX < 1)
      real NPSiCu                          ! limiting internal nutrient value
      real netPhotoRate                    ! see function netPS above (Cfixation)
      real plateau                         ! see function plateau above (max photosynthetic rate possible)
      real, parameter :: Mphoto = 0.5      ! 2.0 scalar for controlling photoacclimation rate           (dl)        
      real, parameter :: L = 0.95          ! upper asymptote
      real, parameter :: k = -24           ! growth rate to form curve     
      real, parameter :: b = 0.7! 0.85          ! displacement along the x-axis
      real nChlC
      
      nChlC = normalize(ChlC, ChlCmin, ChlCmax)
      ! Chlorophyll synthesis and degradation
      synChl    = ChlCmax * maxUmT * maxPSreq * NPSiCu * Mphoto * (1.0 - netPhotoRate / plateau) * sigmoidLogistic(L, k, b, nChlC) 

      ! Kevins hyperbola    
      !if (ChlC < ChlCmax) then 
      !    synChl = ChlCmax * maxUmT * maxPSreq * NPSiCu * Mphoto * (1.0 - netPhotoRate / plateau) * (1.0 - ChlC / ChlCmax) / (1.0 - ChlC / ChlCmax + 0.05) 
      !else 
      !    synChl = 0.0
      !end if
      
   end function synthesisChl
   
   ! Chl degradation 
   ! it also used a sigmoidal curve instead of a hyperbolic curve as a safetly net.... thus it needs to normalize ChlC between 0 and ChlCm 
   ! Units: gChl gC-1 d-1
   real function degradeChl(ChlC, ChlCmax, maxUmT, NPSiCu) result(degChl)
      real ChlC, maxUmT, ChlCmax
      real NPSiCu                          ! limiting internal nutrient value

      ! Chlorophyll degradation
      !degChl   = (ChlC * maxUmT * (1.0 - NPSiCu))
      degChl    = (min(ChlC, ChlCmax) * maxUmT * (1.0 - NPSiCu))
   end function degradeChl
   
   ! uptake of protist chlorophyll by NCM
   ! Units: dl
   real function upChl(ChlC, ChlCmax) result(APchl)
      real ChlC, ChlCmax  
      real nChlC
      real, parameter :: L = 1.0           ! upper asymptote
      real, parameter :: k = -80           ! growth rate to form curve     
      real, parameter :: b = 0.93          ! displacement along the x-axis

      nChlC = normalize(ChlC, 0.0, ChlCmax)
      ! Chlorophyll uptake
      APchl = sigmoidLogistic(L, k, b, nChlC)
   end function upChl
    
end module protist_photosynthesis_functions
    
    
! most functions in this module are currently not used!     
module protist_phagotrophy_functions
    use protist_math_functions
    use protist_cell_functions
    contains  
    
    real function lightInhibition(PFD, relFeeding) result(lightInh)
        real PFD
        real relFeeding  ! rel feeding in night : day
        !real, parameter :: L = 1.0 !0.5           ! upper asymptote
        real, parameter :: k = 10.0          ! growth rate to form curve     
        real, parameter :: b = 1.0           ! displacement along the x-axis    
              
        lightInh = sigmoidLogistic(relFeeding, k, b, PFD) + (1.0 - relFeeding)
    end function lightInhibition

    
    
    ! create module? for preyInput, predInput??? 
    subroutine foodQuantity(nrPr, preyFlag, vec_preyC, vec_CcellPrey, vec_motPrey, vec_rPrey, vec_PR, vec_preyN, vec_preyP, motZoo, rZoo, CcellZoo, optCR, wTurb, sumCP, ingNC, ingPC) 
        integer nrPr
        real, dimension(nrPr) :: preyFlag, vec_preyC, vec_CcellPrey, vec_motPrey, vec_rPrey, vec_PR, vec_preyN, vec_preyP
        real motZoo, rZoo, CcellZoo, optCR, wTurb, sumCP, ingNC, ingPC
        real(8),  parameter :: PI_8  = 4 * atan (1.0_8)       

        
        real, dimension(nrPr) :: vec_nrPrey, vec_smallerVel, vec_largerVel, vec_encPrey, vec_capturedPrey, vec_propPrey, vec_ingNC, vec_ingPC
        
            ! cell abundance of prey per m3
            ! Units: nr cells m-3 (1e12: transform between g (preyC) and pg (CcontentPrey))
            vec_nrPrey = preyFlag * 1e12 * vec_preyC / vec_CcellPrey
            
            ! encounter Rate 
            ! Units: prey predator-1 d-1
            vec_smallerVel = min(vec_motPrey, motZoo)
            vec_largerVel = max(vec_motPrey, motZoo)
            vec_encPrey   = (24.0 * 60.0 * 60.0) * PI_8 *(vec_rPrey / 1E6 + rZoo / 1E6)**2 * vec_nrPrey * ((vec_smallerVel**2 + 3 * vec_largerVel**2 + 4 * wTurb**2) * ((vec_largerVel**2 + wTurb**2)**(-0.5))) * 3.0**(-1.0)
            
            ! potential C-specific capture of prey
            ! Units: gC gC-1 d-1 
            vec_capturedPrey = vec_encPrey * vec_PR * optCR * vec_CcellPrey / CcellZoo
        
            ! sum of all potential C-specific prey captures 
            ! Units: gC gC-1 d-1 (1.0E-20: protection against division by 0 in following line)
            sumCP = sum(vec_capturedPrey) + 1.0E-20
             
            ! proportion iPrey of total prey 
            ! Units: dl
            vec_propPrey = vec_capturedPrey / sumCP
            
            ! total captured prey Nut:C
            ! Units: gNut gC-1 d-1
            vec_ingNC = vec_propPrey * vec_preyN / vec_preyC             
            vec_ingPC = vec_propPrey * vec_preyP / vec_preyC 
            ingNC = sum(vec_ingNC)
            ingPC = sum(vec_ingPC)
    
    end subroutine foodQuantity
    
    
    ! encounter Rate 
    ! Units: prey predator-1 d-1
    real function encounterRate(preyC, CcontentPrey, radiusPrey, velPrey, radiusProtist, velProtist) result(encPrey)
       real preyC, CcontentPrey           ! prey biomass [gC m-3] and cellular Carbon content [pgC cell-1]
       real radiusPrey, radiusProtist     ! radius of protists [um]
       real velPrey, velProtist           ! motility of protists [m s-1] 
       real wTurb                         ! root-mean squared turbulence [m s-1]
       real rPrey, rProtist, TC, nrPrey   ! auxiliary variables
       real(8),  parameter :: PI_8  = 4 * atan (1.0_8)       

       ! Units: nr cells m-3
       ! Explanation: cell abundance of prey per m3
       ! 1e12: transform between g (preyC) and pg (CcontentPrey)
       nrPrey = 1e12 * preyC / CcontentPrey

       ! Units: m 
       rPrey = 1E-6 * radiusPrey
       rProtist = 1E-6 * radiusProtist
       ! time unit conversion
       TC = 24.0 * 60.0 * 60.0   
       ! Units: prey predator-1 d-1
       encPrey = TC * PI_8 *(rPrey + rProtist)**2 * nrPrey * ((min(velPrey, velProtist)**2 + 3 * max(velPrey, velProtist)**2 + 4 * wTurb**2) * ((max(velPrey, velProtist)**2 + wTurb**2)**(-0.5))) * 3.0**(-1.0)
    end function encounterRate
        
    ! assimilation efficieny parameter
    ! Units: dl
    real function assimilationEfficiency(stoichControl, halfSaturation, maxAss, minAss) result(opAE)
       real stoichControl      ! food quality [dl]
       real halfSaturation     ! response to prey quality [dl]
       real maxAss, minAss     ! maximum and minimum assimilaton efficiency [dl[
       ! units: dl
       opAE = (minAss + (maxAss - minAss) * monod(stoichControl, halfSaturation) * (1.0 + halfSaturation)) * stoichControl + 1e-20 
    end function assimilationEfficiency
    
    ! maximum ingestion of prey
    ! gC gC-1 d-1
    real function maxIngestion(maxUmT, basalRespiration, propLostIngC, opAE) result(maxIng)
       real maxUmT ! maximum species growth rate
       real basalRespiration    
       real propLostIngC      ! metabolic respiration/specific dynamic action/C lost during assimilation [dl]
       real opAE              ! assimilation efficiency parameter [dl]
       ! gC gC-1 d-1
       maxIng = ((maxUmT + basalRespiration) / (1.0 - propLostIngC)) / opAE
    end function maxIngestion
    
  end module protist_phagotrophy_functions


  
  
  
module protist_food_functions
    use protist_types    
    use protist_cell_functions
    contains
    
    !! FOOD QUANTITY -------------------------------------------------------------------------------    
    subroutine protistFoodQuantity(prot_array, r, wTurb, Ccell, optCR, mot, sumCP, ingNC, ingPC)
        type(protist_array), intent(inout)  :: prot_array
        real, intent(in)                    :: r, wTurb, Ccell, optCR, mot
        real, intent(out)                   :: sumCP, ingNC, ingPC
        real(8),  parameter                 :: PI_8  = 4 * atan (1.0_8) 

        ! encounter Rate 
        ! Units: prey predator-1 d-1
        prot_array%smallerVel = min(prot_array%motPrey, mot)
        prot_array%largerVel = max(prot_array%motPrey, mot)
        prot_array%encPrey = (24.0 * 60.0 * 60.0) * PI_8 *(prot_array%rPrey / 1E6 + r / 1E6)**2 * &
                                & prot_array%nrPrey * ((prot_array%smallerVel**2 + 3 * prot_array%largerVel**2 + 4 * wTurb**2) * &
                                & ((prot_array%largerVel**2 + wTurb**2)**(-0.5))) * 3.0**(-1.0)
        
        ! potential C-specific capture of prey
        ! Units: gC gC-1 d-1 
        prot_array%capturedPrey = prot_array%encPrey * prot_array%PR * optCR * prot_array%CcellPrey / Ccell

        ! sum of all potential C-specific prey captures 
        ! Units: gC gC-1 d-1 (1.0E-20: protection against division by 0 in following line)
        sumCP = sum(prot_array%capturedPrey) + 1.0E-20            
         
        ! proportion iPrey of total prey 
        ! Units: dl
        prot_array%propPrey = prot_array%capturedPrey / sumCP    
        
        ! total captured prey Nut:C
        ! Units: gNut gC-1 d-1
        prot_array%ingNC = prot_array%propPrey * prot_array%preyN / prot_array%preyC             
        prot_array%ingPC = prot_array%propPrey * prot_array%preyP / prot_array%preyC 
        ingNC = sum(prot_array%ingNC)
        ingPC = sum(prot_array%ingPC)

    end subroutine protistFoodQuantity
    
    
    
    
    !! FOOD QUALITY -------------------------------------------------------------------------------   
    subroutine protistFoodQuality(ingNC, ingPC, NCopt, PCopt, kAE, AEm, AEo, ppNC, ppPC, stoichP, opAE)
        real, intent(in)                     :: ingNC, ingPC, NCopt, PCopt, kAE, AEm, AEo
        real, intent(out)                    :: ppNC, ppPC, stoichP, opAE

        ! MDK 23-11-2021: this routine is identical to protistZOOFoodQuality. Replace by one generic routine?
        ! quota of captured prey in relation to predator    
        ! Units: dl       
        ppNC = ingNC / NCopt
        ppPC = ingPC / PCopt
        ! determine limiting nutrient in prey or set to 1 if preNut > predNut
        ! Units: dl
        stoichP = min(ppNC, ppPC, 1.0)
                        
        !! assimilation efficiency for prey 
        !! Units: dl  
        opAE = (AEo + (AEm - AEo) * stoichP / (stoichP + kAE) * (1.0 + kAE)) * stoichP + 1.0E-20
             
    end subroutine protistFoodQuality
    
    
    ! INGESTION  ------------------------------------------------------------------------------- 
    subroutine protistIngestion(maxIng, sumCP, ingNC, ingPC, KI, ingSat, ingC, ingN, ingP)
        real, intent(in)                     :: maxIng, sumCP, ingNC, ingPC
        real, intent(out)                    :: KI, ingSat, ingC, ingN, ingP

        ! half saturation constant for satiation feedback (see paper by Flynn and Mitra 2016)
        ! Units: gC gC-1 d-1
        KI = (maxIng / 4.0)

        ! ingestion with satiation feedback included
        ! Units: gC gC-1 d-1
        ingSat = maxIng * sumCP / (sumCP + KI)

        ! ingestion of C
        ! Units: gC gC d-1
        ingC = min(ingSat, sumCP)   
        ingN = ingC * ingNC            
        ingP = ingC * ingPC    

    end subroutine protistIngestion
    
    
    
    
    ! MORTALITY ------------------------------------------------------------------------------- 
    subroutine protistMortality(protC, MrtRT, Q10, Temp, RT, FrAut, FrDet, mrt, mrtFrAut,mrtFrDet)
        real, intent(in)                     :: protC, MrtRT, Q10, Temp, RT, FrAut, FrDet
        real, intent(out)                    :: mrt, mrtFrAut, mrtFrDet
         
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
            
    end subroutine protistMortality
    
end module protist_food_functions