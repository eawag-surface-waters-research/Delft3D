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

      subroutine wlcwoc ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
            
!>\file
!>       Heat Load Capacity (WLC) and Heat Extraction Capacity (WOC)                                                      
      
      IMPLICIT NONE

!     arguments

      REAL               :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL               :: FL(*)              ! in/out flux array
      INTEGER            :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER            :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER            :: NOSEG              ! in     number of segments
      INTEGER            :: NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER            :: IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER            :: IKNMRK(*)          ! in     segment features array
      INTEGER            :: NOQ1               ! in     number of exchanges in first direction
      INTEGER            :: NOQ2               ! in     number of exchanges in second direction
      INTEGER            :: NOQ3               ! in     number of exchanges in third direction
      INTEGER            :: NOQ4               ! in     number of exchanges in fourth direction

!     from PMSA array

      REAL               :: Temp               !1   in ambient water temperature                      (oC)                
      REAL               :: surtemp            !2  Surplus temperature                                (oC)
      REAL               :: Surf               !3  horizontal surface area of a DELWAQ segment        (m2)                
      REAL               :: Volume             !4  volume of computational cell                       (m3)                
      REAL               :: RhoWater           !5  density of water                                   (kg/m3)             
      REAL               :: CP                 !6  specific heat of water                             (J/kg/oC)                                   
      REAL               :: Width              !7  total width                                        (m)                 
      REAL               :: Depth              !8  depth of segment                                   (m)                 
      REAL               :: Velocity           !9  horizontal flow velocity                           (m/s)               
      REAL               :: fSpeHeDis          !1O  Zelfkoelgetal                                     (W/m2/C)
      REAL               :: Tmax               !11  maximum wT after heat discharge                   (oC) 
      REAL               :: dTmaxIncr          !12  maximum wT increase after heat discharge          (oC)
      REAL               :: Tmin               !13  minimum wT after heat uptake (aanslag T)          (oC)
      REAL               :: dTmaxDecr          !14  maximum wT decrease heat uptake                   (oC)
      REAL               :: dtminDecr          !15  minimum wT decrease heat uptake (                  (oC)      

!     fluxes

!     Dummy              :: dummy              ! 1      dummy                                         [oC/d]

!     local decalrations

      REAL               :: flowabs            ! flow absolute value used in wlc and woc              (m3/s)
	REAL               :: flowsegvel         ! flow calc from velocity and width and depth          (m3/s)
      REAL               :: flowseg            ! flow from routine velocity SW=3                      (m3/s)      
      REAL               :: SWCalcVelo         ! I  switch (1=lin avg, 2=Flow avg, 3=Area avg)         (-)    
      REAL               :: Z
      

      real               ::dTWLC               ! dT for heat loading capacity
      real               ::dTWOC               ! dT for heat extraction capacity
C      
      REAL               ::WLC_Q_s             ! O  Heat Loading Capacity Flow per segment             MW                  
      REAL               ::WLC_A_s             ! O  Heat Loading Capacity Atmosphere per segment       MW   
      REAL               ::WLC_m2              ! O  Heat Loading Capacity total  per m2           W/m2  
C     
      REAL               ::TMinWOC             ! I  Minimum required water temperature to start WO   (oC)
      REAL               ::dTMaxWOC            ! I  Maxium temperature decrease by WO                (oC) 
      REAL               ::dTMinWOC            ! I  Minimum temperature decrease by WO               (oC) 
      REAL               ::WOC_Q_s             ! O  Heat Extraction Capacity per segment               MW                  
      REAL               ::WOC_A_s             ! O  Heat Extraction Capacity Atmosphere per segment    MW                
      REAL               ::WOC_m2              ! O  Heat Extraction Capacity Total per m2             W/m2                  
    
C     tbv alternatieve berekening met voorbelasting 
      real               ::dTWLC2      !dT for heat loading capacity second method
      real               ::dTWOC2      !dT for heat extraction capacity second metod

      REAL               ::WLC2_Q_s     ! O  Heat Loading Capacity Flow per segment             MW                  
      REAL               ::WLC2_A_s     ! O  Heat Loading Capacity Atmosphere per segment       MW   
      REAL               ::WLC2_m2      ! O  Heat Loading Capacity total  per m2           W/m2  
C      
      REAL               ::WOC2_Q_s     ! O  Heat Extraction Capacity per segment               MW                  
      REAL               ::WOC2_A_s     ! O  Heat Extraction Capacity Atmosphere per segment    MW     
c      
      REAL               ::WOC2_m2      ! O  Heat Extraction Capacity total per m2              W/m2                  
    
                                  
      INTEGER  IP1 ,IP2 ,IP3 ,IP4 ,IP5 ,IP6 ,IP7 ,IP8 ,IP9 ,IP10,
     J         IP11,IP12,IP13,IP14,IP15,IP16,IP17,IP18,IP19,IP20,
     J         IP21,IP22,IP23,IP24,IP25,IP26,IP27,IP28,IP29, IP30,
     J         IP31,IP32,IP33,IP34,IP35
      INTEGER  IFLUX , ISEG  , IKMRK2

      IP1  = IPOINT(1 )
      IP2  = IPOINT(2 )
      IP3  = IPOINT(3 )
      IP4  = IPOINT(4 )
      IP5  = IPOINT(5 )
      IP6  = IPOINT(6 )
      IP7  = IPOINT(7 )
      IP8  = IPOINT(8 )
      IP9  = IPOINT(9 )
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
      IP16 = IPOINT(16)
      IP17 = IPOINT(17)
      IP18 = IPOINT(18)
      IP19 = IPOINT(19)
      IP20 = IPOINT(20)
      IP21 = IPOINT(21)
      IP22 = IPOINT(22)
      IP23 = IPOINT(23)
      IP24 = IPOINT(24)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)
      IP29 = IPOINT(29) 
      IP30 = IPOINT(30) 
      IP31 = IPOINT(31) 
      IP32 = IPOINT(32) 
      IP33 = IPOINT(33)       
      IP34 = IPOINT(34) 
      IP35 = IPOINT(35) 
      !
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
C                                                                                                     
         Temp       = PMSA(IP1)
         Surtemp    = PMSA(IP2) 
         Surf       = PMSA(IP3) 
         Volume     = PMSA(IP4)          
         RhoWater   = PMSA(IP5)
         CP         = PMSA(IP6)
         Depth      = PMSA(IP7)
         Width      = PMSA(IP8)
         Velocity   = PMSA(IP9)         
         SWCalcVelo = PMSA(IP10)         
         flowseg    = PMSA(IP11)         
         Z          = PMSA(IP12)	
         Tmax       = PMSA(IP13)	
         dTmaxIncr  = PMSA(IP14)        
         Tmin       = PMSA(IP15) 	
         dTmaxDecr  = PMSA(IP16)	
         dTminDecr  = PMSA(IP17)
       
C        Calculate FLOW from from Velocity, Width and Depth if possible (WIDTH required, not available in D3D)
C        Velocity (module VELOC) can be calculated using various averaging options but Width is not available in D3D (it is in Sobek)
C        Module VELOC calculates FLOW too (only for SWCalVelo=3). This is the preffered option which works for D3D and Sobek
C        If other options for Velociy calculation are needed this works for Sobek, to make it work in D3D provide WIDTH as input.    
         flowsegvel = depth * width * velocity
         if ( int ( SWCalcVelo + 0.5 ) .eq. 3 ) then
             flowabs = abs(flowseg)
         else    
             flowabs = abs(flowsegvel)
         endif    
         
C        
C       WarmteLozingsCapaciteit (WLC) = warmtelozing = KoudeOnttrekkingscapacitiet 
C         
         dTWLC = 0
         dTWLC2 = 0
c       
c        maximale opwarming - geldig domein 0 tot dTmaxIncr (in praktijk 0 tot 3graden)
c                           - maximale temperatuur (in praktijk 28 graden)
c                           - dTmaxIncr corrigeren voor al geloosde warmte via SurTemp
c
         dTWLC = max(min(Tmax - Temp,dTmaxIncr),0.0) 
         
c        Extra beperking door gerealiseerde bijdrage lozingen
c        voorbeeld: max opwarming (TmaxDelt=3) waarvan al 1 graad bovenstrooms is opgevuld door
c        warmtelozingen (Surtemp) dan nog maar 2 graden opwarming mogelijk
c         
         dTmaxIncr = dTmaxIncr -surtemp
         dTWLC2 = max(min(Tmax - Temp,dTmaxIncr),0.0)         
     
c        potentiele bijdrage lucht-water aan koelcapaciteit via zelfkoelgetal
c        m2 . gC . W/m2/oC / 10^6 =  Mega Watt per segment
         WLC_A_s     = Surf * dTWLC * Z /1000000  
         WLC2_A_s    = Surf * dTWLC2 * Z /1000000  

c        potentiele bijdrage debiet aan koelcapaciteit 
c        m3/s . gC . J/kg/gC . kg/m3 = J/s / 10^6 = Mega Watt per segment   
         WLC_Q_s   = (flowabs * dTWLC * CP * RhoWater) /1000000 
         WLC2_Q_s   = (flowabs * dTWLC2 * CP * RhoWater) /1000000 
         
        if (surf > 1.0) then
c           Meag Watt * 10^6 ->  Watt / m2  
	      WLC_m2     = ( WLC_A_s  + WLC_Q_s)  / Surf * 1000000
	      WLC2_m2    = ( WLC2_A_s + WLC2_Q_s ) / Surf * 1000000           
        endif
C        
C       WarmteOtrekkingscapaciteit (WOC) = koudelozing
C
         dTWOC = 0
         dTWOC2 = 0
         
         if (Temp .ge. TMin) then
C            if Temp>15 allow WOC with min -3C (down to 12)
C            if Termp=26 allow WOC with max -6C (down to 20)
             dTWOC = min(max((Temp - TMin), dTMinDecr),dTmaxDecr) 
C            verruim WOC met surplustemperatuur HOE???
             Tmin = Tmin - Surtemp
             dTminDecr = dTminDecr
             dTmaxDecr = dTmaxDecr + Surtemp            
             dTWOC2 = min(max((Temp - TMin), dTMinDecr),dTmaxDecr) 
         endif
     
c        potentiele bijdrage lucht-water aan koelcapaciteit via zelfkoelgetal
c        m2 . gC . J.s-1.m-2.gC-1 = J.s-1 = W * 10-6 = MW per segment
c        per segment in MW
         WOC_A_s   = Surf * dTWOC * Z /1000000  
         WOC2_A_s  = Surf * dTWOC2 * Z /1000000  

c        potentiele bijdrage debiet aan koelcapaciteit 
c        m3/s . gC . J/kg/gC . kg/m3 = J/s = W * 10-6 = MW per segment    
         WOC_Q_s    = (flowabs * dTWOC * CP * RhoWater) /1000000 
         WOC2_Q_s   = (flowabs * dTWOC2 * CP * RhoWater) /1000000 
        
c        bijdrage stroming en opprvalk per surface area in W/m2
         if (surf > 1.0) then
	      WOC_m2      = ( WOC_A_s + WOC_Q_s )  / Surf * 1000000
 	      WOC2_m2     = ( WOC2_A_s + WOC2_Q_s) / Surf * 1000000           
         endif
C        
C
C   *****     End of your code       *****
C                                                                                                      
         PMSA(IP18) = WLC_Q_s
         PMSA(IP19) = WLC_A_s        
         PMSA(IP20) = WLC2_Q_s 
         PMSA(IP21) = WLC2_A_s
         PMSA(IP22) = dTWLC
         PMSA(IP23) = dTWLC2  
         PMSA(IP24) = WLC_m2
         PMSA(IP25) = WLC2_m2
         PMSA(IP26) = WOC_Q_s
         PMSA(IP27) = WOC_A_s         
         PMSA(IP28) = WOC2_Q_s
         PMSA(IP29) = WOC2_A_s
         PMSA(IP30) = dTWOC  
         PMSA(IP31) = dTWOC2 
         PMSA(IP32) = WOC_m2  
         PMSA(IP33) = WOC2_m2          
         PMSA(IP34) = flowsegvel
         PMSA(IP35) = flowabs             
C                                                                                                     
         IFLUX = IFLUX + NOFLUX
         IP1   = IP1   + INCREM ( 1  )
         IP2   = IP2   + INCREM ( 2  )
         IP3   = IP3   + INCREM ( 3  )
         IP4   = IP4   + INCREM ( 4  )
         IP5   = IP5   + INCREM ( 5  )
         IP6   = IP6   + INCREM ( 6  )
         IP7   = IP7   + INCREM ( 7  )
         IP8   = IP8   + INCREM ( 8  )
         IP9   = IP9   + INCREM ( 9  )
         IP10  = IP10  + INCREM ( 10 )
         IP11  = IP11  + INCREM ( 11 )
         IP12  = IP12  + INCREM ( 12 )
         IP13  = IP13  + INCREM ( 13 )
         IP14  = IP14  + INCREM ( 14 )
         IP15  = IP15  + INCREM ( 15 )
         IP16  = IP16  + INCREM ( 16 )
         IP17  = IP17  + INCREM ( 17 )
         IP18  = IP18  + INCREM ( 18 )
         IP19  = IP19  + INCREM ( 19 )
         IP20  = IP20  + INCREM ( 20 )
         IP21  = IP21  + INCREM ( 21 )
         IP22  = IP22  + INCREM ( 22 )
         IP23  = IP23  + INCREM ( 23 )
         IP24  = IP24  + INCREM ( 24 )
         IP25  = IP25  + INCREM ( 25 )
         IP26  = IP26  + INCREM ( 26 )
         IP27  = IP27  + INCREM ( 27 )
         IP28  = IP28  + INCREM ( 28 )
         IP29  = IP29  + INCREM ( 29 )  
         IP30  = IP30  + INCREM ( 30 )            
         IP31  = IP31  + INCREM ( 31 ) 
         IP32  = IP32  + INCREM ( 32 )            
         IP33  = IP33  + INCREM ( 33 )           
         IP34  = IP34  + INCREM ( 34 )            
         IP35  = IP35  + INCREM ( 35 )            
 9000       CONTINUE
!
      RETURN
!
      END
