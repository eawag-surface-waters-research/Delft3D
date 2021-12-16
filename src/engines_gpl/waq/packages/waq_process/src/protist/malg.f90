      SUBROUTINE MALG       ( PMSA   , FL     , IPOINT , INCREM, NOSEG , &
                              NOFLUX , IEXPNT , IKNMRK , NOQ1  , NOQ2  , &
                              NOQ3   , NOQ4   )


!
!*******************************************************************************
!

      use malg_functions_general
      use malg_functions_Broch
    
      IMPLICIT NONE
      REAL(4) PMSA(*)     !I/O Process Manager System Array, window of routine to process library
      REAL(4) FL(*)       ! O  Array of fluxes made by this process in mass/volume/time
      INTEGER IPOINT(113) ! I  Array of pointers in PMSA to get and store the data
      INTEGER INCREM(113) ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      INTEGER NOSEG       ! I  Number of computational elements in the whole model schematisation
      INTEGER NOFLUX      ! I  Number of fluxes, increment in the FL array
      INTEGER IEXPNT(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      INTEGER IKNMRK(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      INTEGER NOQ1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      INTEGER NOQ2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
      INTEGER NOQ3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      INTEGER NOQ4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      INTEGER IPNT(113)   !    Local work array for the pointering
      INTEGER ISEG        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      REAL(4) MALS        ! I  MacroALgae Structural biomass                       (gDM/m2)
      REAL(4) MALN        ! I  MacroALgae Nitrogen storage                         (gN/m2)
      REAL(4) MALP        ! I  MacroALgae phosphorous storage                      (gP/m2)
      REAL(4) MALC        ! I  MacroALgae carbon storage                           (gC/m2)
      REAL(4) NO3         ! I  Nitrate ambient concentration                       (gN/m3)
      REAL(4) NH4         ! I  Ammonium ambient concentration                      (gN/m3)
      REAL(4) PO4         ! I  Orthophosphate ambient concentration                (gP/m3)
      REAL(4) FrBmMALS    ! I  Fraction of MALS in this segment                    (-)
      REAL(4) TotAreMAL   ! I  area of frond in this column                        (m2)
      INTEGER MBotSeg     ! I  bottom segment for this segment                     (-)
      !REAL(4) Nfrond      ! I  number of fronds in segment                         (-)
      REAL(4) Surf        ! I  horizontal surface area of a DELWAQ segment         (m2)
      REAL(4) Depth       ! I  depth of segment                                    (m)
      REAL(4) LocalDepth  ! I  depth of segment below surface                      (m)
      REAL(4) Temp        ! I  ambient water temperature                           (oC)
      REAL(4) Velocity    ! I    velocity                                          (m/s)
      REAL(4) RadSurf     ! I  irradiation at the water surface                    (W/m2)
      REAL(4) ExtVl       ! I  total extinction coefficient of visible light       (1/m)
      REAL(4) daylend     ! I  length of current day                               (-)
      REAL(4) daylenp     ! I  length of previous day                              (-)
      REAL(4) daylenm     ! I  max day length at this latitude                     (-)
      REAL(4) DELT        ! I  timestep for processes                              (d)
      REAL(4) SeedMass    ! I  mass of a seed at begining of simulation            (gDM)
      REAL(4) DoLocUpP    ! I  include phosphorous uptake in P balance for MALS    (-)
      REAL(4) MALNmin     ! I  minimal N in nitrogen storage                       (gN/gDM)
      REAL(4) MALNmax     ! I  maximum N in nitrogen storage                       (gN/gDM)
      REAL(4) MALPmin     ! I  minimal P in phosphorous storage                    (gP/gDM)
      REAL(4) MALPmax     ! I  maximum P in nitrogen storage                       (gP/gDM)
      REAL(4) MALCmin     ! I  minimal C in carbon storage                         (gC/gDM)
      REAL(4) MALCmax     ! I  maximum C in nitrogen storage                       (gC/gDM)
      REAL(4) CDRatMALS   ! I  Carbon to dry matter ratio in MALS                  (gC/gDM)
      REAL(4) NCRatMALS   ! I  Nitrogen to carbon ratio in MALS                    (gN/gC)
      REAL(4) PCRatMALS   ! I  Phosphorous to carbon ratio in MALS                 (gP/gC)
      REAL(4) Kn          ! I  mass of nitrogen reserves per gram nitrogen         (gDM/gN)
      REAL(4) Kp          ! I  mass of nitrogen reserves per gram nitrogen         (gDM/gN)
      REAL(4) Kc          ! I  mass of carbon reserves per gram carbon             (gDM/gC)
      REAL(4) Kdw         ! I  dry weight to wet weight ratio MALS                 (gDM/g)
      REAL(4) mumax       ! I  MALS maximum growth rate parameter 1                (1/d)
      REAL(4) m1          ! I  MALS growth rate parameter 1                        (-)
      REAL(4) m2          ! I  MALS growth rate parameter 2                        (-)
      REAL(4) MALS0       ! I  MALS growth rate parameter 3                        (m2)
      REAL(4) a1          ! I  MALS photoperiod parameter 1                        (-)
      REAL(4) a2          ! I  MALS photoperiod parameter 2                        (-)
      REAL(4) epsBroch    ! I  (epsilon) erosion/mortality parameter macroalgae    (m2/DM)
      REAL(4) mrtMAL      ! I  maximum mortality rate                              (1/d)
      REAL(4) FrPOC1MAL   ! I  Fraction MALS that goes to POC1 in decay            (-)
      REAL(4) FrPOC2MAL   ! I  Fraction MALS that goes to POC2 in decay            (-)
      REAL(4) Ksn         ! I  half saturation N uptake                            (gN/m3)
      REAL(4) Ksp         ! I  half saturation P uptake                            (gP/m3)
      REAL(4) JNmax       ! I  maximum N uptake rate                               (gN/m2 d)
      REAL(4) JPmax       ! I  maximum p uptake rate                               (gP/m2 d)
      REAL(4) Vel65       ! I  current speed at which J = 0.65Jmax                 (m/s)
      REAL(4) NH4Thresh   ! I  threshold below which NH4 uptake by MALN is limited (g/m3)
      REAL(4) alpha0      ! I  photosynthetic efficiency                           (gC/dm2/h/(umol photons m-2 s-1)-1)
      REAL(4) Isat        ! I  light intensity where photosynthesis is at max      (W/m2)   
      REAL(4) exuMALC     ! I  exudation parameter                                 (gC/gC)
      REAL(4) R1          ! I  Reference respiration rate at T1                    (gC/m2 d)
      REAL(4) Tr1         ! I  reference temperature 1 for respiration             (K)
      REAL(4) Tar         ! I  Arrhenius temperature for respiration               (K)
      REAL(4) K0HrvMALS   ! I  Zero order harvesting rate of MALS                  (gC/m2/d)
      REAL(4) K1HrvMALS   ! I  First order harvesting rate of MALS                 (1/d)
      REAL(4) ArDenMAL    ! I                                                      (gDM/m2)
      INTEGER BrochMeth
      REAL(4) qTmu        ! I                                                      (-)
      REAL(4) Trefmu      ! I                                                      (oC)
      REAL(4) DoLocUpC    ! I  include carbon storage dynamics MALC                (-)
      REAL(4) NH4pref     ! I  
      REAL(4) JNO3fact    ! I  
      REAL(4) Ksno3       ! I  

      REAL(4) MALNDMS     ! O  macroalgae N storage in segment                     (gN/gDM)
      REAL(4) MALPDMS     ! O  macroalgae P storage in segment                     (gP/gDM)
      REAL(4) MALCDMS     ! O  macroalgae C storage in segment                     (gC/gDM)
      REAL(4) MALSNC      ! O  ratio N:C in whole plant                            (-)
      REAL(4) MALSPC      ! O  ratio P:C in whole plant                            (-)
      REAL(4) MALSCDM     ! O  ratio C:DM in whole plant                           (-)
      REAL(4) MALSNDM     ! O  ratio N:DM in whole plant                           (-)
      REAL(4) MALSPDM     ! O  ratio P:DM in whole plant                           (-)      
      REAL(4) Wdry        ! O
      REAL(4) Wwet        ! O
      REAL(4) WdryTot     ! O
      REAL(4) WwetTot     ! O  
      REAL(4) NFrond      ! O  calculated number of fronds in cell                 (m2)
      REAL(4) SpecArea    ! O  area of each frond     
      REAL(4) LimDen      ! O
      REAL(4) LimPho      ! O
      REAL(4) LimTemp     ! O
      REAL(4) LimMALN     ! O
      REAL(4) LimMALP     ! O
      REAL(4) LimMALC     ! O
      REAL(4) LimNut      ! O 
      REAL(4) muMALS      ! O
      real(4) mrtMALS     ! O     
      REAL(4) LocGroPS    ! O  local gross growth of MALS                          (gDM/m2/d)
      REAL(4) LocNetPS    ! O  local net growth of MALS                            (gDM/m2/d)
      REAL(4) LocDecS     ! O  local decay of MALS                                 (gDM/m2/d)
      REAL(4) LocGroN     ! O  local uptake of MALN for growth                     (gN/m2/d)
      REAL(4) LocGroP     ! O  local uptake of MALP for growth                     (gP/m2/d)
      REAL(4) LocGroC     ! O  local uptake of MALC for growth                     (gC/m2/d)
      REAL(4) LimVel      ! O  ambient nitrogen limitation for storage             (-)                 
      REAL(4) LimN        ! O  ambient nitrogen limitation for storage             (-)                 
      REAL(4) LimP        ! O  ambient phosphorous limitation for storage          (-)                 
      REAL(4) LocUpN      ! O  Local N storage uptake flux in this segment         (gN/m2/d)           
      REAL(4) LocUpP      ! O  Local P storage uptake flux in this segment         (gP/m2/d)           
      REAL(4) FrNH4MALN   ! O  fraction ammonium uptake by MALN                    (-)            
      REAL(4) LocUpC      ! O  Local C uptake flux                                 (gC/m2/d)           
      REAL(4) beta        ! O  photoinhibition parameter                           (-)      
      REAL(4) Ilay        ! O  Radiation experienced by frond in current layer     (W/m2)              
      REAL(4) Ilayu       ! O  photon radiation experienced by frond in current l  (umol/m2/s)         
      REAL(4) GrosMALC    ! O  gross photosynthesis MALC                           (gC/m2/d)           
      REAL(4) RespMALC    ! O  respiration MALC                                    (gC/m2/d)           
      REAL(4) ExudMALC    ! O  exudation MALC                                      (gC/m2/d)           
      REAL(4) RespMALS    ! O  cannibalization MAL                                 (gDW/m2/d)           
      REAL(4) HrvMALS     ! O  Total harvest MALS                                  (gDM/m2/d)      
      
      REAL(4) dGrMALS     ! F Growth of MALS on storage                            (gDM/m2/d)           
      REAL(4) dGrMALN     ! F Utilization of MALN in growth                        (gN/m2/d)           
      REAL(4) dGrMALP     ! F Utilization of MALP in growth                        (gP/m2/d)           
      REAL(4) dGrMALC     ! F Utilization of MALC in growth                        (gC/m2/d)          
      REAL(4) dDecMALS    ! F Decay of MALS                                        (gDM/m2/d)           
      REAL(4) dPrPOC1M    ! F POC1 production MALS                                 (gC/m3/d)           
      REAL(4) dPrPOC2M    ! F POC2 production MALS                                 (gC/m3/d)           
      REAL(4) dPrPON1M    ! F PON1 production MALS                                 (gN/m3/d)           
      REAL(4) dPrPON2M    ! F PON2 production MALS                                 (gN/m3/d)           
      REAL(4) dPrPOP1M    ! F POP1 production MALS                                 (gP/m3/d)           
      REAL(4) dPrPOP2M    ! F POP2 production MALS                                 (gP/m3/d)           
      REAL(4) dUpMALN     ! F N uptake by Macroalgae nitrogen storage              (gN/m2/d)           
      REAL(4) dUpNO3M     ! F NO3 uptake by Macroalgae nitrogen storage            (gN/m3/d)           
      REAL(4) dUpNH4M     ! F NH4 uptake by Macroalgae nitrogen storage            (gN/m3/d)           
      REAL(4) dUpMALP     ! F P uptake by Macroalgae nitrogen storage              (gN/m2/d)           
      REAL(4) dUpPO4M     ! F PO4 uptake by Macroalgae nitrogen storage            (gP/m3/d)           
      REAL(4) dUpMALC     ! F C uptake by Macroalgae nitrogen storage              (gN/m2/d)           
      REAL(4) dPrMALC     ! F Primary production of macroalgae                     (gC/m3/d)           
      REAL(4) dExMALC     ! F Exudation of macroalgae                              (gC/m3/d)           
      REAL(4) dRspMALC    ! F Respiration of macroalgae                            (gC/m3/d)           
      REAL(4) dCanMALS    ! F Cannibalization of structural mass in respiration    (gC/m2/d)             
      REAL(4) dCanCMALS   ! F Mineralization of C through cannibalization          (gC/m3/d)           
      REAL(4) dCanNMALS   ! F Mineralization of N through cannibalization          (gN/m3/d)           
      REAL(4) dCanPMALS   ! F Mineralization of P through cannibalization          (gP/m3/d)           
      REAL(4) dHrvMALS    ! F harvesting flux MALS                                 (gDM/m2/d)          
      REAL(4) dHrvMALN    ! F harvesting flux MALN                                 (gN/m2/d)           
      REAL(4) dHrvMALP    ! F harvesting flux MALP                                 (gP/m2/d)           
      REAL(4) dHrvMALC    ! F harvesting flux MALC                                 (gC/m2/d)                            
      
      INTEGER IdGrMALS     
      INTEGER IdGrMALN     
      INTEGER IdGrMALP     
      INTEGER IdGrMALC  
      INTEGER IdDecMALS  
      INTEGER IdPrPOC1M 
      INTEGER IdPrPOC2M 
      INTEGER IdPrPON1M 
      INTEGER IdPrPON2M 
      INTEGER IdPrPOP1M 
      INTEGER IdPrPOP2M 
      INTEGER IdUpMALN                                              
      INTEGER IdUpNO3M        
      INTEGER IdUpNH4M       
      INTEGER IdUpMALP                                          
      INTEGER IdUpPO4M                 
      INTEGER IdUpMALC                                              
      INTEGER IdPrMALC           
      INTEGER IdExMALC             
      INTEGER IdRspMALC      
      INTEGER IdCanMALS       
      INTEGER IdCanCMALS               
      INTEGER IdCanNMALS             
      INTEGER IdCanPMALS           
      INTEGER IdHrvMALS            
      INTEGER IdHrvMALN               
      INTEGER IdHrvMALP           
      INTEGER IdHrvMALC                             
      INTEGER FLCREM

      INTEGER IKMRK1
      INTEGER IKMRK2
      
      ! extra/internal
      INTEGER DOMALG
      REAL(4) TotN
      REAL(4) TotP
      REAL(4) TotC
      REAL(4) EffNit     
      REAL(4) P 
      REAL(4) R
      REAL(4) E 
      REAL(4) LocUpNH4
      REAL(4) LocUpNO3

      REAL(4) chk         !    debug variable
      
      LOGICAL DOFRND      !    is the first time
      DATA    DOFRND /.TRUE./
      SAVE    DOFRND

!     
!*******************************************************************************
!
      ! ------------ INITIALIZATION ------------ !

      ! NFrond calculation loop at 1st time step, assigns Nfrond to bottom segments with mass
      IF (DOFRND) THEN 
          IPNT       = IPOINT
          
          ! for all segs
          DO ISEG = 1,NOSEG
             PMSA( IPNT(82) ) = 0.0
             DOMALG = 0
             CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
             
             ! if active
             IF (IKMRK1.EQ.1) THEN
                 CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)

                 MBotSeg    = nint(PMSA( IPNT(10) ))
                 
                 IF (MBotSeg .GE. 0.) THEN
                     
                     ! if bottom segment, calculate NFrond using MALS,
                     ! and put NFrond in the bottom segment
                     IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
                            MALS       = PMSA( IPNT(1) )
                        
                        ! if there is biomass in the column
                        ! due to presence in bed
                        IF (MALS .gt. 0.0) THEN    
                            write(*,*) 'initializing step'

                            MALN       = PMSA( IPNT(2) )
                            MALP       = PMSA( IPNT(3) )
                            MALC       = PMSA( IPNT(4) )        
                        
                            Surf       = PMSA( IPNT(12) )   
                        
                            SeedMass   = PMSA( IPNT(23) )       
              
                            Kn         = PMSA( IPNT(34) )
                            Kp         = PMSA( IPNT(35) )
                            Kc         = PMSA( IPNT(36) )
                            Kdw        = PMSA( IPNT(37) )          
                   
                            MALN = MALN / MALS ! gN/m2 to gN/gDM
                            MALP = MALP / MALS ! gP/m2 to gP/gDM
                            MALC = MALC / MALS ! gC/m2 to gC/gDM
                            
                            ! find amount of mass in this column                       
                            Wdry = MALS * (1 + Kn*MALN + Kp*MALP + Kc*MALC)         
                            Wwet = MALS * (1/Kdw + Kn*MALN + Kp*MALP + Kc*MALC) 
                        
                            ! number of fronds is total mass divided by mass per sporophyte
                            NFrond = Wdry * Surf / SeedMass
                            IF (NFrond .lt. 1.0) THEN
                                NFrond = 1.0
                            ENDIF
                      
                            ! put in output to be read in input
                            PMSA(IPNT(82)) = NFrond

                            write(*,*) 'putting' , NFrond
                            write(*,*) 'fronds in segment ' , ISEG

                        ENDIF
                     ENDIF
                 ELSE
                     ! MALS distributed over the water column
                     ! put NFrond in the current segment
                     MALS       = PMSA( IPNT(1) )
                        
                     ! if there is biomass in the column
                     IF (MALS .gt. 0.0) THEN
                         write(*,*) 'initializing step'

                         MALN       = PMSA( IPNT(2) )
                         MALP       = PMSA( IPNT(3) )
                         MALC       = PMSA( IPNT(4) )        
                        
                         Surf       = PMSA( IPNT(12) )   
                        
                         SeedMass   = PMSA( IPNT(23) )       
              
                         Kn         = PMSA( IPNT(34) )
                         Kp         = PMSA( IPNT(35) )
                         Kc         = PMSA( IPNT(36) )
                         Kdw        = PMSA( IPNT(37) )          
                   
                         MALN = MALN / MALS ! gN/m2 to gN/gDM
                         MALP = MALP / MALS ! gP/m2 to gP/gDM
                         MALC = MALC / MALS ! gC/m2 to gC/gDM
                            
                         ! find amount of mass in this column                       
                         Wdry = MALS * (1 + Kn*MALN + Kp*MALP + Kc*MALC)         
                         Wwet = MALS * (1/Kdw + Kn*MALN + Kp*MALP + Kc*MALC) 
                        
                         ! number of fronds is total mass divided by mass per sporophyte
                         NFrond = Wdry * Surf / SeedMass
                         IF (NFrond .lt. 1.0) THEN
                             NFrond = 1.0
                         ENDIF
                      
                         ! put in output to be read in input
                         PMSA(IPNT(82)) = NFrond

                         write(*,*) 'putting' , NFrond
                         write(*,*) 'fronds in segment ' , ISEG
                     ENDIF
                 ENDIF
             ENDIF
             
          IPNT = IPNT   +   INCREM

          ENDDO
          
          DOFRND = .FALSE.

      ENDIF

!*******************************************************************************
      ! indexes in fluxes array
      IPNT        = IPOINT
      IdGrMALS    = 1  
      IdGrMALN    = 2      
      IdGrMALP    = 3      
      IdGrMALC    = 4   
      IdDecMALS   = 5  
      IdPrPOC1M   = 6  
      IdPrPOC2M   = 7  
      IdPrPON1M   = 8  
      IdPrPON2M   = 9  
      IdPrPOP1M   = 10  
      IdPrPOP2M   = 11 
      IdUpMALN    = 12
      IdUpNO3M    = 13        
      IdUpNH4M    = 14  
      IdUpMALP    = 15
      IdUpPO4M    = 16 
      IdUpMALC    = 17
      IdPrMALC    = 18           
      IdExMALC    = 19              
      IdRspMALC   = 20      
      IdCanMALS   = 21       
      IdCanCMALS  = 22               
      IdCanNMALS  = 23            
      IdCanPMALS  = 24          
      IdHrvMALS   = 25           
      IdHrvMALN   = 26             
      IdHrvMALP   = 27          
      IdHrvMALC   = 28                             
!*******************************************************************************
      ! do all segments
      DO ISEG = 1 , NOSEG
         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
         ! if active
         IF (IKMRK1.EQ.1) THEN
            CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
            
            DOMALG = 0
            MBotSeg    = nint(PMSA( IPNT(10) ))
            
            IF (MBotSeg .ge. 0) THEN  
                FrBmMALS   = PMSA( IPNT(8) )
                IF (FrBmMALS > 0.0) THEN
                    DOMALG = 1
                    
                    ! need to take from bottom segment
                    MALS       = PMSA( IPNT(1)+(MBotSeg-ISEG)*INCREM(1) )
                    MALN       = PMSA( IPNT(2)+(MBotSeg-ISEG)*INCREM(2) )
                    MALP       = PMSA( IPNT(3)+(MBotSeg-ISEG)*INCREM(3) )
                    MALC       = PMSA( IPNT(4)+(MBotSeg-ISEG)*INCREM(4) )   
                    TotAreMAL  = PMSA( IPNT(9) )           
                    ! input 11, comes from output 74 in initialization step
                    ! take the Nfrond value stored in the output of the bottom segment of this column
                    NFrond     = PMSA(IPNT(11)+(MBotSeg-ISEG)*INCREM(11) )
                ENDIF
            ELSE
                FrBmMALS = 1.0
                MALS = PMSA( IPNT(1) )
                
                IF (MALS > 0.0) THEN
                    DOMALG = 1
                    
                    ! load the rest
                    MALS       = PMSA( IPNT(1) )
                    MALN       = PMSA( IPNT(2) )
                    MALP       = PMSA( IPNT(3) )
                    MALC       = PMSA( IPNT(4) )              
                    ! input 11, comes from output 74 in initialization step
                    NFrond     = PMSA(IPNT(11) )
                    
                    ! frond area calculated from dry weight to area ratio
                    ArDenMAL   = PMSA(IPNT(62) )
                    TotAreMAL  = MALS / ArDenMAL
                ENDIF
            ENDIF
            
            IF (DOMALG > 0) THEN
                
                IF (NFrond .LT. 1.0) THEN
                  write(*,*) 'ERROR: NFrond<1.0 in seg with !=0 biomass'
                  write(*,*) 'Segment = ' , ISEG
                ENDIF

                ! take from this segment                
                NO3        = PMSA( IPNT(5) )    
                NH4        = PMSA( IPNT(6) )    
                PO4        = PMSA( IPNT(7) )    
                !FrBmMALS   = PMSA( IPNT(8) )  
                !TotAreMAL  = PMSA( IPNT(9) )
                !MBotSeg    = nint(PMSA( IPNT(10) ))
                !NFrond     = PMSA( IPNT(11) )   
                Surf       = PMSA( IPNT(12) )    
                Depth      = PMSA( IPNT(13) )   
                LocalDepth = PMSA( IPNT(14) )
                Temp       = PMSA( IPNT(15) )     
                Velocity   = PMSA( IPNT(16) ) 
                RadSurf    = PMSA( IPNT(17) )  
                ExtVl      = PMSA( IPNT(18) )   
                daylend    = PMSA( IPNT(19) ) 
                daylenp    = PMSA( IPNT(20) )  
                daylenm    = PMSA( IPNT(21) ) 
                DELT       = PMSA( IPNT(22) )     
                !SeedMass   = PMSA( IPNT(23) ) 
                DoLocUpP   = PMSA( IPNT(24) )
                MALNmin    = PMSA( IPNT(25) )
                MALNmax    = PMSA( IPNT(26) ) 
                MALPmin    = PMSA( IPNT(27) )  
                MALPmax    = PMSA( IPNT(28) )  
                MALCmin    = PMSA( IPNT(29) ) 
                MALCmax    = PMSA( IPNT(30) ) 
                CDRatMALS  = PMSA( IPNT(31) )
                NCRatMALS  = PMSA( IPNT(32) )
                PCRatMALS  = PMSA( IPNT(33) )
                Kn         = PMSA( IPNT(34) )   
                Kp         = PMSA( IPNT(35) )   
                Kc         = PMSA( IPNT(36) )    
                Kdw        = PMSA( IPNT(37) )   
                mumax      = PMSA( IPNT(38) )   
                m1         = PMSA( IPNT(39) )     
                m2         = PMSA( IPNT(40) )     
                MALS0      = PMSA( IPNT(41) )    
                a1         = PMSA( IPNT(42) )      
                a2         = PMSA( IPNT(43) )     
                epsBroch   = PMSA( IPNT(44) )
                mrtMAL     = PMSA( IPNT(45) )    
                FrPOC1MAL  = PMSA( IPNT(46) ) 
                FrPOC2MAL  = PMSA( IPNT(47) )  
                Ksn        = PMSA( IPNT(48) )     
                Ksp        = PMSA( IPNT(49) )     
                JNmax      = PMSA( IPNT(50) )    
                JPmax      = PMSA( IPNT(51) )  
                Vel65      = PMSA( IPNT(52) )  
                NH4Thresh  = PMSA( IPNT(53) ) 
                alpha0     = PMSA( IPNT(54) )  
                Isat       = PMSA( IPNT(55) )  
                exuMALC    = PMSA( IPNT(56) )  
                R1         = PMSA( IPNT(57) )   
                Tr1        = PMSA( IPNT(58) )   
                Tar        = PMSA( IPNT(59) )    
                K0HrvMALS  = PMSA( IPNT(60) )
                K1HrvMALS  = PMSA( IPNT(61) )
                !ArDenMAL
                BrochMeth  = PMSA( IPNT(63) )
                qTmu       = PMSA( IPNT(64) )
                Trefmu     = PMSA( IPNT(65) )
                DoLocUpC   = PMSA( IPNT(66) )
                NH4pref    = PMSA( IPNT(67) )
                JNO3fact   = PMSA( IPNT(68) )
                Ksno3      = PMSA( IPNT(69) )
                
                ! need to convert storage substance from gX/m2 to gX/gDM
                ! to be consistent with constants from Broch
                ! gX/m2 to gX/gDM
                MALN = MALN / MALS ! gN/m2 to gN/gDM
                MALP = MALP / MALS ! gP/m2 to gP/gDM
                MALC = MALC / MALS ! gC/m2 to gC/gDM
                
                ! state variables as per Broch
                ! valid with assumption that storage content is homogeneous
                MALNDMS = MALN
                MALPDMS = MALP
                MALCDMS = MALC
                            
                ! find amount of mass in this segment (gDM/m2)
                MALS = MALS * FrBmMALS 

                ! Dry weight and wet weight
                Wdry = MALS * (1 + Kn*MALN + Kp*MALP + Kc*MALC)
                Wwet = MALS * (1/Kdw + Kn*MALN + Kp*MALP + Kc*MALC)
                
                WdryTot = Wdry * Surf
                WwetTot = Wdry * Surf
                
                ! Nutrient content
                TotN = MALS*(MALN + CDRatMALS*NCRatMALS)
                TotP = MALS*(MALP + CDRatMALS*PCRatMALS)
                TotC = MALS*(MALC + CDRatMALS)
                
                ! N:C ratio
                MALSNC = TotN/TotC
                ! P:C ratio
                MALSPC = TotP/TotC
                ! C:DM ratio
                MALSCDM = TotC/Wdry
                ! N:DM ratio
                MALSNDM = TotN/Wdry     
                ! P:DM ratio
                MALSPDM = TotP/Wdry

                ! Specarea is the specific area of a frond, the whole frond across the column
                SpecArea = TotAreMAL / NFrond
                
                ! ---------------- GROWTH ---------------- !
                ! density limitation - "if the plant is too big it will grow slower"
                ! area is in m2 and MALS0 is in m2 as well
                IF ((BrochMeth .gt. 0) .AND. (MALS0 .gt. 0.0)) THEN
                    LimDen = density_limitation_Broch(SpecArea, MALS0, &
                                                      m1, m2)
                ELSE
                    LimDen = 1.0
                ENDIF

                ! temperature limitation                
                IF (BrochMeth .gt. 0) THEN
                    LimTemp = temp_limitation_Broch(Temp)
                ELSE
                    LimTemp = temp_limitation_Arrhe(Temp, qTmu, Trefmu)
                ENDIF
                    
                ! photoperiod limitation
                IF (BrochMeth .gt. 0) THEN
                    LimPho = photo_limitation_Broch(daylend, daylenp, &
                                                    daylenm, a1, a2)
                ENDIF
                
                ! if C storage is not modelled, growth is directly limited by potential photosynthesis
                Ilay = radiation_layer(RadSurf, ExtVl, Depth, LocalDepth)
                
                IF (DoLocUpC .le. 0) THEN
                    LimPho = photo_limitation_noMALC(Ilay, Isat)
                ENDIF              

                ! storage limitations
                IF (MALN .lt. MALNmin) THEN
                    write(*,*) 'ERROR: MALN (gN/gDM) LESS THAN MALNmin'
                ENDIF
                LimMALN = MAX(1-(MALNmin/MALN), 0.0)

                IF (DoLocUpC .gt. 0.0) THEN
                    IF (MALC .lt. MALCmin) THEN
                        write(*,*) 'ERROR: MALC (gC/gDM) LESS THAN MALCmin'
                    ENDIF           
                    LimMALC = MAX(1-(MALCmin/MALC), 0.0)
                ELSE
                    LimMALC = 1.0
                ENDIF

                IF (DoLocUpP .gt. 0.0) THEN
                    IF (MALP .lt. MALPmin) THEN
                        write(*,*) 'ERROR: MALP (gP/gDM) LESS THAN MALPmin'
                    ENDIF           
                    LimMALP = MAX(1-(MALPmin/MALP), 0.0)
                ELSE
                    ! no P limitation
                    LimMALP = 1.0
                ENDIF
                
                LimNut = MIN(LimMALN,LimMALC,LimMALP)
                
                ! growth rate
                muMALS = MAX(mumax * LimDen * LimPho * LimTemp * LimNut, 0.0)
                
                ! Gross growth and corresponding storage utilization
                LocGroPS = muMALS * MALS
                LocGroN = LocGroPS * CDRatMALS * NCRatMALS 
                LocGroP = LocGroPS * CDRatMALS * PCRatMALS !DO something here
                LocGroC = LocGroPS * CDRatMALS !DO something here
                dGrMALS = LocGroPS / Depth
                dGrMALN = LocGroN / Depth
                dGrMALP = LocGroP / Depth !DO something here
                IF (DoLocUpC > 0.0) THEN
                    dGrMALC = LocGroC / Depth !DO something here
                ELSE
                    dGrMALC = 0.0
                ENDIF
                
                ! ---------------- DECAY ---------------- !
                ! mortality rate
                IF (epsBroch .gt. 0.0) THEN
                    mrtMALS = mortality_rate_Broch(SpecArea, epsBroch)
                ELSE
                    mrtMALS = mrtMAL
                ENDIF
                
                ! Mortality flux
                LocDecS = mrtMALS * MALS
                LocNetPS = LocGroPS - LocDecS
                
                dDecMALS = LocDecS / Depth
                dPrPOC1M = (dDecMALS * CDRatMALS) * FrPOC1MAL
                dPrPOC2M = (dDecMALS * CDRatMALS) * FrPOC2MAL
                dPrPON1M = (dDecMALS * CDRatMALS * NCRatMALS) * FrPOC1MAL 
                dPrPON2M = (dDecMALS * CDRatMALS * NCRatMALS) * FrPOC2MAL 
                IF (DoLocUpP > 0.0) THEN
                    ! In old version, this flux was calculated even if there was no PO4 uptake
                    ! Leads to source of P in the model...
                    dPrPOP1M = (dDecMALS * CDRatMALS * PCRatMALS) * FrPOC1MAL  
                    dPrPOP2M = (dDecMALS * CDRatMALS * PCRatMALS) * FrPOC2MAL  
                ELSE
                    ! for now no uptake PO4 so no release of POP
                    ! effect of seaweed on P cycle totally neglected
                    dPrPOP1M = 0.0 
                    dPrPOP2M = 0.0
                ENDIF
                
                ! -------- EXT. NUTRIENT UPTAKE -------- !
                ! Nutrient uptake from ambient water to consitute storage pools
                
                ! velocity limitation
                IF (Vel65.gt.0.0) THEN
                    LimVel = 1.0 - exp(-Velocity/Vel65)
                ELSE
                    LimVel = 1.0
                ENDIF

                ! nitrogen uptake
                IF (MALN .gt. MALNmax) THEN
                    write(*,*) 'ERROR: MALN (gN/gDM) MORE THAN MALNmax'
                    write(*,*) 'In segment' , ISEG
                    write(*,*) 'MALN = ' , MALN , 'MALNmax = ', MALNmax
                ENDIF

                LimN = MAX((MALNmax - MALN)/(MALNmax - MALNmin), 0.0)

                IF (MALN .lt. MALNmax) THEN 
                    IF (NH4pref .gt. 0.0) THEN
                        LocUpNH4 = (TotAreMAL*FrBmMALS) * JNmax * &
                                    (NH4/(Ksn + NH4)) 
                        LocUpNO3 = (TotAreMAL*FrBmMALS) * JNmax * JNO3fact * &
                                    (NO3/(Ksno3 + NO3)) 
                        IF ((LocUpNH4+LocUpNO3) .gt. 0.0) THEN
                            FrNH4MALN = LocUpNH4/(LocUpNH4+LocUpNO3)
                        ELSE
                            FrNH4MALN = 0.0
                        ENDIF
                        LocUpN = (LocUpNH4+LocUpNO3)  * LimN * LimVel     
                    ELSE
                        IF ((NH4thresh .gt. 0.0) .AND. (NH4 .gt. NH4Thresh)) THEN
                            FrNH4MALN = 1.0 ! equivalent to older version of MALG
                            Effnit = NH4
                        ELSE    
                            FrNH4MALN = NH4/(NH4 + NO3)
                            EffNit = NH4+NO3
                        ENDIF
                        LocUpN = (TotAreMAL*FrBmMALS) * JNmax * &
                                 (EffNit/(Ksn + EffNit))  * LimN * LimVel 
                    ENDIF
                ELSE
                    LocUpN = 0.0
                ENDIF          

                ! phosphorus uptake
                IF (DoLocUpP .gt. 0.0) THEN
                    IF (MALP .gt. MALPmax) THEN
                        write(*,*) &
                            'ERROR: MALP (gP/gDM) MORE THAN MALPmax'
                    ENDIF            
                
                    LimP = MAX((MALPmax - MALP)/(MALPmax - MALPmin), 0.0)
    
                    IF (MALP .lt. MALPmax) THEN
                        LocUpP = (TotAreMAL*FrBmMALS) * JPmax * &
                                 (PO4/(Ksp + PO4))  * LimP * LimVel
                    ELSE
                        LocUpP = 0.0
                    ENDIF                    
                ELSE
                    LocUpP = 0.0          
                ENDIF

                ! output per surface area of bottom
                LocUpN = LocUpN / Surf
                LocUpP = LocUpP / Surf
                
                dUpMALN = LocUpN / Depth
                dUpNH4M = FrNH4MALN * dUpMALN 
                dUpNO3M = (1.0-FrNH4MALN) * dUpMALN 
                dUpMALP = LocUpP / Depth
                dUpPO4M = dUpMALP
                
                ! --------- PRIMARY PRODUCTION --------- !
                ! Carbon storage dynamics: photosynthesis and respiration  
                ! gross photosynthesis
                ! integrate the radiation decay function between z2 (local depth, bottom)
                ! and m1 (Localdepth - segment depth, top)
                ! Radiation at top is RadSurf
                !Ilay = -RadSurf/(ExtVl * Depth) * (exp(-ExtVl * LocalDepth) &
                ! - exp(-ExtVl * (LocalDepth - Depth)))                    
                
                IF (DoLocUpC > 0.0) THEN
                    ! need to convert to correct units
                    ! 1 W/m2 = 4.57 umol photons m-2 s-1 
                    ! assumption is data supplied consistent with saturation value
                    Ilayu = Ilay * 4.57 ! umol/m2s
                    Isat = Isat * 4.57 ! umol/m2s
                
                    beta = beta_Broch(Temp)
                    P = P_Broch(Ilayu, Isat, alpha0, beta)
                    GrosMALC = TotAreMAL * FrBmMALS * P / Surf
                    dPrMALC = GrosMALC / Depth
                
                    ! exudation fraction
                    E = 1.0 - exp(exuMALC*(MALCmin - MALC))
                    ExudMALC = E * GrosMALC
                    dExMALC = ExudMALC / Depth
                
                    ! respiration 
                    R = R_Broch(Temp, R1, Tar, Tr1) 
                
                    ! If MALC is depleted, respiration still occurs, but uses structural mass
                    ! cannibalization
                
                    IF ((P * (1.0-E) - R) .lt. 0.0 &
                        .and. MALC .le. MALCmin) THEN  
                        RespMALC = 0.0
                        RespMALS = TotAreMAL * FrBmMALS * R / (CDRatMALS * Surf) 
                    ELSE
                        RespMALC = TotAreMAL * FrBmMALS * R / Surf
                        RespMALS = 0.0
                    ENDIF
         
                    ! local uptake of C for storage
                    LocUpC = GrosMALC - ExudMALC - RespMALC
                    dUpMALC = LocUpC / Depth                
                    dRspMALC = RespMALC / Depth
                    dCanMALS = RespMALS / Depth 
                    dCanCMALS = RespMALS * CDRatMALS / Depth
                    dCanNMALS =  RespMALS * CDRatMALS * NCRatMALS / Depth
                    dCanPMALS = RespMALS * CDRatMALS * PCRatMALS / Depth
                ELSE
                    dPrMALC = dGrMALS * CDRatMALS
                    dExMALC = 0.0
                    LocUpC = 0.0
                    dUpMALC = 0.0             
                    dRspMALC = 0.0
                    dCanMALS = 0.0
                    dCanCMALS = 0.0
                    dCanNMALS = 0.0
                    dCanPMALS = 0.0
                ENDIF                    
                
                ! -------------- HARVEST --------------- !
                !HrvMALS = MIN(MALS / DELT, K0HrvMALS + MALS * K1HrvMALS)
                IF (K1HrvMALS > 0.000001) THEN
                    HrvMALS = MAX(0., (MALS - 0.037) / DELT)
                ELSE
                    HrvMALS = 0.0
                ENDIF
               
                dHrvMALS = HrvMALS / Depth       
                dHrvMALN = MALN * HrvMALS / Depth        
                dHrvMALP = MALP * HrvMALS / Depth       
                dHrvMALC = MALC * HrvMALS / Depth            
                
                ! fill DELWAQ fluxes array
                FL ( IdPrPOC1M )  = dPrPOC1M 
                FL ( IdPrPOC2M )  = dPrPOC2M 
                FL ( IdPrPON1M )  = dPrPON1M 
                FL ( IdPrPON2M )  = dPrPON2M 
                FL ( IdPrPOP1M )  = dPrPOP1M 
                FL ( IdPrPOP2M )  = dPrPOP2M 
                FL ( IdUpNO3M )   = dUpNO3M
                FL ( IdUpNH4M )   = dUpNH4M 
                FL ( IdUpPO4M )   = dUpPO4M
                FL ( IdPrMALC )   = dPrMALC 
                FL ( IdExMALC )   = dExMALC
                FL ( IdRspMALC )  = dRspMALC 
                FL ( IdCanCMALS ) = dCanCMALS 
                FL ( IdCanNMALS ) = dCanNMALS 
                FL ( IdCanPMALS ) = dCanPMALS
                
                ! allocate to bottom segment flux address
                IF (MBotSeg .ge. 0) THEN
                    FLCREM = (MBotSeg-ISEG)*NOFLUX
                ELSE
                    FLCREM = 0
                ENDIF
                FL(IdGrMALS + FLCREM)  = FL(IdGrMALS + FLCREM)   + dGrMALS
                FL(IdGrMALN + FLCREM)  = FL(IdGrMALN + FLCREM )  + dGrMALN
                FL(IdGrMALP + FLCREM)  = FL(IdGrMALP + FLCREM)   + dGrMALP
                FL(IdGrMALC + FLCREM)  = FL(IdGrMALC + FLCREM)   + dGrMALC
                FL(IdDecMALS + FLCREM) = FL(IdDecMALS + FLCREM)  + dDecMALS
                FL(IdUpMALN + FLCREM)  = FL(IdUpMALN + FLCREM )  + dUpMALN
                FL(IdUpMALP + FLCREM)  = FL(IdUpMALP + FLCREM)   + dUpMALP
                FL(IdUpMALC + FLCREM)  = FL(IdUpMALC + FLCREM)   + dUpMALC
                FL(IdCanMALS + FLCREM) = FL(IdCanMALS + FLCREM)  + dCanMALS
                FL(IdHrvMALS + FLCREM) = FL(IdHrvMALS + FLCREM ) + dHrvMALS
                FL(IdHrvMALN + FLCREM) = FL(IdHrvMALN + FLCREM)  + dHrvMALN
                FL(IdHrvMALP + FLCREM) = FL(IdHrvMALP + FLCREM)  + dHrvMALP
                FL(IdHrvMALC + FLCREM) = FL(IdHrvMALC + FLCREM)  + dHrvMALC
                
                ! store outputs in PMSA
                PMSA( IPNT( 70)   ) =  MALNDMS      
                PMSA( IPNT( 71)   ) =  MALPDMS      
                PMSA( IPNT( 72)   ) =  MALCDMS      
                PMSA( IPNT( 73)   ) =  MALSNC       
                PMSA( IPNT( 74)   ) =  MALSPC   
                PMSA( IPNT( 75)   ) =  MALSCDM
                PMSA( IPNT( 76)   ) =  MALSNDM
                PMSA( IPNT( 77)   ) =  MALSPDM
                PMSA( IPNT( 78)   ) =  Wdry
                PMSA( IPNT( 79)   ) =  Wwet
                PMSA( IPNT( 80)   ) =  WdryTot
                PMSA( IPNT( 81)   ) =  WwetTot
                !PMSA( IPNT( 82)   ) =  NFrond
                PMSA( IPNT( 83)   ) =  SpecArea*100
                PMSA( IPNT( 84)   ) =  LimDen
                PMSA( IPNT( 85)   ) =  LimPho  
                PMSA( IPNT( 86)   ) =  LimTemp 
                PMSA( IPNT( 87)   ) =  LimMALN  
                PMSA( IPNT( 88)   ) =  LimMALP  
                PMSA( IPNT( 89)   ) =  LimMALC   
                PMSA( IPNT( 90)   ) =  LimNut  
                PMSA( IPNT( 91)   ) =  muMALS
                PMSA( IPNT( 92)   ) =  mrtMALS    
                PMSA( IPNT( 93)   ) =  LocGroPS     
                PMSA( IPNT( 94)   ) =  LocNetPS
                PMSA( IPNT( 95)   ) =  LocDecS
                PMSA( IPNT( 96)   ) =  LocGroN   
                PMSA( IPNT( 97)   ) =  LocGroP   
                PMSA( IPNT( 98)   ) =  LocGroC
                PMSA( IPNT( 99)   ) =  LimVel  
                PMSA( IPNT(100)   ) =  LimN    
                PMSA( IPNT(101)   ) =  LimP
                PMSA( IPNT(102)   ) =  LocUpN   
                PMSA( IPNT(103)   ) =  LocUpP
                PMSA( IPNT(104)   ) =  FrNH4MALN  
                PMSA( IPNT(105)   ) =  LocUpC    
                PMSA( IPNT(106)   ) =  beta
                PMSA( IPNT(107)   ) =  Ilay
                PMSA( IPNT(108)   ) =  Ilayu    
                PMSA( IPNT(109)   ) =  GrosMALC    
                PMSA( IPNT(110)   ) =  RespMALC
                PMSA( IPNT(111)   ) =  ExudMALC
                PMSA( IPNT(112)   ) =  RespMALS    
                PMSA( IPNT(113)   ) =  HrvMALS    
                
            ELSE
                PMSA( IPNT( 70)   ) =  0.0      
                PMSA( IPNT( 71)   ) =  0.0      
                PMSA( IPNT( 72)   ) =  0.0      
                PMSA( IPNT( 73)   ) =  0.0      
                PMSA( IPNT( 74)   ) =  0.0  
                PMSA( IPNT( 75)   ) =  0.0
                PMSA( IPNT( 76)   ) =  0.0
                PMSA( IPNT( 77)   ) =  0.0
                PMSA( IPNT( 78)   ) =  0.0
                PMSA( IPNT( 79)   ) =  0.0
                PMSA( IPNT( 80)   ) =  0.0
                PMSA( IPNT( 81)   ) =  0.0
                !PMSA( IPNT( 82)   ) =  0.0
                PMSA( IPNT( 83)   ) =  0.0
                PMSA( IPNT( 84)   ) =  0.0
                PMSA( IPNT( 85)   ) =  0.0  
                PMSA( IPNT( 86)   ) =  0.0 
                PMSA( IPNT( 87)   ) =  0.0  
                PMSA( IPNT( 88)   ) =  0.0  
                PMSA( IPNT( 89)   ) =  0.0   
                PMSA( IPNT( 90)   ) =  0.0  
                PMSA( IPNT( 91)   ) =  0.0
                PMSA( IPNT( 92)   ) =  0.0    
                PMSA( IPNT( 93)   ) =  0.0     
                PMSA( IPNT( 94)   ) =  0.0
                PMSA( IPNT( 95)   ) =  0.0
                PMSA( IPNT( 96)   ) =  0.0   
                PMSA( IPNT( 97)   ) =  0.0   
                PMSA( IPNT( 98)   ) =  0.0
                PMSA( IPNT( 99)   ) =  0.0  
                PMSA( IPNT(100)   ) =  0.0    
                PMSA( IPNT(101)   ) =  0.0
                PMSA( IPNT(102)   ) =  0.0   
                PMSA( IPNT(103)   ) =  0.0
                PMSA( IPNT(104)   ) =  0.0  
                PMSA( IPNT(105)   ) =  0.0    
                PMSA( IPNT(106)   ) =  0.0
                PMSA( IPNT(107)   ) =  0.0
                PMSA( IPNT(108)   ) =  0.0    
                PMSA( IPNT(109)   ) =  0.0    
                PMSA( IPNT(110)   ) =  0.0
                PMSA( IPNT(111)   ) =  0.0
                PMSA( IPNT(112)   ) =  0.0    
                PMSA( IPNT(113)   ) =  0.0                    
            ENDIF !if fraction MALS >0
         ENDIF !if active segment

         IdGrMALS   = IdGrMALS   + NOFLUX    
         IdGrMALN   = IdGrMALN   + NOFLUX
         IdGrMALP   = IdGrMALP   + NOFLUX
         IdGrMALC   = IdGrMALC   + NOFLUX
         IdDecMALS  = IdDecMALS  + NOFLUX
         IdPrPOC1M  = IdPrPOC1M  + NOFLUX
         IdPrPOC2M  = IdPrPOC2M  + NOFLUX
         IdPrPON1M  = IdPrPON1M  + NOFLUX
         IdPrPON2M  = IdPrPON2M  + NOFLUX
         IdPrPOP1M  = IdPrPOP1M  + NOFLUX
         IdPrPOP2M  = IdPrPOP2M  + NOFLUX
         IdUpMALN   = IdUpMALN   + NOFLUX                                       
         IdUpNO3M   = IdUpNO3M   + NOFLUX
         IdUpNH4M   = IdUpNH4M   + NOFLUX
         IdUpMALP   = IdUpMALP   + NOFLUX                                  
         IdUpPO4M   = IdUpPO4M   + NOFLUX          
         IdUpMALC   = IdUpMALC   + NOFLUX                                          
         IdPrMALC   = IdPrMALC   + NOFLUX       
         IdExMALC   = IdExMALC   + NOFLUX      
         IdRspMALC  = IdRspMALC  + NOFLUX   
         IdCanMALS  = IdCanMALS  + NOFLUX 
         IdCanCMALS = IdCanCMALS + NOFLUX         
         IdCanNMALS = IdCanNMALS + NOFLUX         
         IdCanPMALS = IdCanPMALS + NOFLUX      
         IdHrvMALS  = IdHrvMALS  + NOFLUX       
         IdHrvMALN  = IdHrvMALN  + NOFLUX          
         IdHrvMALP  = IdHrvMALP  + NOFLUX      
         IdHrvMALC  = IdHrvMALC  + NOFLUX      
               
         IPNT       = IPNT       + INCREM

      ENDDO

      RETURN
      END

