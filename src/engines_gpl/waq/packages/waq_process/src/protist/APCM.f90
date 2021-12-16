    ! 6 char name for process mathc with second line of PDF  
subroutine ACQPCM     ( pmsa   , fl     , ipoint , increm, noseg , &                            
                            noflux , iexpnt , iknmrk , noq1  , noq2  , &                            
                            noq3   , noq4   )     
!                                                                                                     
!*******************************************************************************                      
!                                                                                                     
    IMPLICIT NONE                                                                                   
!                                                                                                     
!     Type    Name         I/O Description                                                            
!          
    integer, parameter :: plen = 593 ! total length of the PMSA input and output array
    real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library     
    real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time               
    integer ipoint(plen) ! I  Array of pointers in pmsa to get and store the data                    
    integer increm(plen) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying 
    integer noseg       ! I  Number of computational elements in the whole model schematisation     
    integer noflux      ! I  Number of fluxes, increment in the fl array                            
    integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces     
    integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use              
    integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
    integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid  
    integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward    
    integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)     
!                                                                                                     
!*******************************************************************************                      
!                                                                                                     
!     Type    Name         I/O Description                                        Unit                
!                                                                                                     
!     support variables
    integer ipnt(plen)     ! Local work array for the pointering                                    
    integer iseg          ! Local loop counter for computational element loop                      
    integer ioq
    integer iflux   
    integer ikmrk1        ! first segment attribute
    
    real(8),  parameter :: PI_8  = 4 * atan (1.0_8)
    !integer, parameter :: maxNrSp = 4 ! maximum number pf species implemented
    integer maxNrSp != 4 ! maximum number pf species implemented
      
    integer iPred         ! local species number counter
    integer iPrey         ! local species number counter
    !integer ispec         ! local species number counter
    integer spInc         ! local species PMSA number counter
    integer inpItems      ! nr of input items need for output PMSA
      
    ! divide the following into:
    ! species independent input and species dependent input
    !add comments to the constants
    
    !input
     integer    nrSp        ! nr of species to be modelled    
     integer    nrSpCon     ! nr of species dependent items   
     integer    nrInd       ! nr of species independent items     
     real    APaDOC      
     real    APaNH4      
     real    APaNO3      
     real    APaP        
     real    AR                
     real    betaSi      
     real    CR          
     real    HdDOC       
     real    HdN         
     real    HdP         
     real    HiDOC       
     real    HiN         
     real    HiP         
     real    HSi         
     real    Hv          
     real    KdDOC       
     real    KdN         
     real    KdP         
     real    KiDOC       
     real    KiN         
     real    KiP         
     real    KQN         
     real    KQP         
     real    KSi         
     real    Kv          
     real    Mphoto      
     real    Q10         
     real    redco       
     real    wTurb       
     real    DOC         
     real    PO4                 
     real    NH4         
     real    NO3         
     real    Si          
     real    Rad         
     real    Temp        
     real    ExtVl       
     real    Depth 
     
     real    protC     
     real    protChl    
     real    protN      
     real    protP      
     real    protSi     
     real    avgCu      
     real    avgnetPS   
     real    AEm        
     real    AEo        
     real    alpha      
     real    CcellPro   
     real    ChlCm      
     real    ChlCo  
     real    FrAut
     real    FrDet
     real    kAE        
     real    KtDOC      
     real    KtNH4      
     real    KtNO3      
     real    KtP        
     real    KtSi
     real    MrtRT
     real    NCm        
     real    NCo        
     real    NCopt      
     real    NO3Cm      
     real    NO3Copt    
     real    Opti_CR     
     real    PCm        
     real    PCo        
     real    PCoNCm     
     real    PCoNCopt   
     real    PCopt      
     real    PSDOC      
     real    RemiUmPS   
     real    RePSm      
     real    ReUmNH4    
     real    ReUmNO3    
     real    ReUmPS     
     real    rProt     
     real    RT         
     real    SDA        
     real    SiCm       
     real    SiCopt     
     real    SiiCo      
     real    sw_diat    
     real    UmRT  
     real    PR
     
     !auxiliaries
     real    PARRAD                    
     real    PFD                       
     real    atten                     
     real    exat                      
     real    NC
     real    PC
     real    SC
     real    ChlC
     real    UmT
     real    BR     
     real    Nregen
     real    Pregen
     real    NCu
     real    PCu
     real    VSi
     real    SCu
     real    NPCu
     real    NPSiCu
     real    nPCopt
     real    nPC
     real    LHP
     real    HLP
     real    APoptP
     real    APP
     real    NH4CPopt
     real    NH4CPm
     real    nNH4Copt
     real    nNH4C
     real    HLNH4
     real    LHNH4
     real    APoptNH4
     real    APNH4
     real    NO3CPopt
     real    NO3CPm
     real    nNO3Copt
     real    nNO3C
     real    HLNO3
     real    LHNO3
     real    APoptNO3
     real    APNO3
     real    nNCopt
     real    nNC
     real    HHDOC
     real    LLDOC
     real    APoptDOC
     real    APDOC
     real    upP
     real    upNH4
     real    upNO3
     real    HetMax
     real    APDOCm
     real    relU
     real    satCon
     real    upDOC
     real    frat
     real    minPhotUm
     real    PhotUm
     real    addPhotUm
     real    PSqm
     real    Pyt
     real    PS
     real    Cfix
     real    dChl
     real    photoR
     real    phoTro
        
     real    sumC 
     real    preyC
     real    preyChl
     real    preyN
     real    preyP
     real    preySi
     real    avgCu_prey                   
     real    r_prey        
     real    sw_diat_prey  
     real    UmRT_prey              
     real    relU_prey
     real    satCon_prey
     real    Ccell_prey  
     real    UmT_prey
     real    v_Prot
     real    v_prey
     real    nos_prey
     real    Enc_prey
     real    CR_prey
     real, dimension(:), allocatable :: CRCP_prey
     real, dimension(:), allocatable :: propIPrey
     real    CRCP_sum
     
     real    ingNC
     real    ingPC
     real    stoich_con
     real    AEqual
     real    opAEC
     real    Igmax
     real    APoptpred
     real    Igmop
     real    KI
     real    ingC_Prot
     real    ingN_Prot
     real    ingP_Prot
     real    assC_Prot
     real    assN_Prot
     real    assP_Prot
     real    SDAN
     real    totR
     real    Cu
     real    upSi
     real    hetTro
     real    Cgro
     real    mrt
     real    mrtFrAut
     real    mrtFrDet
     real    clT
     
     !Fluxes
     real    dPS      
     real    dCu      
     real    dCeat    
     real    dNeat    
     real    dPeat    
     real    dNH4gro  
     real    dNO3gro  
     real    dPgro    
     real    dSigro   
     real    dDOCgro  
     real    dPSgro   
     real    dChlgro  
     real    dCresp   
     real    dDOCleak     
     real    dDOCvoid
     real    dPout
     real    dNH4out
     real    dVOCout  
     real    dVONout  
     real    dVOPout  
     real    dAutC  
     real    dDetC  
     real    dAutN  
     real    dDetN  
     real    dAutP  
     real    dDetP  
     real    dAutSi 
     real    dDetSi 
     real    dAutChl
     real    dDetChl
         
     real    dPreyC  
     real    dPreyChl
     real    dPreyN  
     real    dPreyP  
     real    dPreySi
      
!                                                                                                     
!******************************************************************************* 
!                                                                                                     
    ipnt        = ipoint
           
    iflux = 0
    maxNrSp = 4
    
    !!segment and species independent items
    nrSp      = PMSA(ipnt(   1 ))   !   nr of species to be modelled                           (dl)                
    nrSpCon   = PMSA(ipnt(   2 ))   !   nr of species dependent items                          (dl)                
    nrInd     = PMSA(ipnt(   3 ))   !   nr of species independent items                        (dl)  
    APaDOC    = PMSA(ipnt(   4 ))   !   add. max. acquisition multipler of C on upturn         (dl)
    APaNH4    = PMSA(ipnt(   5 ))   !   add. max. acquisition multipler of NH4 on upturn       (dl)
    APaNO3    = PMSA(ipnt(   6 ))   !   add. max. acquisition multipler of NO3 on upturn       (dl)
    APaP      = PMSA(ipnt(   7 ))   !   add. max. acquisition multipler of DIP on upturn       (dl)
    AR        = PMSA(ipnt(   8 ))   !   anabolic respiration cost in terms of C                (gC gN-1 d-1)
    betaSi    = PMSA(ipnt(   9 ))   !   control for Si (diatom) uptake                         (dl)
    CR        = PMSA(ipnt(  10 ))   !   catabolic respiration quotient                         (dl)
    HdDOC     = PMSA(ipnt(  11 ))   !   H for decrease in C transport potential                (dl)
    HdN       = PMSA(ipnt(  12 ))   !   H for decrease in DIN transport potential              (dl)
    HdP       = PMSA(ipnt(  13 ))   !   H for decrease in DIP transport potential              (dl)
    HiDOC     = PMSA(ipnt(  14 ))   !   H for increase in C transport potential                (dl)
    HiN       = PMSA(ipnt(  15 ))   !   H for increase in DIN transport potential              (dl)
    HiP       = PMSA(ipnt(  16 ))   !   H for increase in DIP transport potential              (dl)
    HSi       = PMSA(ipnt(  17 ))   !   control for Si (diatom) uptake                         (dl)
    Hv        = PMSA(ipnt(  18 ))   !   Hill number for satiation-modulated motility           (dl)
    KdDOC     = PMSA(ipnt(  19 ))   !   K for decrease in C transport potential                (dl)
    KdN       = PMSA(ipnt(  20 ))   !   K for decrease in DIN transport potential              (dl)
    KdP       = PMSA(ipnt(  21 ))   !   K for decrease in DIP transport potential              (dl)
    KiDOC     = PMSA(ipnt(  22 ))   !   K for increase in C transport potential                (dl)
    KiN       = PMSA(ipnt(  23 ))   !   K for increase in DIN transport potential              (dl)
    KiP       = PMSA(ipnt(  24 ))   !   K for increase in DIP transport potential              (dl)
    KQN       = PMSA(ipnt(  25 ))   !   control constant of quota curve for N:C                (dl)
    KQP       = PMSA(ipnt(  26 ))   !   control constant of quota curve for P:C                (dl)
    KSi       = PMSA(ipnt(  27 ))   !   control for Si (diatom) uptake                         (dl)
    Kv        = PMSA(ipnt(  28 ))   !   K for controlling satiation-modulated motility         (dl)
    Mphoto    = PMSA(ipnt(  29 ))   !   scalar for controlling photoacclimation rate           (dl)
    RT        = PMSA(ipnt(  30 ))   !   reference temperature for UmRT                         (deg C)
    redco     = PMSA(ipnt(  31 ))   !   C respired to support nitrate reduction for NH4        (gC gN-1)
     
    allocate( CRCP_prey(nrSp) ) 
    allocate( propIPrey(nrSp) )
      
    !!length of the PMSA input array. 4 = the  maximum number of functional group implemented 
    !!in the proc_def file. Beaware of this if I ever implement more than four functional groups in the 
    !!proc_def PMSA input
    inpItems = maxNrSp * nrSpCon + nrInd
   
    ! segment loop
    do iseg = 1 , noseg
        call dhkmrk(1,iknmrk(iseg),ikmrk1)
        if (ikmrk1.eq.1) then
            
        !!species independent items
        PO4          = PMSA(ipnt(  32 ))  !    initial external DIP                                   (gP m-3)
        DOC          = PMSA(ipnt(  33 ))  !    initial DOC concentration                              (gN m-3)
        NH4          = PMSA(ipnt(  34 ))  !    initial external NH4                                   (gN m-3)
        NO3          = PMSA(ipnt(  35 ))  !    initial external NO3                                   (gN m-3)
        Si           = PMSA(ipnt(  36 ))  !    initial external Si                                    (gSi m-3)
        Rad          = PMSA(ipnt(  37 ))  !    irradiation at the segment upper-boundary              (W/m2)              
        Temp         = PMSA(ipnt(  38 ))  !    ambient water temperature                              (oC)                
        ExtVl        = PMSA(ipnt(  39 ))  !    total extinction coefficient visible light             (1/m)               
        Depth        = PMSA(ipnt(  40 ))  !    depth of segment                                       (m)
        wTurb        = PMSA(ipnt(  41 ))  !    turbulence in segment
           
        ! calculate light availability in segment
        ! added from the MAP2 fortran model
        PARRAD = RAD * 0.45 ! does the 0.45 really need to be hardcoded? I don't think so from RAD to PARRAD
        PFD = PARRAD * 4.57 ! from PARRAD to PFD
           
        !attenuation    MLD*(attco_W+abcoChl*algChl)    dl   attenuation of light by water and by phytoplankton Chl 
        atten = depth * ExtVL                           
        !exat   dl    -ve exponent of attenuation     
        exat = exp(-atten)
           
        ! segemnt dependent, but species independent output
        PMSA(ipnt( inpItems +  1 ))  =  PARRAD
        PMSA(ipnt( inpItems +  2 ))  =  PFD   
        PMSA(ipnt( inpItems +  3 ))  =  atten 
        PMSA(ipnt( inpItems +  4 ))  =  exat  
      
        ! species loop
        do iPred = 0, (nrSp-1)

            spInc = nrSpCon * iPred
               
            !!species dependetnt items
            !(number of species independent items + location of input item in vector + species loop)
            protC        = PMSA(ipnt( nrInd +  1 + spInc ))   !      C-biomass                                              (gC m-3)  
            protChl      = PMSA(ipnt( nrInd +  2 + spInc ))   !      Chl-biomass                                            (gChl m-3)   
            protN        = PMSA(ipnt( nrInd +  3 + spInc ))   !      N-biomass                                              (gN m-3)   
            protP        = PMSA(ipnt( nrInd +  4 + spInc ))   !      P-biomass                                              (gP m-3)   
            protSi       = PMSA(ipnt( nrInd +  5 + spInc ))   !      Si-biomass                                             (gSi m-3)   
            avgCu        = PMSA(ipnt( nrInd +  6 + spInc ))   !      Cu state variable for memory                           ()  
            avgnetPS     = PMSA(ipnt( nrInd +  7 + spInc ))   !      PS state variable for memory                           ()  
            AEm          = PMSA(ipnt( nrInd +  8 + spInc ))   !      maximum assimilation efficiency (AE)                   (dl)
            AEo          = PMSA(ipnt( nrInd +  9 + spInc ))   !      minimum AE                                             (dl)
            alpha        = PMSA(ipnt( nrInd + 10 + spInc ))   !      alpha for photosynthesis in protist                    ()
            CcellPro     = PMSA(ipnt( nrInd + 11 + spInc ))   !      C content of protist cell                              (pgC cell-1)
            ChlCm        = PMSA(ipnt( nrInd + 12 + spInc ))   !      maximum cellular Chl:C ratio                           (gChl gC-3)
            ChlCo        = PMSA(ipnt( nrInd + 13 + spInc ))   !      minimum Chl:C                                          (gChl gC-1)
            FrAut        = PMSA(ipnt( nrInd + 14 + spInc ))   !      fraction of mortality to autolysis                     (dl)       
            FrDet        = PMSA(ipnt( nrInd + 15 + spInc ))   !      fraction of mortality to detritus                      (dl)                
            kAE          = PMSA(ipnt( nrInd + 16 + spInc ))   !      Control of AE in response to prey quality              (dl)         
            KtDOC        = PMSA(ipnt( nrInd + 17 + spInc ))   !      Kt for transport of DOC                                (gC m-3)
            KtNH4        = PMSA(ipnt( nrInd + 18 + spInc ))   !      Kt for NH4 transport                                   (gN m-3)
            KtNO3        = PMSA(ipnt( nrInd + 19 + spInc ))   !      Kt for NO3 transport                                   (gN m-3)
            KtP          = PMSA(ipnt( nrInd + 20 + spInc ))   !      Kt for DIP transport                                   (gP m-3)
            KtSi         = PMSA(ipnt( nrInd + 21 + spInc ))   !      Kt for Si transport                                    (gSi m-3)
            MrtRT        = PMSA(ipnt( nrInd + 22 + spInc ))   !      mortality at reference temperature                     (dl)
            NCm          = PMSA(ipnt( nrInd + 23 + spInc ))   !      N:C that totally represses NH4 transport               (gN gC-1)
            NCo          = PMSA(ipnt( nrInd + 24 + spInc ))   !      minimum N-quota                                        (gN gC-1)
            NCopt        = PMSA(ipnt( nrInd + 25 + spInc ))   !      N:C for growth under optimal conditions                (gN gC-1)
            NO3Cm        = PMSA(ipnt( nrInd + 26 + spInc ))   !      N:C that totally represses NO3 transport               (gN gC-1)
            NO3Copt      = PMSA(ipnt( nrInd + 27 + spInc ))   !      N:C for growth on NO3 under optimal conditions         (gN gC-1)
            Opti_CR      = PMSA(ipnt( nrInd + 28 + spInc ))   !      proportion of prey captured by starved Zoo             (dl)
            PCm          = PMSA(ipnt( nrInd + 29 + spInc ))   !      PC maximum quota                                       (gP gC-1)
            PCo          = PMSA(ipnt( nrInd + 30 + spInc ))   !      PC minimum quota                                       (gP gC-1)
            PCoNCm       = PMSA(ipnt( nrInd + 31 + spInc ))   !      maximum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCoNCopt     = PMSA(ipnt( nrInd + 32 + spInc ))   !      optimum NC when PC is minimum (PCu = 0)                (gN gC-1)
            PCopt        = PMSA(ipnt( nrInd + 33 + spInc ))   !      PC optimum quota                                       (gP gC-1)
            PSDOC        = PMSA(ipnt( nrInd + 34 + spInc ))   !      propoortion of current PS being leaked as DOC          (dl)
            Q10          = PMSA(ipnt( nrInd + 35 + spInc ))   !      Q10 for UmRT                                           (dl)  
            RemiUmPS     = PMSA(ipnt( nrInd + 36 + spInc ))   !      minimum proportion of Umax to come via PS              (dl)
            RePSm        = PMSA(ipnt( nrInd + 37 + spInc ))   !      relative PSmax:Umax on phototrophy                     (dl)
            ReUmNH4      = PMSA(ipnt( nrInd + 38 + spInc ))   !      max. growth rate supported by NH4-N:Umax               (dl)
            ReUmNO3      = PMSA(ipnt( nrInd + 39 + spInc ))   !      max. growth rate supported by NO3-N:Umax               (dl)
            ReUmPS       = PMSA(ipnt( nrInd + 40 + spInc ))   !      max. growth rate supported by PS:Umax                  (dl)
            rProt        = PMSA(ipnt( nrInd + 41 + spInc ))   !      radius of nutrient repleted protist cell               (um)
            SDA          = PMSA(ipnt( nrInd + 42 + spInc ))   !      specific dynamic action                                (dl)
            SiCm         = PMSA(ipnt( nrInd + 43 + spInc ))   !      absolute maximum Si:C (diatom)                         (gSi gC-1)
            SiCopt       = PMSA(ipnt( nrInd + 44 + spInc ))   !      optimum Si:C for (diatom) growth                       (gSi gC-1)
            SiiCo        = PMSA(ipnt( nrInd + 45 + spInc ))   !      minimum Si:C (diatom)                                  (gSi gC-1)
            sw_diat      = PMSA(ipnt( nrInd + 46 + spInc ))   !      switch for selecting diatom; 1 if diatom               (dl)
            UmRT         = PMSA(ipnt( nrInd + 47 + spInc ))   !      maximum growth rate using NH4-N at reference T         (d-1)      
     

            ! Check SV values ---------------------------------------------------------
            ! Check for protC values close to 0, cycle if occur
            !if (protC <= 1.0E-6) then
            !   cycle
            !endif
     
            !Ratio calculation-------------------------------------------------------------------------------                            
            !NC gN gC-1   protist N:C
            NC = protN / protC
               
            !PC gP gC-1   protist P:C
            PC = protP / protC
               
            !SC gSi gC-1   protist Si:C (diatom)
            SC =    protSi / protC  
               
            !ChlC   gChl gC-1   protist Chl:C 
            ChlC    = protChl / protC   
                           
            !Q10 growth-------------------------------------------------------------------------------                
            !UmT    gC gC-1 d-1   Umax at current temperature; this is the maximum possible growth rate which may 
            !only be achieved growing mixotrophically   
            UmT = UmRT * Q10**((Temp - RT) / 10.0)
                              
            !BR gC gC-1 d-1   basal respiration rate    
            BR = UmT * CR   
               
            !Regeneration-------------------------------------------------------------------------------   
            !Nregen gN m-3 d-1   if NC exceeds NH4Cmax (actually the maximum that halts NH4 usage) then this 
            !excess is voided   
            if (NC > NCm) then 
                Nregen = (protN - protC * NCm)
            else 
                Nregen = 0.0
            end if 
               
            !Pregen gP m-3 d-1   if PC exceeds PCmax then this excess is voided 
            if (PC > PCm) then 
                Pregen = (protP - protC * PCm)
            else 
                Pregen = 0.0
            end if 
               
            !Nutrient sufficiency------------------------------------------------------------------------------- 
            !NCu    dl   N:C status of cell; 1 is maximum (good)
            if (NC >= NCo) then 
                if (NC <= NCopt) then
                    NCu = (1 + KQN) * (NC - NCo) / ((NC - NCo) + KQN * (NCopt - NCo))
                else 
                    NCu = 1.0
                end if 
            else 
                NCu = 0.0
            end if 
                                     
            !PCu    dl   P:C status of cell; 1 is maximum (good)
            if (PC >= PCo) then 
                if (PC<=PCopt) then 
                    PCu = (1 + KQP) * (PC - PCo) / ((PC - PCo) + KQP * (PCopt - PCo))
                else 
                    PCu = 1.0
                end if
            else
                PCu = 0.0
            end if 
               
            !VSi    gSi gC-1 d-1   potential transport rate of Si to compute SCu
            if (sw_diat == 1.0) then 
                VSi = UmT * SiCopt * Si / (Si + KtSi)
            else 
                VSi = 0.0
            end if 
               
            !SCu    dl   Si:C status of the (diatom) cell; 1 is maximum (good)
            !!??I Added = to SC >= SiiCo, is it ok??
            if (sw_diat == 1.0) then 
                if (SC >= SiiCo) then 
                    if (VSi >= UmT * SiiCo) then 
                        SCu = 1.0
                    else 
                        SCu = VSi / (UmT * SiiCo)
                    end if
                else 
                    SCu = 0.0
                end if 
            else 
                SCu = 1.0
            end if
               
            !NPCu   dl   nutrient status (assumes Liebig-like selection)
            NPCu    = MIN(NCu, PCu)
               
            !NPSiCu dl   minimum of N-P-Si limitation; Liebig-style limitation of growth
            NPSiCu = MIN(NPCu, SCu) 

            !PO4 control-------------------------------------------------------------------------------
            !nPCopt dl   normalised optimal quota for P:C
            nPCopt = (PCopt - PCo) / (PCm - PCo)    
               
            !nPC    dl   normalised quota for P:C
            nPC = (PC - PCo) / (PCm - PCo)  
               
            !LHP    dl   increase in acquisition potential for PO4
            LHP = 1.0 + APaP * (1.0 + KiP**HiP) * (((nPCopt - nPC) / nPCopt)**HiP) / ((((nPCopt - nPC) / nPCopt)**HiP) + KiP**HiP)  
               
            !HLP    dl   decrease in acquisition potential for PO4
            HLP = 1.0 - (1.0 + KdP**HdP) * (((nPC - nPCopt) / (1.0 - nPCopt))**HdP) / ((((nPC - nPCopt) / (1.0 - nPCopt))**HdP) + KdP**HdP)
     
            !APoptP gP/gC/d   acquisition potential for P at PC=PCopt  to match Umax
            !?? can protozooplankton really grow purely osmotrophically? 
            APoptP = UmT * PCopt    
     
            !APP    gP/gC/d   acqusiton potential for PO4
            if (nPC < nPCopt) then 
                APP = APoptP * LHP
            else 
                APP = APoptP * HLP
            end if
               
            !NH4 control-------------------------------------------------------------------------------
            !NH4CPopt   gN gC-1   optimum N:C controlling NH4 transport, affected by PC status
            if (PCu < NCu) then 
                NH4CPopt =  PCoNCopt + PCu * (NCopt - PCoNCopt) 
            else
                NH4CPopt =  PCoNCopt + 1.0 * (NCopt - PCoNCopt)
            end if
               
            !NH4CPm gN gC-1   maximum N:C controlling NH4 transport, affected by PC status
            if (PCu < NCu) then 
                NH4CPm = PCoNCm + PCu * (NCm - PCoNCm)
            else 
                NH4CPm = PCoNCm + 1.0 * (NCm - PCoNCm)
            end if   
               
            !nNH4Copt   dl   normalised NCopt for NH4 transport
            nNH4Copt    = (NH4CPopt - NCo) / (NH4CPm - NCo) 
               
            !nNH4C dl   normalised optimal quota for N:C controlng NH4 transport
            nNH4C = (NC - NCo) / (NH4CPm - NCo) 
               
            !HLNH4 dl   decrease in acquisition potential for NH4
            if (nNH4C > nNH4Copt) then
                HLNH4 = 1.0 - (1.0 + KdN**HdN) * (((nNH4C - nNH4Copt) / (1.0 - nNH4Copt))**HdN) / ((((nNH4C - nNH4Copt) / (1.0 - nNH4Copt))**HdN) + KdN**HdN)
            else 
                HLNH4 = 0.0 
            end if 
                              
            !LHNH4 dl   increase in acquisition potential for NH4 with normalised quotas
            LHNH4 = 1.0 + APaNH4 * (1.0 + KiN**HiN) * (((nNH4Copt - nNH4C) / nNH4Copt)**HiN) / ((((nNH4Copt - nNH4C) / nNH4Copt)**HiN) + KiN**HiN)   
                             
            !APoptNH4   gN gC-1 d-1   acquisition potential for NH4 at NC=NH4Copt to match UmaxNH4
            APoptNH4    = UmT * ReUmNH4 * NCopt
               
            !APNH4  gN gC-1 d-1   acqusiton potential for NH4
            if (nNH4C < nNH4Copt) then 
                APNH4 = APoptNH4 * LHNH4
            else 
                APNH4 = APoptNH4 * HLNH4
            end if 
               
            !NO3 control------------------------------------------------------------------------------- 
            !NO3CPopt   gN gC-1   optimum N:C controlling NO3 transport, affected by PC status
            if (PCu < NCu) then 
                NO3CPopt = PCoNCopt + PCu * (NO3Copt - PCoNCopt)
            else
                NO3CPopt = PCoNCopt + 1.0 * (NO3Copt - PCoNCopt)
            end if 
               
            !NO3CPm gN gC-1   maximum N:C controlling NO3 transport, affected by PC status
            if (PCu < NCu) then 
                NO3CPm = PCoNCm + PCu * (NO3Cm - PCoNCm)
            else 
                NO3CPm = PCoNCm + 1.0 * (NO3Cm - PCoNCm)
            end if 
               
            !nNO3Copt   dl   normalised NCopt for NO3 transport
            nNO3Copt    = (NO3CPopt - NCo) / (NO3CPm - NCo)
               
            !nNO3C dl   normalised optimal quota for N:C controlng NO3 transport
            if (NC<NO3CPm) then 
                nNO3C = (NC - NCo) / (NO3CPm - NCo)
            else 
                nNO3C = 1.0
            end if 
               
            !HLNO3 dl   decrease in acquisition potential for NO3
            if (nNO3C > nNO3Copt) then 
                HLNO3 = 1.0 - (1.0 + KdN**HdN)*(((nNO3C - nNO3Copt) / (1.0 - nNO3Copt))**HdN) / ((((nNO3C - nNO3Copt) / (1.0 - nNO3Copt))**HdN) + KdN**HdN)
            else 
                HLNO3 = 0.0
            end if 
               
            !LHNO3 dl   increase in acquisition potential for NO3
            LHNO3 = 1.0 + APaNO3 * (1.0 + KiN**HiN) * (((nNO3Copt - nNO3C) / nNO3Copt)**HiN) / ((((nNO3Copt - nNO3C) / nNO3Copt)**HiN) + KiN**HiN)  
               
            !APoptNO3   gN gC-1 d-1  acquisition potential for NO3 at NC=NO3Copt to match UmaxNO3
            APoptNO3 = (UmT * ReUmNO3) * NO3Copt    
               
            !APNO3  gN gC-1 d-1   acqusiton potential for NO3
            if (nNO3C < nNO3Copt) then 
                APNO3 = APoptNO3 * LHNO3
            else 
                APNO3 = APoptNO3 * HLNO3
            end if   
                        
            !uptake-------------------------------------------------------------------------------  
            !upP    gP gC-1 d-1   PO4 uptake rate
            if (PO4 > 1.0E-6) then 
                upP = APP * PO4 / (PO4 + KtP)
            else 
                upP = 0.0
            end if 
               
            !upNH4  gN gC-1 d-1   NH4 uptake rate
            if (NH4 > 1.0E-6) then 
                upNH4 = APNH4 * NH4 / (NH4 + KtNH4)
            else 
                upNH4 = 0.0
            end if 
               
            !upNO3  gN gC-1 d-1   NO3 uptake rate
            if (NO3 > 1.0E-6) then 
                upNO3 = APNO3 * NO3 / (NO3 + KtNO3)
            else 
                upNO3 = 0.0
            end if        

            !DOC control-------------------------------------------------------------------------------  
               
            !nNCopt dl   normalised optimal quota for N:C
            nNCopt =    (NCopt - NCo) / (NCm - NCo) 
               
            !nNC    dl   normalised quota for N:C
            nNC = (NC - NCo) / (NCm - NCo)
               
            !HHDOC  dl   increase in acquisition potential for DOC
            HHDOC = 1.0 + APaDOC * (1.0 + KiDOC**HiDOC) * (((nNC - nNCopt) / (1.0 - nNCopt))**HiDOC) / ((((nNC - nNCopt) / (1.0 - nNCopt))**HiDOC) + KiDOC**HiDOC)  
               
            !LLDOC  dl   decrease in acquisition potential for DOC
            LLDOC = 1.0 - (1.0 + KdDOC**HdDOC) * (((nNCopt - nNC) / nNCopt)**HdDOC) / ((((nNCopt - nNC) / nNCopt)**HdDOC) + KdDOC**HdDOC)   
               
            !APoptDOC   gC gC-1 d-1   acquisition potential for DOC at NC=NH4Copt to match Umax against respratory costs
            APoptDOC    = UmT * (1.0 + CR + AR * NCopt) 
               
            !APDOC  APoptDOC*(IF (nNC<nNCopt,LLDOC,HHDOC))  gC/gC-1 d-1   acqusition potential for DOC
            if (nNC < nNCopt) then 
                APDOC = APoptDOC * LLDOC
            else 
                APDOC = APoptDOC * HHDOC
            end if 
                        
            ! phagotrophic requirements------------------------------------------------------------------ 
            !HetMax gC gC-1 d-1    maximum heterotrophic C uptake when a proportion of C must come via PS, 
            !and heterotrophy can in total darkness thus only support survival (covering BR)
            if (ReUmPS > 0.0) then 
                if (avgnetPS > 0.0) then 
                    HetMax = avgnetPS / RemiUmPS + BR
                else 
                    HetMax = BR
                end if 
            else 
                HetMax = UmT + BR
            end if               
  
            ! DOC uptake-------------------------------------------------------------------------------  
            !APDOCm gC/gC-1 d-1   maximum acquisition potential for DOC, depending on average PS and BR; 
            !only applicable if there is a need for PS to contribute to C acqusition to support growth
            if (RemiUmPS > 0.0) then 
                APDOCm = HetMax
            else 
                APDOCm = APoptDOC
            end if 
                              
            !relU   dl   growth rate (day-averaged) relative to maximum
            if (avgCu > 0.0) then 
                relU = avgCu/UmT
            else 
                relU = 0.0 
            end if 
                              
            !satCon dl    satiation control related to rate of growth (relU) via a sigmoidal function with 
            !Hill number Hv and K Kv; value 1 of unsatiated, 0 if fully satiated 
            if (relU < 1.0) then 
                satCon = (1.0 + Kv**Hv) * (1.0 - relU)**Hv / ((1.0 - relU)**Hv + Kv**Hv)
            else 
                satCon = 0.0
            end if 
            
            !upDOC  gC gC-1 d-1  DOC uptake rate; if C-supply is fully satiated (satCon) then shut down 
            !this function. Note though that upDOC also interacts with PS, so if sufficient DOC is coming in 
            !then decrease the PS rate.
            if (DOC > 1.0E-6) then 
                upDOC = MIN(APDOC,APDOCm) * satCon * DOC / (DOC + KtDOC)
            else 
                upDOC = 0.0
            end if 
                                                               
            ! !Photosynthesis-------------------------------------------------------------------------------  
            !frat   dl   f-ratio
            frat    = upNO3 / (upNO3 + upNH4 + 1e-12)   
               
            !minPhotUm  gC gC-1 d-1   min Umax via PS
            minPhotUm = UmT * RemiUmPS  
               
            !PhotUm gC gC-1 d-1   maximum growth by phototrophy
            PhotUm = UmT * ReUmPS   
               
            !addPhotUm  gC gC-1 d-1   additional need for PS after accounting for the minimum need, 
            !and C input from upDOC
            addPhotUm = MAX((PhotUm - minPhotUm - upDOC), 0.0)  
               
            !PSqm   gC gC-1 d-1   maximum photosyntheic rate required to support the highest growth rate 
            !(assumed to be NH4-supported); plateau of the gross PE curve. Note that this value makes reference 
            !to the maximum possible value (relative to Umax) and to the difference between the day-average growth 
            !rate (avgCu) and the maximum growth rate.
            if (ReUmPS > 0.0) then 
                if (avgCu > 0.0) then 
                    PSqm = ((1.0 + PSDOC) * (minPhotUm + addPhotUm) * MIN(RePSm, (PhotUm / avgCu)) * (1.0 + NCopt * (redco + AR)) * NPSiCu + BR) + 1e-6
                else 
                    PSqm = ((1.0 + PSDOC) * (minPhotUm + addPhotUm) * RePSm * (1.0 + NCopt * (redco + AR)) * NPSiCu + BR) + 1e-6
                end if 
            else 
                PSqm = 1e-6
            end if 
               
            !Pyt    dl   intermediate in depth-integrated photosynthesis calculation according to the Smith equation
            Pyt = (alpha * ChlC * PFD * 24 * 60 * 60) / PSqm    
               
            !PS gC gC-1 d-1   gross photosynthesis rate
            PS = PSqm * (log(Pyt + sqrt(1.0 + Pyt**2)) - log(Pyt * exat + sqrt(1.0 + (Pyt * exat)**2))) / atten 
               
            !Cfix   gC gC-1 d-1   gross photosynthesis rate retained for physiology
            Cfix = PS * (1.0 - PSDOC)   

            !dChl   gChl gC-1 d-1   rate of change in Chl:C (synthesis and degradation)
            if (ChlCm > 0.0) then 
                if (ChlC > ChlCo) then
                    dChl    = ChlCm * PhotUm * NPSiCu * Mphoto * (1.0 - Cfix / PSqm) * (1.0 - ChlC / ChlCm) / (1.0 - ChlC / ChlCm + 0.05) - (ChlC * UmT * (1.0 - NPSiCu))   
                else 
                    dChl    = ChlCm * PhotUm * NPSiCu * Mphoto * (1.0 - Cfix / PSqm) * (1.0 - ChlC / ChlCm) / (1.0 - ChlC / ChlCm + 0.05) 
                end if 
            else 
                dChl = 0.0
            end if 
                           
            !photoR gC gC-1 d-1     rate of phototrophy-related respiration. 
            !NOTE this does not include the cost for assimilating amino acid N, nor of recovering SDA-NH4
            photoR = (redco * upNO3) + AR * (upNH4 + upNO3) 

            !phoTro gC gC-1 d-1     rate of (positive) net phototrophy
            if (Cfix > photoR) then 
                phoTro = Cfix - photoR
            else 
                phoTro = 0.0
            end if 
            
            ! Prey and Pred configuration ------------------------------------------------------------------
                            
            !v_Prot m s-1   motility speed of protist, adjusted for its relative level of satiation; 
            !a cell with a low growth rate has a higher motolity rate (as controlled via relV) Diatoms canot swim. 
            !The 1e-12 prevents this value becoming zero.
            !!?? What do the numbers stand for??
            if (sw_diat == 0.0) then 
                v_Prot = satCon * (1e-6 * (38.542 * (rProt * 2)**0.5424)) + 1e-12
            else 
                v_Prot = 1.0e-12
            end if 
                       
            CRCP_sum = 0.0
            
            !ADD SAFETY CHEK SO THAT THE PREDATOR CANNOT EAT ITSELF!!!
            
            ! interim species loop 2 to calculate the ingestion of each species by the current predator
            do iPrey = 0, (nrSp - 1) 
                
                !prey specific input
                preyC = PMSA(ipnt( nrInd +  1 + nrSpCon * iPrey ))   !      C-biomass
                preyN = PMSA(ipnt( nrInd +  3 + nrSpCon * iPrey ))   !      N-biomass
                preyP = PMSA(ipnt( nrInd +  4 + nrSpCon * iPrey ))   !      P-biomass
                avgCu_prey    = PMSA(ipnt( nrInd +  6 + nrSpCon * iPrey ))   !      avgCu of Prey
                             
                Ccell_prey    = PMSA(ipnt( nrInd + 11 + nrSpCon * iPrey ))   !      C content of protist cell                              (pgC cell-1)                
                r_prey        = PMSA(ipnt( nrInd + 41 + nrSpCon * iPrey ))   !      radius of nutrient repleted protist cell               (um)      
                sw_diat_prey  = PMSA(ipnt( nrInd + 46 + nrSpCon * iPrey ))   !      switch for selecting diatom; 1 if diatom               (dl)  
                UmRT_prey     = PMSA(ipnt( nrInd + 47 + nrSpCon * iPrey ))   !      maximum growth rate using NH4-N at reference T         (d-1) 
                
                !predator specific input
                PR            = PMSA(ipnt( nrInd + 48 + iPrey + spInc ))     !      prey handling
                
                !auxiliaries needed to for prey motility  ---------------------------------------------------
                UmT_prey = UmRT_prey * Q10**((Temp - RT) / 10.0)
                
                !relU   dl   growth rate (day-averaged) relative to maximum
                if (avgCu_prey > 0.0) then 
                    relU_prey = avgCu_prey/UmT_prey
                else 
                    relU_prey = 0.0 
                end if 
                                  
                !satCon dl    satiation control related to rate of growth (relU) via a sigmoidal function with 
                !Hill number Hv and K Kv; value 1 of unsatiated, 0 if fully satiated 
                if (relU_prey < 1.0) then 
                    satCon_prey = (1.0 + Kv**Hv) * (1.0 - relU_prey)**Hv / ((1.0 - relU_prey)**Hv + Kv**Hv)
                else 
                    satCon_prey = 0.0
                end if   
                                
                !v_Prey m s-1   motility speed of prey, adjusted for its relative level of satiation; 
                !a cell with a low growth rate has a higher motolity rate (as controlled via relV) Diatoms canot swim. 
                if (sw_diat_prey == 0.0) then 
                    v_Prey = satCon_prey * (1e-6 * (38.542 * (r_prey * 2)**0.5424)) + 1e-12
                else 
                    v_Prey = 1.0e-12
                end if 
                
                !predator prey encounter --------------------------------------------------------------------
                                                
                !nos_prey   nos m-3    cell abundance of prey1; Ccell is pgC/cell, C_prey1 is gC/m3; 
                !transform betwen g and pg is 10^12
                if (preyC > 1.0E-3) then 
                    nos_prey = 1e12 * preyC / Ccell_prey
                else 
                    nos_prey = 0.0
                end if 
                                                
                !Enc_prey   prey predator-1 d-1     cell-specific encounter rate between protist and prey1  
                !!?? I can't get the units to fit... :( ??
                if (v_Prey <= v_Prot) then                              
                    Enc_prey = (24.0 * 60.0 * 60.0) * PI_8 *(r_prey / 1E6 + rProt / 1E6)**2 * nos_prey * ((v_Prey**2 + 3 * v_Prot**2 + 4 * wTurb**2) * ((v_Prot**2 + wTurb**2)**(-0.5))) * 3.0**(-1.0)
                else 
                    Enc_prey = (24.0 * 60.0 * 60.0) * PI_8 *(r_prey / 1E6 + rProt / 1E6)**2 * nos_prey * ((v_Prot**2 + 3 * v_Prey**2 + 4 * wTurb**2) * ((v_Prey**2 + wTurb**2)**(-0.5))) * 3.0**(-1.0)
                end if 
                                        
                !CR_prey    prey Prot-1 d-1 potential capture of prey1 taking into account all factors
                if (preyC > 1.0E-3) then 
                    CR_prey = Enc_prey * PR * Opti_CR
                else 
                    CR_prey = 0.0
                end if 
                                            
                !CRCP_prey  gC gC-1 d-1 Potential C-specific ingestion of prey1
                CRCP_prey(iPrey + 1) = CR_prey * Ccell_prey / CcellPro
                                            
                !CRCP_sum   gC gC-1 d-1 sum of potential C-specific ingestions of all prey types
                CRCP_sum = CRCP_prey(iPrey + 1) + CRCP_sum
                
            enddo
            
            ingNC = 0.0
            ingPC = 0.0
            
            do iPrey = 0, (nrSp - 1) 
                                
                preyC = PMSA(ipnt( nrInd +  1 + nrSpCon * iPrey ))   !      C-biomass
                preyN = PMSA(ipnt( nrInd +  3 + nrSpCon * iPrey ))   !      N-biomass
                preyP = PMSA(ipnt( nrInd +  4 + nrSpCon * iPrey ))   !      P-biomass
                   
                !propIPrey  dl   proportion of prey X in the diet
                if (CRCP_sum > 0.0 .AND. preyC > 1.0E-3) then 
                    propIPrey(iPrey + 1) = CRCP_prey(iPrey + 1)/CRCP_sum
                else 
                    propIPrey(iPrey + 1) = 0.0 
                end if 
                                 
            ! Stoichiometric assimilation of prey ---------------------------------------------------
                                  
                !ingNC gN gC-1   ingestate N:C
                ingNC = propIPrey(iPrey + 1) * preyN/preyC + ingNC  
                   
                !ingPC  gP gC-1   ingestate P:C
                ingPC = propIPrey(iPrey + 1) * preyP/preyC + ingPC  
                
            enddo
               
            !stoich_con dl     stoichiometric control used to regulate AE
            stoich_con = MIN((ingNC / NCopt), (ingPC / PCopt), 1.0)
               
            !AEqual dl  efficiency parameter for assimilation
            AEqual = AEo + (AEm - AEo) * stoich_con / (stoich_con + kAE) * (1.0 + kAE)  
               
            !opAEC  dl    Operational AE for C  
            opAEC = stoich_con * AEqual + 1e-20 
               
            !Igmax  gC gC-1 d-1    maximum ingestion rate linked to the maximum rate of heterotrophy
            Igmax = (HetMax / (1.0 - SDA)) / opAEC
                              
            !APoptpred  gC gC-1 d-1   predation rate to support the stated growth rate (ingCmax is previous models).
            if (ReUmPS > 0.0 .AND. avgnetPS > 0.0 .AND. avgnetPS < UmT) then 
                APoptpred = (1.0 - avgnetPS / UmT) * ((UmT + BR) / (1.0 - SDA)) / opAEC
            else 
                APoptpred = ((UmT + BR) / (1.0 - SDA)) / opAEC
            end if 
               
            !Igmop  gC gC-1 d-1     operational maximum ingestion rate as the minimum of the acqusition potential 
            !and Igmax
            Igmop = MIN(APoptpred, Igmax)   
                               
            !KI gC (gC)-1 d-1 satiation control constant
            KI  = Igmop/4   

            !ingC_Prot  gC (gC)-1 d-1    Ingestion rate of prey-C 
            ingC_Prot= MIN((Igmop * CRCP_sum / (CRCP_sum + KI)), CRCP_sum)  
                        
            !ingN_Prot gN (gC)-1 d-1    Ingestion rate of prey-N
            ingN_Prot = ingC_Prot * ingNC

            !ingP_Prot gP (gC)-1 d-1    Ingestion rate of prey-P 
            ingP_Prot = ingC_Prot * ingPC
                         
            !assC_Prot  gC (gC)-1 d-1   assimilation rate of prey C (all sources) into protist 
            if (opAEC > 0.0) then 
                assC_Prot = ingC_Prot * opAEC
            else 
                assC_Prot = 0.0
            end if 
               
            !assN_Prot  gN (gC)-1 d-1   assimilation rate of prey N (all sources) into protist 
            assN_Prot = assC_Prot * NCopt
               
            !assP_Prot  gP (gC)-1 d-1   assimilation rate of prey P (all sources) into protist 
            assP_Prot = assC_Prot * PCopt   
                        
            !Growth------------------------------------------------------------------------------- 
            !SDAN   gN/gC/d    los of assimilated N via SDA (if mixotrophic, this is then recovered, 
            !but at an additional C cost.
            SDAN    = assN_Prot * SDA   
               
            !totR   gC gC-1 d-1   total respiration rate
            if (ReUmPS > 0.0) then 
                totR = (redco * upNO3) + AR * (upNH4 + upNO3 + SDAN) + (assC_Prot * SDA) + BR
            else 
                totR = (redco * upNO3) + AR * (upNH4 + upNO3) + (assC_Prot * SDA) + BR
            end if 
               
            !Cu gC gC-1 d-1   instantaneous C-specific growth rate
            Cu  = Cfix + upDOC + assC_Prot - totR
                
            !upSi   gSi gC-1 d-1 Si uptake rate
            if (sw_diat == 1.0 .AND. Si > 0.0 .AND. (SC < SiCm - 0.01)) then 
                if (SCu > NPSiCu .OR. SCu == 1.0) then 
                    if (avgCu > 0.0) then 
                        upSi = (avgCu / UmT)**betaSi * UmT * SiCopt * Si / (Si + KtSi) * (1.0 - SC / SiCm)**HSi / ((1.0 - SC / SiCm)**HSi + KSi)
                    else 
                        upSi = 0.0
                    end if 
                else 
                    upSi = UmT * SiCopt * Si / (Si + KtSi) * (1.0 - SC / SiCm)**HSi / ((1.0 - SC / SiCm)**HSi + KSi)
                end if 
            else 
                upSi = 0.0
            end if 
                                    
            !hetTro gC gC-1 d-1    rate of heterotrophy
            hetTro = Cu - phoTro
                        
            !Cgro   gC m-3 d-1   population biomass growth rate
            Cgro = protC * Cu   
                                   
            !mortality-------------------------------------------------------------------------------  
            mrt = MrtRT * Q10**((Temp - RT) / 10.0)
            mrtFrAut = mrt * FrAut 
            mrtFrDet = mrt * FrDet 
                                                               
            ! Output -------------------------------------------------------------------
               
            !(input items + position of specific output item in vector + species loop * total number of output) 
            PMSA(ipnt( inpItems +   5 + iPred * 86 )) = NC
            PMSA(ipnt( inpItems +   6 + iPred * 86 )) = PC
            PMSA(ipnt( inpItems +   7 + iPred * 86 )) = SC
            PMSA(ipnt( inpItems +   8 + iPred * 86 )) = ChlC
            PMSA(ipnt( inpItems +   9 + iPred * 86 )) = UmT
            PMSA(ipnt( inpItems +  10 + iPred * 86 )) = BR
            PMSA(ipnt( inpItems +  11 + iPred * 86 )) = Nregen
            PMSA(ipnt( inpItems +  12 + iPred * 86 )) = Pregen
            PMSA(ipnt( inpItems +  13 + iPred * 86 )) = NCu
            PMSA(ipnt( inpItems +  14 + iPred * 86 )) = PCu
            PMSA(ipnt( inpItems +  15 + iPred * 86 )) = VSi
            PMSA(ipnt( inpItems +  16 + iPred * 86 )) = SCu
            PMSA(ipnt( inpItems +  17 + iPred * 86 )) = NPCu
            PMSA(ipnt( inpItems +  18 + iPred * 86 )) = NPSiCu
            PMSA(ipnt( inpItems +  19 + iPred * 86 )) = nPCopt
            PMSA(ipnt( inpItems +  20 + iPred * 86 )) = nPC
            PMSA(ipnt( inpItems +  21 + iPred * 86 )) = LHP
            PMSA(ipnt( inpItems +  22 + iPred * 86 )) = HLP
            PMSA(ipnt( inpItems +  23 + iPred * 86 )) = APoptP
            PMSA(ipnt( inpItems +  24 + iPred * 86 )) = APP
            PMSA(ipnt( inpItems +  25 + iPred * 86 )) = NH4CPopt
            PMSA(ipnt( inpItems +  26 + iPred * 86 )) = NH4CPm
            PMSA(ipnt( inpItems +  27 + iPred * 86 )) = nNH4Copt
            PMSA(ipnt( inpItems +  28 + iPred * 86 )) = nNH4C
            PMSA(ipnt( inpItems +  29 + iPred * 86 )) = HLNH4
            PMSA(ipnt( inpItems +  30 + iPred * 86 )) = LHNH4
            PMSA(ipnt( inpItems +  31 + iPred * 86 )) = APoptNH4
            PMSA(ipnt( inpItems +  32 + iPred * 86 )) = APNH4
            PMSA(ipnt( inpItems +  33 + iPred * 86 )) = NO3CPopt
            PMSA(ipnt( inpItems +  34 + iPred * 86 )) = NO3CPm
            PMSA(ipnt( inpItems +  35 + iPred * 86 )) = nNO3Copt
            PMSA(ipnt( inpItems +  36 + iPred * 86 )) = nNO3C
            PMSA(ipnt( inpItems +  37 + iPred * 86 )) = HLNO3
            PMSA(ipnt( inpItems +  38 + iPred * 86 )) = LHNO3
            PMSA(ipnt( inpItems +  39 + iPred * 86 )) = APoptNO3
            PMSA(ipnt( inpItems +  40 + iPred * 86 )) = APNO3
            PMSA(ipnt( inpItems +  41 + iPred * 86 )) = nNCopt
            PMSA(ipnt( inpItems +  42 + iPred * 86 )) = nNC
            PMSA(ipnt( inpItems +  43 + iPred * 86 )) = HHDOC
            PMSA(ipnt( inpItems +  44 + iPred * 86 )) = LLDOC
            PMSA(ipnt( inpItems +  45 + iPred * 86 )) = APoptDOC
            PMSA(ipnt( inpItems +  46 + iPred * 86 )) = APDOC
            PMSA(ipnt( inpItems +  47 + iPred * 86 )) = upP
            PMSA(ipnt( inpItems +  48 + iPred * 86 )) = upNH4
            PMSA(ipnt( inpItems +  49 + iPred * 86 )) = upNO3
            PMSA(ipnt( inpItems +  50 + iPred * 86 )) = HetMax
            PMSA(ipnt( inpItems +  51 + iPred * 86 )) = APDOCm
            PMSA(ipnt( inpItems +  52 + iPred * 86 )) = relU
            PMSA(ipnt( inpItems +  53 + iPred * 86 )) = satCon
            PMSA(ipnt( inpItems +  54 + iPred * 86 )) = upDOC
            PMSA(ipnt( inpItems +  55 + iPred * 86 )) = frat
            PMSA(ipnt( inpItems +  56 + iPred * 86 )) = minPhotUm
            PMSA(ipnt( inpItems +  57 + iPred * 86 )) = PhotUm
            PMSA(ipnt( inpItems +  58 + iPred * 86 )) = addPhotUm
            PMSA(ipnt( inpItems +  59 + iPred * 86 )) = PSqm
            PMSA(ipnt( inpItems +  60 + iPred * 86 )) = Pyt
            PMSA(ipnt( inpItems +  61 + iPred * 86 )) = PS
            PMSA(ipnt( inpItems +  62 + iPred * 86 )) = Cfix
            PMSA(ipnt( inpItems +  63 + iPred * 86 )) = dChl
            PMSA(ipnt( inpItems +  64 + iPred * 86 )) = photoR
            PMSA(ipnt( inpItems +  65 + iPred * 86 )) = phoTro
            PMSA(ipnt( inpItems +  66 + iPred * 86 )) = CRCP_sum
            PMSA(ipnt( inpItems +  67 + iPred * 86 )) = ingNC
            PMSA(ipnt( inpItems +  68 + iPred * 86 )) = ingPC
            PMSA(ipnt( inpItems +  69 + iPred * 86 )) = stoich_con
            PMSA(ipnt( inpItems +  70 + iPred * 86 )) = AEqual
            PMSA(ipnt( inpItems +  71 + iPred * 86 )) = opAEC
            PMSA(ipnt( inpItems +  72 + iPred * 86 )) = Igmax
            PMSA(ipnt( inpItems +  73 + iPred * 86 )) = APoptpred
            PMSA(ipnt( inpItems +  74 + iPred * 86 )) = Igmop
            PMSA(ipnt( inpItems +  75 + iPred * 86 )) = KI
            PMSA(ipnt( inpItems +  76 + iPred * 86 )) = ingC_Prot
            PMSA(ipnt( inpItems +  77 + iPred * 86 )) = ingN_Prot
            PMSA(ipnt( inpItems +  78 + iPred * 86 )) = ingP_Prot
            PMSA(ipnt( inpItems +  79 + iPred * 86 )) = assC_Prot
            PMSA(ipnt( inpItems +  80 + iPred * 86 )) = assN_Prot
            PMSA(ipnt( inpItems +  81 + iPred * 86 )) = assP_Prot
            PMSA(ipnt( inpItems +  82 + iPred * 86 )) = SDAN
            PMSA(ipnt( inpItems +  83 + iPred * 86 )) = totR
            PMSA(ipnt( inpItems +  84 + iPred * 86 )) = Cu
            PMSA(ipnt( inpItems +  85 + iPred * 86 )) = upSi
            PMSA(ipnt( inpItems +  86 + iPred * 86 )) = hetTro
            PMSA(ipnt( inpItems +  87 + iPred * 86 )) = Cgro
            PMSA(ipnt( inpItems +  88 + iPred * 86 )) = mrt
            PMSA(ipnt( inpItems +  89 + iPred * 86 )) = mrtFrAut
            PMSA(ipnt( inpItems +  90 + iPred * 86 )) = mrtFrDet

            ! FLUXES -------------------------------------------------------------------
            !## let all fluxes start with d?##   
            ! growth and PS fluxes -----------------------------------------------------
            !PSflux gC gC-1 d-1 difference current PS rate to the previous timestep PS rate
            dPS = (phoTro - avgnetPS) 
                        
            !Cuflux gC gC-1 d-1 difference current Cu rate to the previous timestep Cu rate
            dCu = (Cu - avgCu) 
            
            ! Protist growth through assimilation -----------------------------------------------------
            !Ceat gC m-3 d-1 assimilation of C from prey
            dCeat = protC * assC_Prot
                          
            !Neat gN m-3 d-1 assimilation of N from prey
            dNeat = protC * assN_Prot   
                        
            !Peat gP m-3 d-1 assimilation of P from prey
            dPeat = protC * assP_Prot
              
            ! Protist growth through uptake -----------------------------------------------------            
            !NH4gro gN m-3 d-1   uptake of NH4 into algal biomass
            dNH4gro = protC * upNH4 
                
            !NO3gro gN m-3 d-1  uptake of NO3 into algal biomass
            dNO3gro = protC * upNO3 
                
            !Pgro gC m-3 d-1   uptake of PO4 into algal biomass
            dPgro = protC * upP
                        
            !Sigro  gSi m-3 d-1   uptake of Si into algal biomass
            dSigro = protC * upSi
                        
            !DOCgro gC m-3 d-1   population uptake of DOC
            dDOCgro = protC * upDOC 
                        
            ! Protist growth through PS -----------------------------------------------------                       
            !PSgro gC m-3 d-1   total contribution to biomass growth from C-fixation
            dPSgro =    protC * Cfix
            
            ! Protist biomass and Chl synthesis ----------------------------------------------------                              
            !Chlgro gChl m-3 d-1   population Chl rate of change
            dChlgro = protC * dChl  
                        
            ! Protist losses ----------------------------------------------------                  
            !Cresp gC m-3 d-1   total respiration rate
            dCresp = protC * totR   
                        
            !DOCleak    gC m-3 d-1   release of DOC 
            !PS*PSDOC   =  gross Cfix that is lost as DOC
            dDOCleak    = protC * PS * PSDOC
                        
            !DOCvoid gC m-3 d-1   voiding of C as DOC if NC falls below NCo
            if (NC < NCo) then 
                dDOCvoid = protC - protN / NCo
            else 
                dDOCvoid = 0.0
            end if 
                                    
            !Pout gP m-3 d-1   PO4 release by regeneration
            dPout = Pregen
                        
            !NH4out gN m-3 d-1   NH4 release by regeneration
            if (ReUmPS == 0.0) then 
                dNH4out = Nregen + protN * SDAN
            else 
                dNH4out = Nregen
            end if
            
            !VOCout gC m-3 d-1     rate of voiding of C as particulates
            dVOCout = protC * (ingC_Prot - assC_Prot)
            
            !VONout gN m-3 d-1     rate of voiding of N as particulates
            dVONout = protC * (ingN_Prot - assN_Prot)   
            
            !VOPout gP m-3 d-1     rate of voiding of P as particulates
            dVOPout = protC* (ingP_Prot - assP_Prot)    
                        
            ! Autolysis and detritus fluxes ----------------------------------------------------    
            if (ReUmPS == 0.0) then             
                dAutC       = protC * preyC * mrtFrAut
                dDetC       = protC * preyC * mrtFrDet  
                dAutN       = protN * preyN * mrtFrAut
                dDetN       = protN * preyN * mrtFrDet          
                dAutP       = protP * preyP * mrtFrAut
                dDetP       = protP * preyP * mrtFrDet      
                dAutSi      = protSi * preySi * mrtFrAut
                dDetSi      = protSi * preySi * mrtFrDet            
                dAutChl     = protChl * preyChl * mrtFrAut
                dDetChl     = protChl * preyChl * mrtFrDet
            else                
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
            endif       
              
            !(1 + SpeciesLoop * nr of fluxes per individual species + total number of fluxes) 
            fl (  1 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dPS       
            fl (  2 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dCu       
            fl (  3 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dCeat     
            fl (  4 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dNeat     
            fl (  5 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dPeat     
            fl (  6 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dNH4gro   
            fl (  7 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dNO3gro   
            fl (  8 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dPgro     
            fl (  9 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dSigro    
            fl ( 10 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDOCgro   
            fl ( 11 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dPSgro    
            fl ( 12 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dChlgro   
            fl ( 13 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dCresp    
            fl ( 14 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDOCleak  
            fl ( 15 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDOCvoid
            fl ( 16 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dPout
            fl ( 17 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dNH4out
            fl ( 18 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dVOCout   
            fl ( 19 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dVONout   
            fl ( 20 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dVOPout   
            fl ( 21 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dAutC   
            fl ( 22 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDetC   
            fl ( 23 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dAutN   
            fl ( 24 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDetN   
            fl ( 25 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dAutP   
            fl ( 26 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDetP    
            fl ( 27 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dAutSi   
            fl ( 28 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDetSi 
            fl ( 29 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dAutChl
            fl ( 30 +  iPred * (30 + maxNrSp * 5) + iflux )  =   dDetChl
            
                      
            ! Prey losses through pred ing. ----------------------------------------------------    
            
            do iPrey = 0, (nrSp - 1)
                                
                preyC   = PMSA(ipnt( nrInd +  1 + nrSpCon * iPrey ))   !      C-biomass
                preyChl = PMSA(ipnt( nrInd +  2 + nrSpCon * iPrey ))   !      C-biomass
                preyN   = PMSA(ipnt( nrInd +  3 + nrSpCon * iPrey ))   !      N-biomass
                preyP   = PMSA(ipnt( nrInd +  4 + nrSpCon * iPrey ))   !      P-biomass
                preySi  = PMSA(ipnt( nrInd +  5 + nrSpCon * iPrey ))   !      P-biomass
                   
                if (preyC <= 1.0E-3 .OR. iPrey == iPred) then
                    ! ingestion of Nut of iPrey through iPred gNut m-3 d-1
                    dPreyC    = 0.0
                    dPreyChl  = 0.0        
                    dPreyN    = 0.0 
                    dPreyP    = 0.0
                    dPreySi   = 0.0
                else  
                    ! ingestion of nut of iPrey through iPred gNut m-3 d-1  
                    dPreyC    = protC * (ingC_Prot * propIPrey(iPrey + 1))  
                    dPreyChl  = dPreyC * (preyChl / preyC)            
                    dPreyN    = dPreyC * (preyN / preyC) 
                    dPreyP    = dPreyC * (preyP / preyC) 
                    dPreySi   = dPreyC * (preySi / preyC)   
                endif
                
                fl ( (30 + 1 + iPrey * 5) + (30 + maxNrSp * 5) * iPred + iflux ) = dPreyC  
                fl ( (30 + 2 + iPrey * 5) + (30 + maxNrSp * 5) * iPred + iflux ) = dPreyChl
                fl ( (30 + 3 + iPrey * 5) + (30 + maxNrSp * 5) * iPred + iflux ) = dPreyN  
                fl ( (30 + 4 + iPrey * 5) + (30 + maxNrSp * 5) * iPred + iflux ) = dPreyP  
                fl ( (30 + 5 + iPrey * 5) + (30 + maxNrSp * 5) * iPred + iflux ) = dPreySi 
            
            end do ! end loop over prey ingestion fluxes
               
        enddo ! end loop over species 

        endif ! end if check for dry cell 

        !allocate pointers
        iflux = iflux + noflux
        ipnt = ipnt + increm

    enddo ! end loop over segments
    return
end ! end subroutine 
