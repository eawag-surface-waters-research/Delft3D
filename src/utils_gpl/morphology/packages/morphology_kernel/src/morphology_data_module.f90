module morphology_data_module
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--module description----------------------------------------------------------
!
! This module defines the data structures for sediment transport and
! morphology.
!
!!--module declarations---------------------------------------------------------
use precision
use handles, only:handletype
use properties, only:tree_data
private

!
! public data types
!
public gd_morpar
public moroutputtype
public mornumericstype
public bedbndtype
public cmpbndtype
public gd_sedpar
public gd_trapar
public fluffy_type

!
! public routines
!
public initmorpar
public clrmorpar
public initsedpar
public clrsedpar
public inittrapar
public clrtrapar
public allocfluffy

integer, parameter, public :: RP_TIME  =  1
integer, parameter, public :: RP_EFUMN =  2
integer, parameter, public :: RP_EFVMN =  3
integer, parameter, public :: RP_EFVLM =  4
integer, parameter, public :: RP_UCHAR =  5
integer, parameter, public :: RP_VCHAR =  6
integer, parameter, public :: RP_VELCH =  7
integer, parameter, public :: RP_ZVLCH =  8
integer, parameter, public :: RP_DEPTH =  9
integer, parameter, public :: RP_CHEZY = 10
integer, parameter, public :: RP_HRMS  = 11
integer, parameter, public :: RP_TPEAK = 12
integer, parameter, public :: RP_TETA  = 13
integer, parameter, public :: RP_RLAMB = 14
integer, parameter, public :: RP_UORB  = 15
integer, parameter, public :: RP_D50   = 16
integer, parameter, public :: RP_DSS   = 17
integer, parameter, public :: RP_DSTAR = 18
integer, parameter, public :: RP_D10MX = 19
integer, parameter, public :: RP_D90MX = 20
integer, parameter, public :: RP_MUDFR = 21
integer, parameter, public :: RP_HIDEX = 22
integer, parameter, public :: RP_SETVL = 23
integer, parameter, public :: RP_RHOSL = 24
integer, parameter, public :: RP_RHOWT = 25
integer, parameter, public :: RP_SALIN = 26
integer, parameter, public :: RP_TEMP  = 27
integer, parameter, public :: RP_GRAV  = 28
integer, parameter, public :: RP_VICML = 29
integer, parameter, public :: RP_TAUB  = 30
integer, parameter, public :: RP_UBED  = 31
integer, parameter, public :: RP_VBED  = 32
integer, parameter, public :: RP_VELBD = 33
integer, parameter, public :: RP_ZVLBD = 34
integer, parameter, public :: RP_VNKAR = 35
integer, parameter, public :: RP_Z0CUR = 36
integer, parameter, public :: RP_Z0ROU = 37
integer, parameter, public :: RP_KTUR  = 38
integer, parameter, public :: RP_DG    = 39
integer, parameter, public :: RP_SNDFR = 40
integer, parameter, public :: RP_DGSD  = 41
integer, parameter, public :: RP_UMEAN = 42
integer, parameter, public :: RP_VMEAN = 43
integer, parameter, public :: RP_VELMN = 44
integer, parameter, public :: RP_USTAR = 45
integer, parameter, public :: MAX_RP   = 45
!

integer, parameter, public :: IP_NM    =  1
integer, parameter, public :: IP_N     =  2
integer, parameter, public :: IP_M     =  3
integer, parameter, public :: IP_ISED  =  4
integer, parameter, public :: MAX_IP   =  4
!
integer, parameter, public :: SP_RUNID =  1
integer, parameter, public :: SP_USRFL =  2
integer, parameter, public :: MAX_SP   =  2

! collection of morphology output options
!
type moroutputtype
    integer :: transptype      ! 0 = mass
                               ! 1 = volume including pores
                               ! 2 = volume excluding pores
    logical :: aks
    logical :: cumavg
    logical :: dg
    logical :: dgsd
    logical :: dm
    logical :: dzduuvv
    logical :: fixfac
    logical :: hidexp
    logical :: frac
    logical :: mudfrac
    logical :: sandfrac
    logical :: percentiles
    logical :: sbcuv
    logical :: sbcuuvv
    logical :: sbwuv
    logical :: sbwuuvv
    logical :: sswuv
    logical :: sswuuvv
    logical :: suvcor
    logical :: sourcesink
    logical :: taurat
    logical :: umod
    logical :: ustar
    logical :: uuuvvv
    logical :: zumod
end type moroutputtype

!
! sediment transport and morphology numerical settings
!
type mornumericstype
    logical :: upwindbedload            ! switch for upwind bedload in UPWBED
    logical :: laterallyaveragedbedload ! bedload transport laterally averaged in UPWBED
    logical :: maximumwaterdepth        ! water depth at zeta point in DWNVEL given by
                                        ! at least water depth at active velocity points
end type mornumericstype

!
! morphology boundary conditions at one open boundary
!
type bedbndtype
    integer                         :: icond    ! bed boundary condition
                                                !   0: "free boundary" (default if updinf = true)
                                                !   1: fixed bed level (default if updinf = false)
                                                !   2: prescribed bed level
                                                !   3: prescribed bed level change rate
                                                !   4: prescribed bed load transport rate
    integer , dimension(4)          :: ibcmt    ! boundary conditions morphology table
                                                !  (1) table index in bcm file
                                                !  (2) first index of boundary parameter
                                                !  (3) number of entries
                                                !  (4) last used record in table
    integer                         :: npnt     ! number points of boundary
    integer , dimension(:), pointer :: idir
    integer , dimension(:), pointer :: nm
    integer , dimension(:), pointer :: nxmx
    real(fp), dimension(:), pointer :: alfa_mag
    real(fp), dimension(:), pointer :: alfa_dist
end type bedbndtype

!
! bed composition boundary conditions at one open boundary
!
type cmpbndtype
    integer                :: icond    ! bed composition condition
                                       !   0: "free composition"
                                       !   1: fixed composition (default)
                                       !   2: prescribed composition
    integer , dimension(4) :: ibcmt    ! boundary conditions morphology table
                                       !  (1) table index in bcm file
                                       !  (2) first index of boundary parameter
                                       !  (3) number of entries
                                       !  (4) last used record in table
end type cmpbndtype

type fluffy_type
    !
    ! doubles (hp)
    !
    !
    ! single / doubles (fp)
    !
    !
    ! singles (sp)
    !
    !
    ! integers
    !
    integer :: iflufflyr  !  switch for fluff layer concept
                          !  0: no fluff layer
                          !  1: all mud to fluff layer, burial to bed layers
                          !  2: part mud to fluff layer, other part to bed layers (no burial) 
    !
    ! pointers
    !
    real(fp)      , dimension(:,:)  , pointer :: mfluff        ! composition of fluff layer: mass of mud fractions, units : kg /m2
    real(fp)      , dimension(:,:)  , pointer :: bfluff0       ! burial parameter fluff layer (only when FluffLayer=1) [kg/m2/s]
    real(fp)      , dimension(:,:)  , pointer :: bfluff1       ! burial parameter fluff layer (only when FluffLayer=1) [1/s]
    real(fp)      , dimension(:,:)  , pointer :: depfac        ! Deposition factor to fluff layer (only when FluffLayer=2) [-]
    real(fp)      , dimension(:,:)  , pointer :: sinkf         ! Settling to fluff layer []
    real(fp)      , dimension(:,:)  , pointer :: sourf         ! Source from fluff layer [] 
    ! 
    ! logicals
    !
    !
    ! characters
    !
    character(256) :: bfluff0_fil      !  name of file for burial parameter 1
    character(256) :: bfluff1_fil      !  name of file for burial parameter 2
    character(256) :: depfac_fil       !  name of file for deposition factor
    !
end type fluffy_type

type gd_morpar
    !
    ! doubles (hp)
    !
    real(hp):: morft      !  morphological time
    real(hp):: morft0     !  initial morphological time
    !
    ! single / doubles (fp)
    !
    real(fp):: morfac     !  morphological timescale factor
    real(fp):: thresh     !  threshold value for slowing erosion near a fixed layer (m)
    real(fp):: aksfac     !  factor for setting aks height
    real(fp):: rwave      !  factor for calculating wave-related roughness from ripple dimensions
    real(fp):: alfabs     !  factor for longitudinal bed load transport
    real(fp):: alfabn     !  factor for transverse bed load transport
    real(fp):: camax      !  Maximum volumetric reference concentration
    real(fp):: dzmax      !  factor for limiting source and sink term in EROSED (percentage of water depth)
    real(fp):: sus        !  flag for calculating suspended load transport
    real(fp):: bed        !  flag for calculating bed load transport
    real(fp):: pangle     !  phase lead angle acc. to Nielsen (1992) for TR2004 expression
    real(fp):: fpco       !  coefficient for phase llag effects
    real(fp):: factcr     !  calibration factor on Shields' critical shear stress   
    real(fp):: tmor       !  time where calculation for morphological changes start (minutes relative to ITDATE,00:00:00)
    real(fp):: thetsd     !  global dry bank erosion factor
    real(fp):: susw       !  factor for adjusting wave-related suspended sand transport (included in bed-load)
    real(fp):: sedthr     !  minimum depth for sediment calculations
    real(fp):: hmaxth     !  maximum depth for setting theta for erosion of dry bank
    real(fp):: bedw       !  factor for adjusting wave-related bed-load sand transport (included in bed-load)
    real(fp):: rdw
    real(fp):: rdc
    real(fp):: espir      !  factor for weighing the effect of the spiral flow intensity in 2D simulations
    real(fp):: ashld      !  bed slope direction effect Shields' parameter
    real(fp):: bshld      !  bed slope direction effect Shields' parameter (power of theta_i)
    real(fp):: cshld      !  bed slope direction effect Shields' parameter (power of d_i/depth)
    real(fp):: dshld      !  bed slope direction effect Shields' parameter (power of d_i/d_m)
    real(fp):: coulfri    !  Coulomb friction coef. in formula of Parker and Andrews
    real(fp):: flfdrat    !  ratio of lift and drag forces (Fl/Fd) in formula of Parker and Andrews
    real(fp):: alfpa      !  coulfri/(1+coulfri*flfdrat)
    real(fp):: thcrpa     !  bed slope theta_cr in formula of Parker and Andrews
    real(fp):: asklhe     !  exponent alpha in hiding & exposure according Soehngen, Kellermann, Loy
    real(fp):: mwwjhe     !  exponent m factor in hiding & exposure according Wu, Wang, Jia
    real(fp):: ttlalpha   !  transport layer thickness: proportionality factor
    real(fp):: ttlmin     !  transport layer thickness: minimum thickness
    real(fp):: wetslope   !  maximum wet bed slope (used for avalanching)
    real(fp):: avaltime   !  time scale in seconds (used for avalanching)
    !
    !  (sp)
    !
    !
    ! integers
    !
    integer :: mergehandle !  stream handle for communication with mormerge
    integer :: i10         !  index of D10 in the xx array
    integer :: i50         !  index of D50 in the xx array
    integer :: i90         !  index of D90 in the xx array
    integer :: ihidexp     !  switch for hiding exposure effect
                           !  1: none
                           !  2: Egiazaroff (1965)
                           !  3: Ashida & Michiue (1971), modified Egiazaroff
                           !  4: Soehngen, Kellermann, Loy (1992)
                           !  5: Wu, Wang, Jia (2000)
    integer :: itmor       !  time step where calculation for morphological changes starts
    integer :: iopkcw
    integer :: iopsus
    integer :: islope      !  switch for bed slope effect, according
                           !  1: none
                           !  2: Bagnold, Ikeda/Van Rijn
                           !  3: Van Bendegom, Koch&Flokstra
    integer :: morfacpar   ! parameter index of morfac in table structure
                           ! only used when varyingmorfac=true
    integer :: morfacrec   ! record index of morfac in table structure
                           ! only used when varyingmorfac=true
    integer :: morfactable ! table index of morfac in table structure
                           ! only used when varyingmorfac=true
    integer :: nxx         !  number of percentiles in xx
    integer :: subiw       ! wave period subdivision in TR2004    
    integer :: ttlform     !  switch for thickness of transport layer
                           !  1: fixed (user-spec.) thickness
                           !  2: 
                           !  3: 
    integer :: telform     !  switch for thickness of exchange layer
                           !  1: fixed (user-spec.) thickness
    !
    ! pointers
    !
    type (fluffy_type)                  , pointer :: flufflyr   ! data for optional fluff layer
    type (handletype)                             :: bcmfile    ! tables containing morphological boundary conditions
    type (handletype)                             :: morfacfile ! table  containing morphological factor
    type (moroutputtype)                , pointer :: moroutput  ! structure containing morphology output options
    type (mornumericstype)              , pointer :: mornum     ! structure containing numerical settings
    type (bedbndtype)     , dimension(:), pointer :: morbnd     ! transport / bed level boundary parameters
    type (cmpbndtype)     , dimension(:), pointer :: cmpbnd     ! bed composition boundary parameters
    real(hp)              , dimension(:), pointer :: mergebuf   ! buffer array for communcation with mormerge
    real(fp)              , dimension(:), pointer :: xx         ! percentile xx (dxx stored in erosed.ig*)
    ! 
    ! logicals
    !
    logical :: bedupd              !  flag for doing bed level updates
    logical :: cmpupd              !  flag for doing composition (underlayer) updates
    logical :: eqmbcsand           !  flag for setting equilibrium sediment concentration profiles at the open boundaries for sand sediment
    logical :: eqmbcmud            !  flag for setting equilibrium sediment concentration profiles at the open boundaries for mud sediment
    logical :: densin              !  Flag=TRUE when sediment is included in fluid density calculations flag for including sediment in fluid density calculations
    logical :: rouse               !  flag for setting equilibrium concentrations to standard Rouse profiles
    logical :: epspar
    logical :: eulerisoglm         !  Flag for using eulerian velocities for suspended transports    
    logical :: glmisoeuler         !  Flag for using GLM velocities for bedload transports and reference concentrations   
    logical :: updinf              !  Flag for updating bottom at inflow openboundaries
    logical :: neglectentrainment  !  flag for neglecting entrainment and suspension in the mass balance (mass balance based on suspended load fluxes)
    logical :: oldmudfrac          !  true: use the old method for the mud source term calculation (without Frac multiplication)
    logical :: varyingmorfac       !  true: morfac specified in a time serie file
    logical :: multi               !  Flag for merging bottoms of different parallel runs
    !
    ! characters
    !
    character(256) :: bcmfilnam    !  name of input  file for morphological boundary conditions
    character(256) :: flcomp       !  name of file containing initial bed composition
    character(256) :: mmsyncfilnam !  name of output file for synchronisation of mormerge run
    character(256) :: telfil       !  name of file containing exchange layer thickness
    character(256) :: ttlfil       !  name of file containing transport layer thickness
    !
end type gd_morpar

type gd_sedpar
    !
    ! doubles
    !
    real(fp) :: csoil     !  concentration at bed used in hindered settling formulation
    real(fp) :: mdcuni    !  mud content / mud fraction uniform value (non-zero only
                          !  if mud is not included simulation)
    real(fp) :: kssilt    !  ks value for silt for Soulsby 2004 formulation
    real(fp) :: kssand    !  ks value for sand 
    !
    ! reals
    !
    !
    ! integers
    !
    integer  :: nmudfrac  !  number of simulated mud fractions
    !
    ! pointers
    !
    type(tree_data), dimension(:)   , pointer :: sedblock => null()    !  Pointer to array of data block per fraction in .sed file (version 2)
    real(fp)      , dimension(:)    , pointer :: rhosol     !  Soil density
    !
    real(fp)      , dimension(:,:,:), pointer :: logseddia             !  Characteristic sediment diameter table using log scale [%,log(m)]
    real(fp)      , dimension(:)    , pointer :: logsedsig             !  Standard deviation on log scale (log of geometric std.) [-]
    real(fp)      , dimension(:)    , pointer :: sedd10                !  10% Diameter sediment fraction [m]
    real(fp)      , dimension(:)    , pointer :: sedd50                !  50% Diameter sediment fraction [m]
    real(fp)      , dimension(:)    , pointer :: sedd50fld  => null()  !  Spatially varying 50% sediment diameter [m]
    real(fp)      , dimension(:)    , pointer :: seddm                 !  Arithmetic mean sediment diameter [m]
    real(fp)      , dimension(:)    , pointer :: sedd90                !  90% Diameter sediment fraction [m]
    !
    real(fp)      , dimension(:)    , pointer :: cdryb      !  Dry bed concentration for determining
                                                            !  sediment depths
    real(fp)      , dimension(:)    , pointer :: dstar      !  Dimensionless grain size 
    real(fp)      , dimension(:)    , pointer :: gamflc     !  Calibration factor on flocculation parameter in Van Rijn (2004) 
    real(fp)      , dimension(:)    , pointer :: taucr      !  Critical shear stress 
    real(fp)      , dimension(:)    , pointer :: tetacr     !  Dimensionless critical shear stress (Shields parameter)
    real(fp)      , dimension(:)    , pointer :: ws0        !  Settling velocity fresh water
    real(fp)      , dimension(:)    , pointer :: wsm        !  Settling velocity saline water
    real(fp)      , dimension(:)    , pointer :: salmax     !  Maximum salinity [ppt]
    real(fp)      , dimension(:)    , pointer :: sdbuni     !  Uniform value of initial sediment mass at bed
    real(fp)      , dimension(:)    , pointer :: sedtrcfac  !  Calibration factor for tracer sediments
    real(fp)      , dimension(:)    , pointer :: thcmud     !  Critical stress erosion uniform values for mud
    real(fp)      , dimension(:)    , pointer :: tcguni     !  Calibration factor on critical shear stress in Van Rijn (2004) uniform values
    real(fp)      , dimension(:)    , pointer :: mudcnt     !  mud content / mud fraction field (non-zero only if mud
                                                            !  is not included simulation)
    real(fp)      , dimension(:)    , pointer :: pmcrit     !  Critical mud fraction for non-cohesive behaviour
    integer       , dimension(:)    , pointer :: nseddia    !  Number of characteristic sediment diameters
    integer       , dimension(:)    , pointer :: sedtyp     !  Sediment type: 0=total/1=noncoh/2=coh
    character(10) , dimension(:)    , pointer :: inisedunit !  'm' or 'kg/m2' : Initial sediment at bed specified as thickness ([m]) or density ([kg/m2])
    character(20) , dimension(:)    , pointer :: namsed     !  Names of all sediment fractions
    character(256), dimension(:)    , pointer :: flsdbd     !  File name containing initial sediment mass at bed
    character(256), dimension(:)    , pointer :: flstcg     !  File name calibration factor on critical shear stress in Van Rijn (2004) uniform values
    ! 
    ! logicals
    !
    logical :: anymud     ! Flag to indicate whether a mud fraction
                          ! is included in the simulation.
    logical :: bsskin     ! Flag to indicate whether a bed stress should be computed
                          ! according to soulsby 2004
    !
    ! characters
    !
    character(256) :: flsdia     ! spatial sediment diameter file (in case of one sediment
                                 ! fraction only)
    character(256) :: flsmdc     ! mud content / mud fraction file (only if mud is not
                                 ! included in simulation)
    character(256) :: flspmc     ! critical mud fraction for non-cohesive behaviour file
end type gd_sedpar

type gd_trapar
    !
    ! doubles
    !
    ! reals
    !
    ! integers
    !
    integer                                 :: max_integers_settle !  Maximum number of integers which can be delivered to shared library
    integer                                 :: max_reals_settle    !  Maximum number of reals which can be delivered to shared library
    integer                                 :: max_strings_settle  !  Maximum number of character strings which can be delivered to shared library
    !
    integer                                 :: max_integers !  Maximum number of integers which can be delivered to shared library
    integer                                 :: max_reals    !  Maximum number of reals which can be delivered to shared library
    integer                                 :: max_strings  !  Maximum number of character strings which can be delivered to shared library
    integer                                 :: npar         !  Maximum number of sediment transport formula parameters
    integer                                 :: nparfld      !  Number of sediment transport formula 2D field parameters
    !
    ! pointers
    !
    character(256), dimension(:)  , pointer :: dll_function_settle !  Name of subroutine in DLL that calculates the Settling velocity
    character(256), dimension(:)  , pointer :: dll_name_settle     !  Name of DLL that contains the Settling velocity subroutine
    integer(pntrsize), dimension(:)  , pointer :: dll_handle_settle   !  Handle of DLL that contains the Settling velocity subroutine
    integer       , dimension(:)  , pointer :: dll_integers_settle !  Input integer array to shared library
    real(hp)      , dimension(:)  , pointer :: dll_reals_settle    !  Input real array to shared library
    character(256), dimension(:)  , pointer :: dll_strings_settle  !  Input character string array to shared library
    character(256), dimension(:)  , pointer :: dll_usrfil_settle   !  Name of input file to be passed to subroutine in DLL
    !
    character(256), dimension(:)  , pointer :: dll_function !  Name of subroutine in DLL that calculates the Sediment transport formula
    character(256), dimension(:)  , pointer :: dll_name     !  Name of DLL that calculates the Sediment transport formula
    integer(pntrsize), dimension(:)  , pointer :: dll_handle   !  DLL containing Sediment transport formula
    integer       , dimension(:)  , pointer :: dll_integers !  Input integer array to shared library
    real(hp)      , dimension(:)  , pointer :: dll_reals    !  Input real array to shared library
    character(256), dimension(:)  , pointer :: dll_strings  !  Input character string array to shared library
    character(256), dimension(:)  , pointer :: dll_usrfil   !  Name of input file to be passed to subroutine in DLL
    character(256), dimension(:)  , pointer :: flstrn       !  Sediment transport formula file names
    integer       , dimension(:)  , pointer :: iform        !  Numbers of sediment transport formulae
    character(256), dimension(:)  , pointer :: name         !  Sediment transport formula names
    real(fp)      , dimension(:,:), pointer :: par          !  Sediment transport formula parameters
    integer       , dimension(:,:), pointer :: iparfld      !  Index of parameter in parfld array (0 if constant)
    real(fp)      , dimension(:,:), pointer :: parfld       !  Sediment transport formula 2D field parameters
    character(256), dimension(:,:), pointer :: parfil       !  Sediment transport formula file names
    ! 
    ! logicals
    !
    !
    ! characters
end type gd_trapar

contains
!
!
!
!============================================================================== 
subroutine initsedpar(gdsedpar)
!!--description-----------------------------------------------------------------
!
!    Function: - Initialize a gd_sedpar data structure.
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type (gd_sedpar)                         , pointer     :: gdsedpar
    !
    ! Local variables
    !
    ! None
!
!! executable statements -------------------------------------------------------
!
    gdsedpar%mdcuni   = 0.0
    gdsedpar%nmudfrac = 0
    gdsedpar%flsdia   = ' '
    gdsedpar%flsmdc   = ' '
    !
    nullify(gdsedpar%sedblock)
    nullify(gdsedpar%rhosol)
    !
    nullify(gdsedpar%logseddia)
    nullify(gdsedpar%logsedsig)
    nullify(gdsedpar%sedd10)
    nullify(gdsedpar%sedd50)
    nullify(gdsedpar%sedd50fld)
    nullify(gdsedpar%sedd90)
    !
    nullify(gdsedpar%cdryb)
    nullify(gdsedpar%dstar)
    nullify(gdsedpar%taucr)
    nullify(gdsedpar%tetacr)
    nullify(gdsedpar%gamflc)
    nullify(gdsedpar%ws0)
    nullify(gdsedpar%wsm)
    nullify(gdsedpar%salmax)
    nullify(gdsedpar%sdbuni)
    nullify(gdsedpar%tcguni)
    nullify(gdsedpar%mudcnt)
    nullify(gdsedpar%pmcrit)
    nullify(gdsedpar%sedtrcfac)
    !
    nullify(gdsedpar%nseddia)
    nullify(gdsedpar%sedtyp)
    !
    nullify(gdsedpar%inisedunit)
    nullify(gdsedpar%namsed)
    nullify(gdsedpar%flsdbd)
    nullify(gdsedpar%flstcg)
end subroutine initsedpar
!
!
!
!========
subroutine clrsedpar(istat     ,gdsedpar  )
!!--description-----------------------------------------------------------------
!
!    Function: - Clean up a gd_sedpar data structure.
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (gd_sedpar)                         , pointer     :: gdsedpar
    integer                                  , intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdsedpar%sedblock))   deallocate(gdsedpar%sedblock,   STAT = istat) ! the actual data tree should be deleted as part of the whole sed_ptr tree.
    if (associated(gdsedpar%rhosol))     deallocate(gdsedpar%rhosol,     STAT = istat)
    !
    if (associated(gdsedpar%logseddia))  deallocate(gdsedpar%logseddia,  STAT = istat)
    if (associated(gdsedpar%logsedsig))  deallocate(gdsedpar%logsedsig,  STAT = istat)
    if (associated(gdsedpar%sedd10))     deallocate(gdsedpar%sedd10,     STAT = istat)
    if (associated(gdsedpar%sedd50))     deallocate(gdsedpar%sedd50,     STAT = istat)
    if (associated(gdsedpar%sedd50fld))  deallocate(gdsedpar%sedd50fld,  STAT = istat)
    if (associated(gdsedpar%sedd90))     deallocate(gdsedpar%sedd90,     STAT = istat)
    !
    if (associated(gdsedpar%cdryb))      deallocate(gdsedpar%cdryb,      STAT = istat)
    if (associated(gdsedpar%dstar))      deallocate(gdsedpar%dstar,      STAT = istat)
    if (associated(gdsedpar%taucr))      deallocate(gdsedpar%taucr,      STAT = istat)
    if (associated(gdsedpar%tetacr))     deallocate(gdsedpar%tetacr,     STAT = istat)
    if (associated(gdsedpar%gamflc))     deallocate(gdsedpar%gamflc,     STAT = istat)
    if (associated(gdsedpar%ws0))        deallocate(gdsedpar%ws0,        STAT = istat)
    if (associated(gdsedpar%wsm))        deallocate(gdsedpar%wsm,        STAT = istat)
    if (associated(gdsedpar%salmax))     deallocate(gdsedpar%salmax,     STAT = istat)
    if (associated(gdsedpar%sdbuni))     deallocate(gdsedpar%sdbuni,     STAT = istat)
    if (associated(gdsedpar%tcguni))     deallocate(gdsedpar%tcguni,     STAT = istat)
    if (associated(gdsedpar%mudcnt))     deallocate(gdsedpar%mudcnt,     STAT = istat)
    if (associated(gdsedpar%pmcrit))     deallocate(gdsedpar%pmcrit,     STAT = istat)
    !
    if (associated(gdsedpar%nseddia))    deallocate(gdsedpar%nseddia,    STAT = istat)
    if (associated(gdsedpar%sedtyp))     deallocate(gdsedpar%sedtyp,     STAT = istat)
    !
    if (associated(gdsedpar%inisedunit)) deallocate(gdsedpar%inisedunit, STAT = istat)
    if (associated(gdsedpar%namsed))     deallocate(gdsedpar%namsed,     STAT = istat)
    if (associated(gdsedpar%flsdbd))     deallocate(gdsedpar%flsdbd,     STAT = istat)
    if (associated(gdsedpar%flstcg))     deallocate(gdsedpar%flstcg,     STAT = istat)
end subroutine clrsedpar
!
!
!
!============================================================================== 
subroutine initmorpar(gdmorpar)
!!--description-----------------------------------------------------------------
!
!    Function: - Initialize a gd_morpar data structure.
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type (gd_morpar)                     , pointer :: gdmorpar
    !
    ! Local variables
    !
    integer                              , pointer :: ihidexp
    integer                              , pointer :: itmor
    integer                              , pointer :: iopkcw
    integer                              , pointer :: iopsus
    integer                              , pointer :: islope
    integer                              , pointer :: morfacpar
    integer                              , pointer :: morfacrec
    integer                              , pointer :: morfactable
    integer                              , pointer :: nxx
    integer                              , pointer :: subiw
    integer                              , pointer :: ttlform
    integer                              , pointer :: telform
    real(hp)                             , pointer :: morft
    real(hp)                             , pointer :: morft0
    real(fp)                             , pointer :: morfac
    real(fp)                             , pointer :: thresh
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rwave
    real(fp)                             , pointer :: alfabs
    real(fp)                             , pointer :: alfabn
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: dzmax
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: tmor
    real(fp)                             , pointer :: thetsd
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: hmaxth
    real(fp)                             , pointer :: bedw
    real(fp)                             , pointer :: rdc
    real(fp)                             , pointer :: rdw
    real(fp)                             , pointer :: espir
    real(fp)                             , pointer :: ashld
    real(fp)                             , pointer :: bshld
    real(fp)                             , pointer :: cshld
    real(fp)                             , pointer :: dshld
    real(fp)                             , pointer :: coulfri
    real(fp)                             , pointer :: flfdrat
    real(fp)                             , pointer :: alfpa
    real(fp)                             , pointer :: thcrpa
    real(fp)                             , pointer :: asklhe
    real(fp)                             , pointer :: mwwjhe
    real(fp)                             , pointer :: pangle
    real(fp)                             , pointer :: fpco
    real(fp)                             , pointer :: factcr
    real(fp)                             , pointer :: ttlalpha
    real(fp)                             , pointer :: ttlmin
    real(fp)                             , pointer :: wetslope
    real(fp)                             , pointer :: avaltime
    real(fp)              , dimension(:) , pointer :: xx
    !
    real(hp)              , dimension(:) , pointer :: mergebuf
    logical                              , pointer :: bedupd
    logical                              , pointer :: cmpupd
    logical                              , pointer :: eqmbcsand
    logical                              , pointer :: eqmbcmud
    logical                              , pointer :: densin
    logical                              , pointer :: rouse
    logical                              , pointer :: epspar
    logical                              , pointer :: updinf
    logical                              , pointer :: neglectentrainment
    logical                              , pointer :: oldmudfrac
    logical                              , pointer :: varyingmorfac
    logical                              , pointer :: multi
    logical                              , pointer :: eulerisoglm
    logical                              , pointer :: glmisoeuler
    character(256)                       , pointer :: bcmfilnam
    character(256)                       , pointer :: flcomp
    character(256)                       , pointer :: mmsyncfilnam
    character(256)                       , pointer :: ttlfil
    character(256)                       , pointer :: telfil
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
    type (cmpbndtype)     , dimension(:) , pointer :: cmpbnd
    !
    real(fp) :: rmissval
    integer  :: imissval
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    morft               => gdmorpar%morft
    morft0              => gdmorpar%morft0
    morfac              => gdmorpar%morfac
    thresh              => gdmorpar%thresh
    aksfac              => gdmorpar%aksfac
    rwave               => gdmorpar%rwave
    alfabs              => gdmorpar%alfabs
    alfabn              => gdmorpar%alfabn
    camax               => gdmorpar%camax
    dzmax               => gdmorpar%dzmax
    sus                 => gdmorpar%sus
    bed                 => gdmorpar%bed
    tmor                => gdmorpar%tmor
    thetsd              => gdmorpar%thetsd
    susw                => gdmorpar%susw
    sedthr              => gdmorpar%sedthr
    hmaxth              => gdmorpar%hmaxth
    bedw                => gdmorpar%bedw
    rdc                 => gdmorpar%rdc
    rdw                 => gdmorpar%rdw
    espir               => gdmorpar%espir
    ashld               => gdmorpar%ashld
    bshld               => gdmorpar%bshld
    cshld               => gdmorpar%cshld
    dshld               => gdmorpar%dshld
    coulfri             => gdmorpar%coulfri
    flfdrat             => gdmorpar%flfdrat
    alfpa               => gdmorpar%alfpa
    thcrpa              => gdmorpar%thcrpa
    asklhe              => gdmorpar%asklhe
    mwwjhe              => gdmorpar%mwwjhe
    ttlalpha            => gdmorpar%ttlalpha
    ttlmin              => gdmorpar%ttlmin
    wetslope            => gdmorpar%wetslope
    avaltime            => gdmorpar%avaltime
    !
    ihidexp             => gdmorpar%ihidexp
    itmor               => gdmorpar%itmor
    iopkcw              => gdmorpar%iopkcw
    iopsus              => gdmorpar%iopsus
    islope              => gdmorpar%islope
    morfacpar           => gdmorpar%morfacpar
    morfacrec           => gdmorpar%morfacrec
    morfactable         => gdmorpar%morfactable
    nxx                 => gdmorpar%nxx
    morbnd              => gdmorpar%morbnd
    cmpbnd              => gdmorpar%cmpbnd
    mergebuf            => gdmorpar%mergebuf
    xx                  => gdmorpar%xx
    ttlform             => gdmorpar%ttlform
    telform             => gdmorpar%telform
    !
    bedupd              => gdmorpar%bedupd
    cmpupd              => gdmorpar%cmpupd
    eqmbcsand           => gdmorpar%eqmbcsand
    eqmbcmud            => gdmorpar%eqmbcmud
    densin              => gdmorpar%densin
    rouse               => gdmorpar%rouse
    epspar              => gdmorpar%epspar
    updinf              => gdmorpar%updinf
    neglectentrainment  => gdmorpar%neglectentrainment
    oldmudfrac          => gdmorpar%oldmudfrac
    varyingmorfac       => gdmorpar%varyingmorfac
    multi               => gdmorpar%multi
    !
    bcmfilnam           => gdmorpar%bcmfilnam
    flcomp              => gdmorpar%flcomp
    mmsyncfilnam        => gdmorpar%mmsyncfilnam
    ttlfil              => gdmorpar%ttlfil
    telfil              => gdmorpar%telfil
    !
    istat = 0
    allocate (gdmorpar%moroutput  , stat = istat)
    allocate (gdmorpar%mornum     , stat = istat)
    allocate (gdmorpar%flufflyr   , stat = istat)
    !
    pangle              => gdmorpar%pangle
    fpco                => gdmorpar%fpco
    factcr              => gdmorpar%factcr
    subiw               => gdmorpar%subiw
    eulerisoglm         => gdmorpar%eulerisoglm
    glmisoeuler         => gdmorpar%glmisoeuler
    !
    gdmorpar%moroutput%transptype  = 2
    !
    gdmorpar%moroutput%aks         = .false.
    gdmorpar%moroutput%cumavg      = .false.
    gdmorpar%moroutput%dg          = .false.
    gdmorpar%moroutput%dgsd        = .false.
    gdmorpar%moroutput%dm          = .false.
    gdmorpar%moroutput%dzduuvv     = .false.
    gdmorpar%moroutput%fixfac      = .false.
    gdmorpar%moroutput%hidexp      = .false.
    gdmorpar%moroutput%frac        = .false.
    gdmorpar%moroutput%mudfrac     = .false.
    gdmorpar%moroutput%sandfrac    = .false.
    gdmorpar%moroutput%percentiles = .false.
    gdmorpar%moroutput%sbcuv       = .false.
    gdmorpar%moroutput%sbcuuvv     = .false.
    gdmorpar%moroutput%sbwuv       = .false.
    gdmorpar%moroutput%sbwuuvv     = .false.
    gdmorpar%moroutput%sswuv       = .false.
    gdmorpar%moroutput%sswuuvv     = .false.
    gdmorpar%moroutput%suvcor      = .false.
    gdmorpar%moroutput%sourcesink  = .false.
    gdmorpar%moroutput%taurat      = .false.
    gdmorpar%moroutput%umod        = .false.
    gdmorpar%moroutput%ustar       = .false.
    gdmorpar%moroutput%uuuvvv      = .false.
    gdmorpar%moroutput%zumod       = .false.
    !
    gdmorpar%mornum%upwindbedload            = .true.
    gdmorpar%mornum%laterallyaveragedbedload = .false.
    gdmorpar%mornum%maximumwaterdepth        = .false.
    !
    rmissval           = -999.0_fp
    imissval           = -999
    !
    morft              = 0.0_hp
    morft0             = 0.0_hp
    !
    bcmfilnam          = ' '
    flcomp             = ' '
    mmsyncfilnam       = ' '
    ttlfil             = ' '
    telfil             = ' '
    !
    morfac             = 1.0_fp
    thresh             = 0.1_fp
    aksfac             = 1.0_fp
    rwave              = 2.0_fp
    alfabs             = 1.0_fp
    alfabn             = 1.5_fp
    camax              = 0.65_fp
    dzmax              = 0.05_fp
    sus                = 1.0_fp
    bed                = 1.0_fp
    tmor               = 0.0_fp
    thetsd             = 0.0_fp
    susw               = 1.0_fp
    sedthr             = 0.5_fp
    hmaxth             = 1.0_fp
    bedw               = 1.0_fp
    factcr             = 1.0_fp    
    rdw                = 0.02_fp
    rdc                = 0.01_fp
    espir              = 0.0_fp
    ashld              = 0.85_fp
    bshld              = 0.5_fp
    cshld              = 0.0_fp
    dshld              = 0.0_fp
    pangle             = 0.0_fp
    fpco               = 1.0_fp
    subiw              = 51
    coulfri            = rmissval
    flfdrat            = rmissval
    alfpa              = rmissval
    thcrpa             = rmissval
    asklhe             = rmissval
    mwwjhe             = rmissval
    ttlalpha           = 0.1_fp
    ttlmin             = 0.0_fp
    wetslope           = 10.0_fp
    avaltime           = 86400.0_fp
    !
    ihidexp            = 1
    itmor              = 0
    iopkcw             = 1
    iopsus             = 0
    islope             = 2
    morfacpar          = imissval
    morfacrec          = imissval
    morfactable        = imissval
    nxx                = 0
    ttlform            = 1
    telform            = 1
    !
    bedupd             = .false.
    cmpupd             = .false.
    eqmbcsand          = .true.
    eqmbcmud           = .false.
    eulerisoglm        = .false.    
    glmisoeuler        = .false.    
    densin             = .true.
    rouse              = .false.
    epspar             = .false.
    updinf             = .false.
    neglectentrainment = .false.
    oldmudfrac         = .false.
    varyingmorfac      = .false.
    multi              = .false.
    !
    nullify(gdmorpar%morbnd)
    nullify(gdmorpar%cmpbnd)
    nullify(gdmorpar%xx)
    nullify(gdmorpar%mergebuf)
    !
    call initfluffy(gdmorpar%flufflyr)
end subroutine initmorpar
!
!
!
!============================================================================== 
subroutine initfluffy(flufflyr)
!!--description-----------------------------------------------------------------
!
!    Function: - Initialize a fluff layer data structure.
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (fluffy_type)                   , pointer     :: flufflyr
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
    flufflyr%iflufflyr = 0
    !
    nullify(flufflyr%mfluff)
    nullify(flufflyr%bfluff0)
    nullify(flufflyr%bfluff1)
    nullify(flufflyr%depfac)
    !
    flufflyr%bfluff0_fil = ' '
    flufflyr%bfluff1_fil = ' '
    flufflyr%depfac_fil  = ' '
end subroutine initfluffy
!
!
!
!============================================================================== 
function allocfluffy(flufflyr, lsed, nmlb, nmub) result(istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Allocate a fluff layer data structure.
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (fluffy_type)                   , pointer     :: flufflyr
    integer                                            :: istat
    integer                              , intent(in)  :: lsed
    integer                              , intent(in)  :: nmlb
    integer                              , intent(in)  :: nmub
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
                  allocate(flufflyr%mfluff(lsed,nmlb:nmub), stat = istat)
    if (istat==0) allocate(flufflyr%sinkf(lsed,nmlb:nmub), stat = istat)
    if (istat==0) allocate(flufflyr%sourf(lsed,nmlb:nmub), stat = istat)
    !
    select case (flufflyr%iflufflyr)
    case (1)
       if (istat==0) allocate(flufflyr%bfluff0(lsed,nmlb:nmub), stat = istat)
       if (istat==0) allocate(flufflyr%bfluff1(lsed,nmlb:nmub), stat = istat)
    case (2)
       if (istat==0) allocate(flufflyr%depfac(lsed,nmlb:nmub), stat = istat)
    endselect
end function allocfluffy
!
!
!
!============================================================================== 
subroutine clrfluffy(istat, flufflyr)
!!--description-----------------------------------------------------------------
!
!    Function: - Clean up a fluff layer data structure.
!
!!--declarations----------------------------------------------------------------
    implicit none
    !
    ! Function/routine arguments
    !
    type (fluffy_type)                   , pointer     :: flufflyr
    integer                              , intent(out) :: istat
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
    flufflyr%iflufflyr = 0
    !
    if (associated(flufflyr%mfluff))      deallocate(flufflyr%mfluff,      STAT = istat)
    if (associated(flufflyr%bfluff0))     deallocate(flufflyr%bfluff0,     STAT = istat)
    if (associated(flufflyr%bfluff1))     deallocate(flufflyr%bfluff1,     STAT = istat)
    if (associated(flufflyr%depfac))      deallocate(flufflyr%depfac,      STAT = istat)
    if (associated(flufflyr%sinkf))       deallocate(flufflyr%sinkf,       STAT = istat)
    if (associated(flufflyr%sourf))       deallocate(flufflyr%sourf,       STAT = istat)
end subroutine clrfluffy
!
!
!
!============================================================================== 
subroutine clrmorpar(istat, gdmorpar)
!!--description-----------------------------------------------------------------
!
!    Function: - Clean up a gd_morpar data structure.
!
!!--declarations----------------------------------------------------------------
    use table_handles
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type (gd_morpar)                     , pointer     :: gdmorpar
    integer                              , intent(out) :: istat
    !
    ! Local variables
    !
    integer                                        :: i
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
!
!! executable statements -------------------------------------------------------
!
    morbnd              => gdmorpar%morbnd
    !
    if (associated(gdmorpar%morbnd)) then
       do i = 1, size(gdmorpar%morbnd)
          if (associated(morbnd(i)%idir))      deallocate(morbnd(i)%idir,      STAT = istat)
          if (associated(morbnd(i)%nm))        deallocate(morbnd(i)%nm,        STAT = istat)
          if (associated(morbnd(i)%nxmx))      deallocate(morbnd(i)%nxmx,      STAT = istat)
          if (associated(morbnd(i)%alfa_dist)) deallocate(morbnd(i)%alfa_dist, STAT = istat)
          if (associated(morbnd(i)%alfa_mag))  deallocate(morbnd(i)%alfa_mag,  STAT = istat)
       enddo
       deallocate(gdmorpar%morbnd, STAT = istat)
    endif
    if (associated(gdmorpar%cmpbnd))    deallocate(gdmorpar%cmpbnd,    STAT = istat)
    if (associated(gdmorpar%xx))        deallocate(gdmorpar%xx,        STAT = istat)
    if (associated(gdmorpar%mergebuf))  deallocate(gdmorpar%mergebuf,  STAT = istat)
    if (associated(gdmorpar%moroutput)) deallocate(gdmorpar%moroutput, STAT = istat)
    if (associated(gdmorpar%mornum))    deallocate(gdmorpar%mornum,    STAT = istat)
    call cleartable(gdmorpar%bcmfile)
    call cleartable(gdmorpar%morfacfile)
    if (associated(gdmorpar%flufflyr)) then
        call clrfluffy(istat, gdmorpar%flufflyr)
        deallocate(gdmorpar%flufflyr, STAT = istat)
    endif
    !
end subroutine clrmorpar
!
!
!
!============================================================================== 
subroutine inittrapar(gdtrapar  )
!!--description-----------------------------------------------------------------
!
!    Function: - Initialize a gd_trapar data structure.
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type (gd_trapar)                     , pointer     :: gdtrapar
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    !
    ! Note: 30 is hardcoded in sediment transport formulae
    !
    gdtrapar%npar    = 30
    gdtrapar%nparfld = 0
    !
    nullify(gdtrapar%dll_function_settle)
    nullify(gdtrapar%dll_name_settle)
    nullify(gdtrapar%dll_handle_settle)
    nullify(gdtrapar%dll_integers_settle)
    nullify(gdtrapar%dll_reals_settle)
    nullify(gdtrapar%dll_strings_settle)
    nullify(gdtrapar%dll_usrfil_settle)
    nullify(gdtrapar%dll_function)
    nullify(gdtrapar%dll_name)
    nullify(gdtrapar%dll_handle)
    nullify(gdtrapar%dll_integers)
    nullify(gdtrapar%dll_reals)
    nullify(gdtrapar%dll_strings)
    nullify(gdtrapar%dll_usrfil)
    nullify(gdtrapar%flstrn)
    nullify(gdtrapar%iform)
    nullify(gdtrapar%name)
    nullify(gdtrapar%par)
    nullify(gdtrapar%parfil)
    nullify(gdtrapar%iparfld)
    nullify(gdtrapar%parfld)
end subroutine inittrapar
!
!
!
!============================================================================== 
subroutine clrtrapar(istat     ,gdtrapar  )
!!--description-----------------------------------------------------------------
!
!    Function: - Clean up a gd_trapar data structure.
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type (gd_trapar)                     , pointer     :: gdtrapar
    integer                              , intent(out) :: istat
    !
    ! Local variables
    !
    integer                     :: i
    integer(pntrsize), external :: close_shared_library
    integer(pntrsize)           :: error
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdtrapar%dll_handle)) then
       do i = 1,size(gdtrapar%dll_handle)
          if (gdtrapar%dll_handle_settle(i) /= 0) then
             error = close_shared_library(gdtrapar%dll_handle_settle(i))
          endif
          if (gdtrapar%dll_handle(i) /= 0) then
             error = close_shared_library(gdtrapar%dll_handle(i))
          endif
       enddo
    endif
    !
    if (associated(gdtrapar%dll_function_settle)) deallocate(gdtrapar%dll_function_settle, STAT = istat)
    if (associated(gdtrapar%dll_name_settle    )) deallocate(gdtrapar%dll_name_settle    , STAT = istat)
    if (associated(gdtrapar%dll_handle_settle  )) deallocate(gdtrapar%dll_handle_settle  , STAT = istat)
    if (associated(gdtrapar%dll_integers_settle)) deallocate(gdtrapar%dll_integers_settle, STAT = istat)
    if (associated(gdtrapar%dll_reals_settle   )) deallocate(gdtrapar%dll_reals_settle   , STAT = istat)
    if (associated(gdtrapar%dll_strings_settle )) deallocate(gdtrapar%dll_strings_settle , STAT = istat)
    if (associated(gdtrapar%dll_usrfil_settle  )) deallocate(gdtrapar%dll_usrfil_settle  , STAT = istat)
    !
    if (associated(gdtrapar%dll_function)) deallocate(gdtrapar%dll_function, STAT = istat)
    if (associated(gdtrapar%dll_name    )) deallocate(gdtrapar%dll_name    , STAT = istat)
    if (associated(gdtrapar%dll_handle  )) deallocate(gdtrapar%dll_handle  , STAT = istat)
    if (associated(gdtrapar%dll_integers)) deallocate(gdtrapar%dll_integers, STAT = istat)
    if (associated(gdtrapar%dll_reals   )) deallocate(gdtrapar%dll_reals   , STAT = istat)
    if (associated(gdtrapar%dll_strings )) deallocate(gdtrapar%dll_strings , STAT = istat)
    if (associated(gdtrapar%dll_usrfil  )) deallocate(gdtrapar%dll_usrfil  , STAT = istat)
    if (associated(gdtrapar%flstrn      )) deallocate(gdtrapar%flstrn      , STAT = istat)
    if (associated(gdtrapar%iform       )) deallocate(gdtrapar%iform       , STAT = istat)
    if (associated(gdtrapar%name        )) deallocate(gdtrapar%name        , STAT = istat)
    if (associated(gdtrapar%par         )) deallocate(gdtrapar%par         , STAT = istat)
    if (associated(gdtrapar%parfil      )) deallocate(gdtrapar%parfil      , STAT = istat)
    if (associated(gdtrapar%iparfld     )) deallocate(gdtrapar%iparfld     , STAT = istat)
    if (associated(gdtrapar%parfld      )) deallocate(gdtrapar%parfld      , STAT = istat)
end subroutine clrtrapar

end module morphology_data_module
