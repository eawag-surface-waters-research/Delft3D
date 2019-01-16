module M_friction                                 !< friction parameters, (more to follow)
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2019.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------

 integer                         :: mxengpar      !< dimension of engpar
 double precision, allocatable   :: engpar(:)     !< Engelund-Hansen parameters

 character(len=40), allocatable  :: exresid(:)    !< ids of Extra Resistances
 character(len=40), allocatable  :: exresnm(:)    !< Names of Extra Resistances

end module M_friction

   
module m_Roughness
   use MessageHandling
   use m_alloc
   use m_branch
   use m_tables
   use m_spatial_data
   use m_hash_search
   
   implicit none
   
   private
   
   public realloc
   public dealloc
   public GetChezy
   public getFrictionValue
   public flengrpr
   
!   public setCrossSectionIncrement
!   
   interface realloc
      module procedure reallocRoughness
   end interface
   
   interface dealloc
      module procedure deallocRoughness
   end interface dealloc
   
 
   interface flengrpr
      module procedure flengrprReal
      module procedure flengrprDouble
   end interface 
   
   !> Roughness definition
   type, public :: t_Roughness
   
      character(len=idLen)              :: id            !< section id/name
       
      integer                           :: iSection = 0  !> Section Type
                                                         !! 1 = Main
                                                         !! 2 = Floodplain1
                                                         !! 3 = Floodplan2
                                                         !! Any other = Other section (Default 0)

      ! branch oriented data
      integer, pointer                  :: rgh_type_pos(:)   !< Size = network%brs%count        
      integer, pointer                  :: fun_type_pos(:)           
      integer, pointer                  :: rgh_type_neg(:)
      integer, pointer                  :: fun_type_neg(:)

      integer                           :: spd_pos_idx    ! Index to Spatial Data
      integer                           :: spd_neg_idx    ! Index to Spatial Data
      
   end type
   
   type, public :: t_RoughnessSet
      integer                                           :: Size = 0
      integer                                           :: growsBy = 2000
      integer                                           :: Count= 0
      type(t_Roughness), pointer, dimension(:)          :: rough
      type(t_tableSet)                                  :: tables
      type(t_hashlist)                                  :: hashlist
   end type t_RoughnessSet

   integer, parameter, public                           :: R_FunctionConstant = 0
   integer, parameter, public                           :: R_FunctionDischarge = 1
   integer, parameter, public                           :: R_FunctionLevel = 2
   integer, parameter, public                           :: R_Chezy = 1
   integer, parameter, public                           :: R_Manning = 4
   integer, parameter, public                           :: R_Nikuradse = 5
   integer, parameter, public                           :: R_Strickler = 6
   integer, parameter, public                           :: R_WhiteColebrook = 7
   integer, parameter, public                           :: R_BosBijkerk = 9
   double precision, parameter, public                  :: sixth = 1.d0/6.d0
   double precision, parameter, public                  :: third = 1.d0/3.d0
   double precision, parameter, public                  :: chlim = 0.001d0

contains
   
subroutine reallocRoughness(rgs)
   ! Modules
   
   implicit none

   ! Input/output parameters
   type(t_RoughnessSet)           :: rgs
   
   ! Local variables

   ! Program code

   ! Local variables
   type(t_Roughness), pointer, dimension(:)    :: oldrough
   
   ! Program code
   
   if (rgs%Size > 0) then
      allocate(oldrough(rgs%Size))
      oldrough=rgs%rough
      deallocate(rgs%rough)
   endif
   
   if (rgs%growsBy <=0) then
      rgs%growsBy = 200
   endif
   allocate(rgs%rough(rgs%Size+rgs%growsBy))
   
   if (rgs%size > 0) then
      rgs%rough(1:rgs%size) = oldrough(1:rgs%size)
      deallocate(oldrough)
   endif
   
   rgs%Size = rgs%Size+rgs%growsBy

end subroutine reallocRoughness
   
subroutine deallocRoughness(rgs)
   ! Modules
   
   implicit none

   ! Input/output parameters
   type(t_RoughnessSet)    :: rgs
   
   ! Local variables
   integer                 :: i
   
   ! Program code
   if (rgs%count > 0) then
      do i = 1, rgs%count

         if (associated(rgs%rough(i)%rgh_type_pos))  deallocate(rgs%rough(i)%rgh_type_pos) 
         if (associated(rgs%rough(i)%fun_type_pos))  deallocate(rgs%rough(i)%fun_type_pos)           
         if (associated(rgs%rough(i)%rgh_type_neg))  deallocate(rgs%rough(i)%rgh_type_neg)
         if (associated(rgs%rough(i)%fun_type_neg))  deallocate(rgs%rough(i)%fun_type_neg)
      
         rgs%rough(i)%rgh_type_pos  => null()
         rgs%rough(i)%fun_type_pos  => null()
         rgs%rough(i)%rgh_type_neg  => null()
         rgs%rough(i)%fun_type_neg  => null()
         
      enddo
      
      call dealloc(rgs%tables)
      call dealloc(rgs%hashlist)
         
      deallocate(rgs%rough)
      rgs%rough => null()
         
      rgs%size=0
      rgs%count=0

   endif

end subroutine deallocRoughness

double precision function getFrictionValue(rgs, spData, ibranch, section, igrid, h, q, u, r, d)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !=======================================================================
    !                       Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:
    !
    ! Function:           getFrictionValue, replacement of old FLCHZT (FLow CHeZy Friction coeff)
    !
    ! Module description: Chezy coefficient is computed for a certain gridpoint
    !
    !
    !
    !     update information
    !     person                    date
    !     Kuipers                   5-9-2001
    !     Van Putten                11-8-2011
    !
    !     Use stored table counters
    !
    !
    !
    !
    !     Declaration of Parameters:
    !

    implicit none
!
! Global variables
!
    type(t_RoughnessSet), intent(in)        :: rgs
    type(t_spatial_dataSet), intent(in)     :: spData
    integer, intent(in)                     :: igrid       !< gridpoint index
    integer, intent(in)                     :: ibranch     !< branch index
    integer, intent(in)                     :: section     !< section index (0=main, 1=Flood plane 1, 2=Flood plane 2)
    double precision, intent(in)            :: d           !< water depth
    double precision, intent(in)            :: h           !< water level
    double precision, intent(in)            :: q           !< discharge
    double precision, intent(in)            :: r           !< hydraulic radius
    double precision, intent(in)            :: u           !< velocity
!
!
! Local variables
!
    integer                         :: isec1
    double precision                :: cpar
    double precision                :: dep
    double precision                :: rad
    type(t_Roughness), pointer      :: rgh 
    type(t_spatial_data), pointer   :: values
    integer, dimension(:), pointer  :: rgh_type
    integer, dimension(:), pointer  :: fun_type

    !     Explanation:
    !     -----------
    !
    !     1. Each Chezy formula, apart from Engelund bed friction, is defined
    !        by 1 constant parameter. This constant is stored in bfricp.
    !        An exception is the Engelund bed friction defined by 10 parameters.
    !     2. For the Engelund bed friction the specific parameters are stored
    !        in the array engpar.
    !
    !
    !     Prevention against zero hydraulic radius and depth
    !
    rad = max(r, 1.d-6)
    dep = max(d, 1.d-6)
    !
    isec1 = section
    !
    !     Formulation = .not. Engelund
    !
    
    rgh => rgs%rough(isec1)
    
    if (q > 0d0 .or. .not. associated(rgh%rgh_type_neg)) then
       values    => spData%quant(rgh%spd_pos_idx)
       rgh_type  => rgh%rgh_type_pos 
       fun_type  => rgh%fun_type_pos 
    else 
       values    => spData%quant(rgh%spd_neg_idx)
       rgh_type  => rgh%rgh_type_neg 
       fun_type  => rgh%fun_type_neg 
    endif   
    !        Positive flow
    !
    !        Roughness function of discharge depending on flow direction
    !
    if (fun_type(ibranch) == R_FunctionDischarge) then
       cpar = interpolate(values%tables%tb(values%tblIndex(igrid))%table,  abs(q))
    !
    !        Roughness function of water level depending on flow direction
    !
    elseif (fun_type(ibranch) == R_FunctionLevel) then
       cpar = interpolate(values%tables%tb(values%tblIndex(igrid))%table,  h)
    !
    !        Roughness constant depending on flow direction
    !
    else
       cpar = values%values(igrid)
    endif

    getFrictionValue = GetChezy(rgh_type(ibranch), cpar, rad, dep, u)
end function getFrictionValue

double precision function GetChezy(frictType, cpar, rad, dep, u)

   implicit none
   double precision, intent(in)   :: dep                     !< water depth
   !double precision, intent(in)   :: h                     !< water level
   !double precision, intent(in)   :: q                     !< discharge
   double precision, intent(in)   :: rad                     !< hydraulic radius
   double precision, intent(in)   :: cpar
   double precision, intent(in)   :: u                     !< velocity
   integer, intent(in)            :: frictType             !< friction type

!
   !
   !     Declaration of Parameters:
   !
   !

   select case(frictType)
   case (7) 
      !
      !           Nikuradze-formula
      !           [Doc. S-FO-001.5KV  Eq. 3-1]
      !
      GetChezy = 18.0d0*log10(12.d0*rad/cpar)
   !
   case (4) 
      !
      !           Manning-formula
      !           [Doc. S-FO-001.5KV  Eq. 3-2]
      !
      GetChezy = rad**sixth/cpar
   !
   case (5) 
      !
      !           Strickler-1 formula
      !           [Doc. S-FO-001.5KV  Eq. 3-3]
      !
      GetChezy = 25.0d0*(rad/cpar)**sixth
   !
   case (6) 
      !
      !           Strickler-2 formula
      !           [Doc. S-FO-001.5KV  Eq. 3-4]
      !
      GetChezy = cpar*rad**sixth
   !
   case (9) 
      !
      !           Bos Bijkerk formula
      !           [See Technical Reference - for DLG]
      !
      GetChezy = cpar*dep**third*rad**sixth
   !
   case (8)
      !
      !        Engelund-like roughness predictor
      !        [Doc. S-FO-001.5KV  Eq. 3-5]
      !
      !                      d90
      call flengrpr(cpar, u, rad, GetChezy)
   case default
      !
      !           Chezy value (kode 0 or 1)
      !
      GetChezy = cpar
   end select
   !
   !
   !        The obtained Chezy value may never be less then 10. in
   !        the main channel (Civil engineers common sense)
   !        and less then 1. in the flood plains
   !
   if (GetChezy<chlim) then
      GetChezy = chlim
   endif

end function GetChezy

subroutine flengrprDouble(d90, u, hrad, chezy)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use m_friction
    implicit none
!
! Global variables
!
    double precision, intent(out)              :: chezy
    double precision, intent(in)               :: d90
    double precision, intent(in)               :: hrad
    double precision, intent(in)               :: u
!
!
! Local variables
!
    logical                        :: high
    logical                        :: low
    logical                        :: moder
    double precision               :: as1
    double precision               :: as11
    double precision               :: as12
    double precision               :: as2
    double precision               :: as21
    double precision               :: as22
    double precision               :: as3
    double precision               :: as31
    double precision               :: as32
    double precision               :: c90
    double precision               :: deltad
    double precision               :: ths
    double precision               :: thsg
    real, dimension(2)             :: theng
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                       Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer
    !
    ! Module:             ENGRPR (ENGelund Roughness PRedictor)
    !
    ! Module description: Calculation of the Chezy value in case of the
    !                     Engelund roughness predictor. (only applicable in
    !                     the main section)
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  5 chezy             O  Chezy value
    !  2 d90               I  D90 value.
    !  1 engpar(9)         I  9 Engelund parameters:
    !                         (1) = Parameter DELTA-d.
    !                         (2) = Parameter THETA-Eng1.
    !                         (3) = Parameter THETA-Eng2.
    !                         (4) = Parameter as11.
    !                         (5) = Parameter as21.
    !                         (6) = Parameter as31.
    !                         (7) = Parameter as12.
    !                         (8) = Parameter as22.
    !                         (9) = Parameter as32.
    !  4 hrad              I  Hydraulic radius
    !  3 u                 I  Velocity
    !
    !
    !     update information
    !     person                    date
    !     Kuipers                   5-9-2001
    !
    !
    !     Declaration of Parameters:
    !
    !
    !     Declaration of local variables:
    !
    !
    !     Engelund-like roughness predictor
    !
    !     Extraction of 'Engelund' parameters from -engpar-
    !
    !     theng(1) : Shields parameter on threshold of sediment movement
    !     theng(2) : Shields parameter at transition point from dunes to
    !                   flat bed
    !     deltad   : relative density of bed material
    !
    deltad = engpar(1)
    theng(1) = engpar(2)
    theng(2) = engpar(3)
    as11 = engpar(4)
    as21 = engpar(5)
    as31 = engpar(6)
    as12 = engpar(7)
    as22 = engpar(8)
    as32 = engpar(9)
    !
    !     Compute C90 = Chezy coeff. w.r.t. grains
    !
    c90 = 18.0*log10(4.0*hrad/d90)
    !
    !     Compute thsg = Shields parameter w.r.t. grains
    !
    thsg = u*u/(c90*c90*deltad*d90)
    !
    !     Determine flow condition
    !
    !     low   = low with flat bed
    !     moder = moderate with dunes
    !     high  = high flow with anti-dunes
    !     thsg     : Shields parameter w.r.t. grains
    !
    !                 thsg  < theng(1) : low flow       [flat bed  ]
    !     theng(1) <  thsg  < theng(2) : moderate flow  [dunes     ]
    !                 thsh  > theng(2) : high flow      [anti-dunes]
    !
    low = thsg<=theng(1)
    moder = thsg>theng(1) .and. thsg<=theng(2)
    high = thsg>theng(2)
    !
    if (low) then
       !
       !        Low flow
       !
       as1 = 0.0d0
       as2 = 1.0d0
       as3 = 0.0d0
    !
    elseif (moder) then
       !
       !        Moderate flow
       !
       as1 = as11
       as2 = as21
       as3 = as31
    !
    elseif (high) then
       !
       !        High flow
       !
       as1 = as12
       as2 = as22
       as3 = as32
    !
    else
    endif
    !
    !     Compute Shields parameter thsh
    !
    ths = as1*thsg*thsg + as2*thsg + as3
    !
    !     Compute Engelund-like roughness predictor
    !
    if (low) then
       chezy = c90
    else
       chezy = c90*sqrt(thsg/ths)
    endif
end subroutine flengrprDouble

subroutine flengrprReal(d90, u, hrad, chezy)
   real, intent(in) :: d90
   real, intent(in) :: u
   real, intent(in) :: hrad
   real, intent(out) :: chezy
   
   double precision C
   
   call flengrprDouble(dble(d90), dble(u), dble(hrad), C)
   
   chezy = C
end subroutine flengrprReal

end module m_Roughness
