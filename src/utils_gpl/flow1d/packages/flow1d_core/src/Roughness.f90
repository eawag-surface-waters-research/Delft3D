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
   use m_tablematrices
   use m_spatial_data
   use m_hash_search
   
   implicit none
   
   private
   
   public realloc
   public dealloc
   public GetChezy
   public flengrpr
   public setVonkar
   public frictiontype_v1_to_new
   
   double precision :: vonkar      = 0.41        !< von Karman constant ()
   double precision :: ag          = 9.81d0     !< gravity acceleration
   double precision :: sag                      !< = sqrt(ag)  
   double precision :: ee                       !< natural e ()

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
   
   !> roughness definition
   
   !> Roughness definition
   type, public :: t_Roughness                           !< Derived type for roughness data 
   
      character(len=idLen)              :: id            !< section id/name
       
                                                         !> Section Type
                                                         !! 1 = Main
                                                         !! 2 = Floodplain1
                                                         !! 3 = Floodplan2
                                                         !! Any other = Other section (Default 0)
      integer                           :: iSection = 0
      type(t_tablematrix), pointer      :: table(:)          !< table for space and parameter dependend roughness
      logical                           :: useGlobalFriction !< Flag indicates to use frictionValue and frictionType or to use the table
      double precision                  :: frictionValue     !< Global friction Value
      integer                           :: frictionType      !< Global friction Type
      integer, pointer                  :: rgh_type_pos(:)   !< Roughness type for positive flow direction at a branch
      integer, pointer                  :: fun_type_pos(:)   !< Roughness parameter value for positive flow direction at a branch     
      ! branch oriented data (obsolete)
      integer, pointer                  :: rgh_type_neg(:)   !< Roughness type for negative flow direction at a branch
      integer, pointer                  :: fun_type_neg(:)   !< Roughness parameter value for negative flow direction at a branch

      integer                           :: spd_pos_idx       !< Index to Spatial Data for positive flow direction parameter values
      integer                           :: spd_neg_idx       !< Index to Spatial Data for negative flow direction parameter values
      
   end type
   
   type, public :: t_RoughnessSet                                           !< Roughness set for roughness sections
      integer                                           :: version =-1      !< Version number for roughness input
      integer                                           :: Size = 0         !< Current size
      integer                                           :: growsBy = 2000   !< Increment for growing array
      integer                                           :: Count= 0         !< Number of elements in array
      type(t_Roughness), pointer, dimension(:)          :: rough            !< Array containing roughness sections
      type(t_tableSet)                                  :: tables           !< Array with tables for flow or water level dependend parameter values
      type(t_hashlist)                                  :: hashlist         !< hashlist for fast searching.
   end type t_RoughnessSet

   integer, parameter, public                           :: R_FunctionConstant = 0      !< Constant type roughness function
   integer, parameter, public                           :: R_FunctionDischarge = 1     !< Discharge dependend roughness 
   integer, parameter, public                           :: R_FunctionLevel = 2         !< Water level dependend roughness
   integer, parameter, public                           :: R_Chezy = 1                 !< Chezy type roughness
   integer, parameter, public                           :: R_Manning = 4               !< Manning  roughness formula
   integer, parameter, public                           :: R_Nikuradse = 5             !< Nikuradse roughness formula
   integer, parameter, public                           :: R_Strickler = 6             !< Strickler roughness formula
   integer, parameter, public                           :: R_WhiteColebrook = 7        !< White Colebrook roughness formula
   integer, parameter, public                           :: R_BosBijkerk = 9            !< Bos en Bijkerk roughness formula 
   double precision, parameter, public                  :: sixth = 1.d0/6.d0           !< 1/6
   double precision, parameter, public                  :: third = 1.d0/3.d0           !< 1/3
   double precision, parameter, public                  :: chlim = 0.001d0             !< Lowest Chezy value

contains
   
subroutine setVonkar(vonkarIn)
   double precision, intent(in) :: vonkarIn
   
   vonkar = vonkarIn
end subroutine setVonkar

   
!> Reallocate roughness array, while keeping the original values in place   
subroutine reallocRoughness(rgs)
   ! Modules
   
   implicit none

   ! Input/output parameters
   type(t_RoughnessSet), intent(inout)           :: rgs    !< roughness set
   
   ! Local variables
   type(t_Roughness), pointer, dimension(:)    :: oldrough
   
   ! Program code
   
   if (rgs%Size > 0) then
      allocate(oldrough(rgs%Size))
      oldrough=rgs%rough
      deallocate(rgs%rough)
   else
      ! set some parameters (not the correct location)
      ee = exp(1d0)
      sag = sqrt(ag)
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
   
!> Deallocate roughness set   
subroutine deallocRoughness(rgs)
   ! Modules
   
   implicit none

   ! Input/output parameters
   type(t_RoughnessSet), intent(inout)    :: rgs          !< roughness set
   
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

! Function getFrictionValue is moved from Roughness.f90 to CorssSections.f90 to avoid circular references

integer function frictiontype_v1_to_new(frictionType)
   integer, intent(in) :: frictionType
   
   select case(frictionTYpe)
   case(1)
      ! Chezy
      frictiontype_v1_to_new = 0
   case (4) 
      ! Manning-formula
      frictiontype_v1_to_new = 1
   case (5) 
      !           Strickler-1 formula
      frictiontype_v1_to_new = 7
   case (6) 
      !           Strickler-2 formula
      frictiontype_v1_to_new = 8
   case (7) 
      !           Nikuradze-formula == White Colebrook Waqua style
      frictiontype_v1_to_new = 3
   case (8)
      !        Engelund-like roughness predictor
      frictiontype_v1_to_new = 10
   case (9) 
      !           Bos Bijkerk formula
      frictiontype_v1_to_new = 9
   end select
   
end function frictiontype_v1_to_new


!> Get the Chezy value for a given friction type and parameter value
double precision function GetChezy(frictType, cpar, rad, dep, u)

   implicit none
   double precision, intent(in)   :: dep                   !< water depth
   double precision, intent(in)   :: rad                   !< hydraulic radius
   double precision, intent(in)   :: cpar                  !< parameter value
   double precision, intent(in)   :: u                     !< velocity
   integer, intent(in)            :: frictType             !< friction type
   !
   !     Declaration of Parameters:
   !
   double precision        :: rad0
   double precision        :: z0
   double precision        :: sqcf
   
   rad0 = max(rad,1d-4)
   
   select case(frictType)
   case(0)
      !           Chezy value
      GetChezy = cpar
   case (1) 
      !           Manning-formula
      GetChezy = rad0**sixth/cpar
   case (2) 
     !            White Colebrook Delft3d
     z0        = min( cpar / 30d0 , rad0*0.3d0)
     sqcf      = vonkar/log( rad0/(ee*z0) )
     getChezy  = sag/sqcf
   case (3) 
      !           White Colebrook WAQUA / Nikuradze-formula
      GetChezy = 18.0d0*log10(12.d0*rad0/cpar)
   case (7) 
      !           Strickler-1 formula
      GetChezy = 25.0d0*(rad0/cpar)**sixth
   case (8) 
      !           Strickler-2 formula
      GetChezy = cpar*rad0**sixth
   case (9) 
      !           Bos Bijkerk formula
      GetChezy = cpar*dep**third*rad0**sixth
   case (10)
      !        Engelund-like roughness predictor
      call flengrpr(cpar, u, rad0, GetChezy)
   case default
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

!> Get the Chezy value, using the Engelund roughness predictor
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
    double precision, intent(out)              :: chezy   !< Roughness value
    double precision, intent(in)               :: d90     !< d90 parameter value
    double precision, intent(in)               :: hrad    !< hydraulic radius
    double precision, intent(in)               :: u       !< flow velocity
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

!> Get the Chezy value, using the Engelund roughness predictor
subroutine flengrprReal(d90, u, hrad, chezy)
   real, intent(in) :: d90          !< Roughness value
   real, intent(in) :: u            !< d90 parameter value
   real, intent(in) :: hrad         !< hydraulic radius
   real, intent(out) :: chezy       !< flow velocity
   
   double precision C
   
   call flengrprDouble(dble(d90), dble(u), dble(hrad), C)
   
   chezy = C
end subroutine flengrprReal

end module m_Roughness
