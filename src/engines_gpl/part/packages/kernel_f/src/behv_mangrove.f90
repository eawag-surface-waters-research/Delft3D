!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

module behv_mangrove_mod
!
!  data definition module(s)
!
use precision_part          ! single/double precision
use timers
!
!  module procedure(s)
!
use intpltd_diurnal_mod            ! explicit interface
use intpltd_divelimit_mod          ! explicit interface
use intpltd_motility_mod           ! explicit interface
!
implicit none

contains
    subroutine behv_mangrove ( btype    , hbtype  , v_swim , d_swim   , n        ,   &
                             m        , nmax    , mmax   , mnmaxk , lgrid       ,   &
                             lgrid2   , lgrid3  , nosegl , wpart  , ipart       ,   &
                             wsettl   , k       , kpart  , zpart  , xpart       ,   &
                             ypart    , nolay   , idelt  , day    , phase_diurn ,   &
                             ebb_flow , flow    , depth  , salin1 , temper1     ,   &
                             vol1     , vol2    , vel1   , vel2   , fstage      ,   &
                             ztop     , zlevel  , zdepth , zbot   , buoy        ,   &
                             vzact    , vz      , buoy1  , buoy2  , ztop1       ,   &
                             ztop2    , zbot1   , zbot2  , vzact1 , vzact2      ,   &
                             vswim1   , vswim2  , iseg   , lunrep , angle  )

        ! function  : Mangrove (Avicennia officinalis) specific behaviour collected 
        !              
        

        ! arguments :

        integer(ip), intent(in)     :: lunrep              ! report file
        integer(ip), intent(in)     :: nosegl              ! number segments per layer
        integer(ip), intent(in)     :: nolay               ! number of layers in calculation
        integer(ip), intent(in)     :: nmax                ! first grid dimension
        integer(ip), intent(in)     :: mmax                ! second grid dimension
        integer(ip), intent(in)     :: mnmaxk              ! total number of active grid cells
        integer(ip), pointer        :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
        integer(ip), pointer        :: lgrid2( : , : )     ! total grid
        integer(ip), pointer        :: lgrid3( : , : )     ! original grid (conc array)

        integer(ip), pointer        :: kpart ( : )         ! third grid index of the particles
        real   (sp), pointer        :: xpart ( : )         ! x-value (0.0-1.0) first  direction within grid cell
        real   (sp), pointer        :: ypart ( : )         ! y-value (0.0-1.0) second direction within grid cell
        real   (sp), pointer        :: zpart ( : )         ! z-value (0.0-1.0) third  direction within grid cell
        real   (sp), pointer, intent(in)   :: wpart ( : , :)      ! weight factors of the subs per particle

        real   (sp), pointer        :: wsettl( : )         ! settling per particle

        real   (sp), pointer        :: angle ( : )         ! angle with horizontal
        real   (sp), pointer        :: depth( : )          ! total depth of the cells from top to bottom
        real   (sp), pointer        :: vol1  ( : )         ! volume begin hydr step
        real   (sp), pointer        :: vol2  ( : )         ! volume end hydr step
        real   (sp), pointer        :: flow  ( : )         ! all flows
        real   (sp), pointer        :: salin1( : )         ! salinity segment numbering
        real   (sp), pointer        :: temper1( : )        ! temperature segment numbering
        real   (sp), pointer        :: v_swim( : )         ! horizontal swimming velocity m/
        real   (sp), pointer        :: d_swim( : )         ! horizontal swimming direction (degree)
        real   (sp), pointer        :: vel1  ( : )         ! velocity begin hydr step
        real   (sp), pointer        :: vel2  ( : )         ! velocity end hydr step

        ! from input (const)

        integer                     :: iseg                ! iseg

        integer , pointer           :: btype(:)            ! behaviour type
        real(sp), pointer           :: buoy1(:)            ! buoyancy begin stage
        real(sp), pointer           :: buoy2(:)            ! buoyancy end stage
        real(sp), pointer           :: vzact1(:)           ! active vertical velocity begin stage
        real(sp), pointer           :: vzact2(:)           ! active vertical velocity end stage
        real(sp), pointer           :: ztop1(:)            ! depth top layer begin stage
        real(sp), pointer           :: ztop2(:)            ! depth top layer end stage
        real(sp), pointer           :: zbot1(:)            ! depth bot layer begin stage
        real(sp), pointer           :: zbot2(:)            ! depth bot layer end stage
        integer , pointer           :: hbtype(:)           ! horizontal behaviour type
        real(sp), pointer           :: vswim1(:)           ! swim velocity at begin of stage
        real(sp), pointer           :: vswim2(:)           ! swim velocity at end of stage

        ! local :

        real(sp), pointer           :: phase_diurn(:)      ! phase in diurnal behaviour
        integer(ip)                 :: ipart               ! particle index        
        real   (sp)                 :: fstage              ! fraction of current stage
        integer(ip)                 :: istage              ! integer stage development
        integer(ip)                 :: idelt               ! timestep in seconds
        real   (sp)                 :: day                 ! time in days
        real   (sp)                 :: a                   ! a coefficient in development (-)
        real   (sp)                 :: b                   ! b coefficient in development (-)
        real   (sp)                 :: vz                  ! vz
        real   (sp)                 :: vzact               ! vzact
        real   (sp)                 :: buoy                ! buoy
        integer                     :: k                   ! k
        real   (sp)                 :: ztop                ! ztop
        real   (sp)                 :: zbot                ! zbot
        integer                     :: m                   ! m
        integer                     :: n                   ! n
        
        real   (sp)                 :: zdepth              ! z relative to water surface
        real   (sp)                 :: zlevel              ! z relative to bottom
        logical, pointer            :: ebb_flow( : )       ! true if flow is ebb
        
        integer                     :: behaviour_type      ! actual behaviour type

        integer, parameter          :: behaviour_none     = 0 ! behaviour type none
        integer, parameter          :: behaviour_passive  = 1 ! behaviour type passive
		integer, parameter          :: behaviour_rooted   = 2 ! behaviour type rooted
       

        real                        :: vswim                  ! swimming velocity
        real                        :: local_angle            ! angle towards lowest salinity in grid
        
        real                        :: lb_sal                 ! lower boundary of temperature
        real                        :: ub_sal                 ! upper boundary of temperature

        real                        :: sal_n0
        real                        :: sal_n1
        real                        :: sal_n12
        real                        :: sal_n2
        real                        :: sal_n23
        real                        :: sal_n3
        real                        :: sal_n34
        real                        :: sal_n4
        real                        :: sal_n41

        real                        :: lb_temp                ! lower boundary of salinity
        real                        :: ub_temp                ! upper boundary of salinity       

        real                        :: temp_n0
        real                        :: temp_n1
        real                        :: temp_n12
        real                        :: temp_n2
        real                        :: temp_n23
        real                        :: temp_n3
        real                        :: temp_n34
        real                        :: temp_n4
        real                        :: temp_n41
        
        logical                     :: stick_to_bottom        ! stick to bottom when reached     
        
        
        ! MANGROVE BEHAVIOUR TYPES
        
        istage  = wpart(2,ipart)                                                         ! Get current stage of particle
        
        !Set layer and/or settling velocity according to stage and type of vertical behaviour

        behaviour_type = btype(istage)                                                   ! Assign the vertical behaviour type by stage
        select case ( behaviour_type )                                                   ! Select behaviour by numbering

           case ( behaviour_none )                                                       !Behaviour 0 

              !The particle shows no active behaviour in the vertical and in the horizontal

              !Horizontal behaviour                                                        
              v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
              d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0

              !Vertical behaviour
              wsettl(ipart) = 0.0                                                       ! Setteling velocity is set to 0


           case ( behaviour_passive)                                                    !Behaviour 1

             !Horizontal behaviour                                                        
             v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
             d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0
             
             !The particle shows no active behaviour in the vertical and in the horizontal, 
             ! but is subject to bouyancy.
            
             buoy = buoy1(istage) + fstage*(buoy2(istage)-buoy1(istage))               ! Set the bouyancy for the particle
             vz = buoy                                                                 ! Set the vertical direction for the particle
             
             wsettl(ipart) = vz
			 
		   case ( behaviour_rooted)                                                     !Behaviour 2

             !Horizontal behaviour                                                        
             v_swim(ipart) = 0.0                                                        ! Set the horizontal swimming velocity to 0
             d_swim(ipart) = 0.0                                                        ! Set the horizontal swimming direction to 0
             
             !The particle shows no active behaviour in the vertical and in the horizontal, 
             ! but is subject to bouyancy.
            
             buoy = buoy1(istage) + fstage*(buoy2(istage)-buoy1(istage))               ! Set the bouyancy for the particle
             vz = buoy                                                                 ! Set the vertical direction for the particle
             
             wsettl(ipart) = vz
             
           case default                                                                 !Behaviour Default

              write(lunrep,*) ' error, larval behaviour type not defined'               ! Give an error notice that vertical behaviour is not defined
              call stop_exit(1)                                                         ! Stop the calculation
    
               
        end select

      return                                                                                                         !Return from the subroutine
      end subroutine
end module



            