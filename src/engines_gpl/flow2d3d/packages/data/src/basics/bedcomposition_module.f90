module bedcomposition_module
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
! This module keeps track of the bed composition at one or more locations. The
! locations are indicated by indices nmlb to nmub. The bed composition consists
! of nfrac sediment fractions; their mass and volume fractions sum to 1. The
! bed may be schematized using one or more layers.
!
!!--module declarations---------------------------------------------------------

use precision
private

!
! public data types
!
public bedcomp_data

!
! public routines
!
public  bedcomposition_module_info
public  copybedcomp
public  updmorlyr
public  gettoplyr
public  detthcmud
public  getalluvthick
public  getporosity
public  getfrac
public  getmfrac
public  setmfrac
public  getvfrac
public  setvfrac
public  getsedthick
public  initmorlyr
public  setbedfracprop
public  allocmorlyr
public  clrmorlyr
public  bedcomp_use_bodsed
!
public bedcomp_getpointer_integer
public bedcomp_getpointer_logical
public bedcomp_getpointer_realfp
public bedcomp_getpointer_realprec
!
! interfaces
!
interface bedcomp_getpointer_logical
   module procedure bedcomp_getpointer_logical_scalar
end interface
interface bedcomp_getpointer_integer
   module procedure bedcomp_getpointer_integer_scalar
end interface
interface bedcomp_getpointer_realfp
   module procedure bedcomp_getpointer_fp_scalar
   module procedure bedcomp_getpointer_fp_1darray
   module procedure bedcomp_getpointer_fp_2darray
   module procedure bedcomp_getpointer_fp_3darray
end interface
interface bedcomp_getpointer_realprec
   module procedure bedcomp_getpointer_prec_2darray
end interface
interface getmfrac
   module procedure getmfrac_1point
   module procedure getmfrac_allpoints
end interface
interface setmfrac
   module procedure setmfrac_1point
end interface
interface getvfrac
   module procedure getvfrac_1point
   module procedure getvfrac_allpoints
end interface
interface setvfrac
   module procedure setvfrac_1point
end interface
interface getsedthick
   module procedure getsedthick_1point
   module procedure getsedthick_allpoints
end interface
!
! morphology layers numerical settings
!
type morlyrnumericstype
    real(fp) :: MinMassShortWarning  ! minimum erosion thickness for a shortage warning
    integer  :: MaxNumShortWarning   ! maximum number of shortage warnings remaining
end type morlyrnumericstype

   
type bedcomp_settings
    !
    ! doubles
    !
    real(fp) :: thlalyr   !  thickness of Lagrangian underlayer layers
    real(fp) :: theulyr   !  thickness of Eulerian underlayer layers
    !
    ! reals
    !
    !
    ! integers
    !
    integer :: iporosity  !  switch for porosity (simulate porosity if iporosity > 0)
                          !  0: porosity included in densities, set porosity to 0
                          !  1: ...
    integer :: iunderlyr  !  switch for underlayer concept
                          !  1: standard fully mixed concept
                          !  2: graded sediment concept
    integer :: keuler     !  index of first Eulerian (i.e. non-moving) layer
                          !  2   : standard Eulerian, only top layer moves with bed level
                          !  nlyr: fully Lagrangian (all layers move with bed level)
    integer :: nfrac      !  number of sediment fractions
    integer :: neulyr     !  number of Eulerian underlayers
    integer :: nlalyr     !  number of Lagrangian underlayers
    integer :: nlyr       !  number of layers (transport + exchange +
                          !  under layers)
    integer :: nmlb       !  start index of segments
    integer :: nmub       !  nm end index
    integer :: updbaselyr !  switch for computing composition of base layer
                          !  1: base layer is an independent layer
                          !  2: base layer composition is kept fixed
                          !  3: base layer composition is set equal to the
                          !     composition of layer above it
                          !  4: base layer composition and thickness constant
    !
    ! pointers
    !
    type (morlyrnumericstype) , pointer :: morlyrnum ! structure containing numerical settings
    real(fp) , dimension(:)   , pointer :: thexlyr   ! thickness of exchange layer
    real(fp) , dimension(:)   , pointer :: thtrlyr   ! thickness of transport layer
    real(fp) , dimension(:)   , pointer :: phi       ! D50 diameter expressed on phi scale
    real(fp) , dimension(:)   , pointer :: rhofrac   ! density of fraction (specific density or including pores)
    real(fp) , dimension(:)   , pointer :: sigphi    ! standard deviation expressed on phi scale
    ! 
    ! logicals
    !
    logical :: exchlyr    !  flag for use of exchange layer (underlayer
                          !  bookkeeping system)
    !
    ! characters
    !
end type bedcomp_settings
!
type bedcomp_state
    real(fp)   , dimension(:,:)  , pointer :: svfrac   ! 1 - porosity coefficient, units : -
    real(prec) , dimension(:,:)  , pointer :: bodsed   ! Array with total sediment, units : kg /m2
    real(fp)   , dimension(:)    , pointer :: dpsed    ! Total depth sediment layer, units : m
    real(fp)   , dimension(:,:,:), pointer :: msed     ! composition of morphological layers: mass of sediment fractions, units : kg /m2
    real(fp)   , dimension(:,:)  , pointer :: sedshort ! sediment shortage in transport layer, units : kg /m2
    real(fp)   , dimension(:,:)  , pointer :: thlyr    ! thickness of morphological layers, units : m
end type bedcomp_state
!
type bedcomp_work
    real(fp), dimension(:,:) , pointer :: msed2
    real(fp), dimension(:)   , pointer :: svfrac2
    real(fp), dimension(:)   , pointer :: thlyr2
end type bedcomp_work
!
type bedcomp_data
   private
   type (bedcomp_settings), pointer :: settings
   type (bedcomp_state)   , pointer :: state
end type bedcomp_data

contains
!
!
!
!==============================================================================
subroutine bedcomposition_module_info(messages)
    use message_module
    !
    type(message_stack), pointer :: messages
    !
    call addmessage(messages,'$Id$')
    call addmessage(messages,'$URL$')
end subroutine bedcomposition_module_info
!
!
!
!==============================================================================
function updmorlyr(this, dbodsd, dz, messages) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Update underlayer bookkeeping system for erosion/sedimentation
!
!!--declarations----------------------------------------------------------------
    use precision
    use message_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                                           :: this 
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac), intent(in)  :: dbodsd  !  change in sediment composition, units : kg/m2
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                     , intent(out) :: dz      !  change in bed level, units : m
    type(message_stack)                                                                          :: messages
    integer                                                                                      :: istat
    !
    ! Local variables
    !
    integer                                 :: l
    integer                                 :: nm
    real(fp)                                :: seddep0
    real(fp)                                :: seddep1
    real(fp)                                :: thdiff
    real(fp)                                :: fac
    real(fp)                                :: temp
    real(fp)                                :: thick
    real(fp)                                :: thtrlyrnew
    real(fp), dimension(this%settings%nfrac):: dmi
    type (bedcomp_work)                     :: work
    !
    character(message_len)                  :: message
    type (morlyrnumericstype)     , pointer :: morlyrnum
    real(prec) , dimension(:,:)   , pointer :: bodsed
    real(fp)   , dimension(:,:)   , pointer :: svfrac
    real(fp)   , dimension(:)     , pointer :: dpsed
    real(fp)   , dimension(:,:,:) , pointer :: msed
    real(fp)   , dimension(:)     , pointer :: rhofrac
    real(fp)   , dimension(:,:)   , pointer :: sedshort 
    real(fp)   , dimension(:,:)   , pointer :: thlyr
    real(fp)   , dimension(:)     , pointer :: thtrlyr   
!
!! executable statements -------------------------------------------------------
!
    morlyrnum   => this%settings%morlyrnum
    rhofrac     => this%settings%rhofrac
    thtrlyr     => this%settings%thtrlyr
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    sedshort    => this%state%sedshort
    thlyr       => this%state%thlyr  
    !
    istat = 0
    if (allocwork(this,work)) return
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          call getsedthick_1point(this, nm, seddep0)
          if (.not.this%settings%exchlyr) then
             !
             ! transport layer only
             !
             do l = 1, this%settings%nfrac
                temp  = msed(nm, 1, l) + dbodsd(nm, l)
                if (temp < 0.0_fp) then
                   if (temp < -morlyrnum%MinMassShortWarning .and. morlyrnum%MaxNumShortWarning>0) then
                      morlyrnum%MaxNumShortWarning = morlyrnum%MaxNumShortWarning - 1
                      write(message,'(a,i5,a,i3,a,e20.4,a,e20.4)') &
                         & 'Sediment erosion shortage at NM ', nm, ' Fraction: ', l, &
                         & ' Mass available   : ' ,msed(nm, 1, l), &
                         & ' Mass to be eroded: ', dbodsd(nm, l)
                      call addmessage(messages,message)
                      if (morlyrnum%MaxNumShortWarning == 0) then
                         message = 'Sediment erosion shortage messages suppressed'
                         call addmessage(messages,message)
                      endif
                   endif
                   sedshort(nm, l) = sedshort(nm, l) + temp
                   temp = 0.0_fp
                elseif ( sedshort(nm, l) < 0.0 ) then
                   temp = temp + sedshort(nm, l)
                   if ( temp < 0.0_fp ) then
                      sedshort(nm, l) = temp
                      temp = 0.0_fp
                   else
                      sedshort(nm, l) = 0.0_fp
                   endif
                endif
                msed(nm, 1, l) = temp
             enddo
             !
             ! get new requested transport layer thickness.
             !
             thtrlyrnew = thtrlyr(nm)
             !
             ! compute actual current thickness of top layer
             !
             call updateporosity(this, nm, 1)
             thick = 0.0_fp
             do l = 1, this%settings%nfrac
                 thick = thick + msed(nm, 1, l)/rhofrac(l)
             enddo
             thick = thick/svfrac(nm, 1)
             !
             thdiff = thick-thtrlyrnew
             !
             ! get sediment from or put sediment into underlayers
             ! to get transport layer of requested thickness
             !
             if ( thdiff > 0.0_fp ) then
                !   
                ! sedimentation to underlayers
                ! determine surplus of mass per fraction
                ! 
                fac = thdiff/thick
                do l = 1, this%settings%nfrac
                    dmi(l) = msed(nm, 1, l)*fac
                    msed(nm, 1, l) = msed(nm, 1, l) - dmi(l)
                enddo
                !
                ! store surplus of mass in underlayers
                !
                call lyrsedimentation(this , nm, thdiff, dmi, svfrac(nm, 1), work)
                !
             elseif ( thdiff < 0.0_fp ) then
                !
                ! erosion of underlayers
                ! total erosion thickness: thdiff
                ! associated mass returned in: dmi
                !
                thdiff = -thdiff
                !  
                call lyrerosion(this , nm, thdiff, dmi)
                !
                ! add to top layer
                ! 
                do l = 1, this%settings%nfrac 
                    msed(nm, 1, l)   = msed(nm, 1, l) + dmi(l)
                enddo
                !
                do l = 1, this%settings%nfrac
                    if (sedshort(nm, l) < 0.0_fp .and. msed(nm, 1, l) > 0.0_fp) then
                        sedshort(nm, l) = sedshort(nm, l) + msed(nm, 1, l)
                        if (sedshort(nm, l) > 0.0_fp) then
                            msed(nm, 1, l)  = sedshort(nm, l)
                            sedshort(nm, l) = 0.0_fp
                        else
                            msed(nm, 1, l) = 0.0_fp
                        endif
                    endif
                enddo
                !
                call updateporosity(this, nm, 1)
                thick = 0.0_fp
                do l = 1, this%settings%nfrac
                    thick = thick + msed(nm, 1, l)/rhofrac(l)
                enddo
                thick = thick/svfrac(nm, 1)
                !
                ! if there is not enough sediment in the bed then the actual
                ! thickness thick of the top layer may not reach the desired
                ! thickness thtrlyrnew, so we should here use thick as the
                ! thickness instead of thtrlyrnew
                !
                thtrlyrnew = thick
             endif
             thlyr(nm, 1) = thtrlyrnew  
          else
             !
             ! transport and exchange layer
             !
             message = 'Updating exchange layer not yet implemented'
             call adderror(messages,message)
             istat = -1
             exit
          endif
          call getsedthick_1point(this, nm, seddep1)
          dz(nm) = seddep1-seddep0
       enddo
    case default
       do nm = this%settings%nmlb,this%settings%nmub
          seddep0   = dpsed(nm)
          dpsed(nm) = 0.0_fp
          dz(nm)    = 0.0_fp
          do l = 1, this%settings%nfrac
             bodsed(nm, l) = bodsed(nm, l) + real(dbodsd(nm, l),prec)
             if (bodsed(nm, l) < 0.0_prec) then
                if (bodsed(nm, l) < real(-morlyrnum%MinMassShortWarning,prec) .and. morlyrnum%MaxNumShortWarning>0) then
                   morlyrnum%MaxNumShortWarning = morlyrnum%MaxNumShortWarning - 1
                   write(message,'(a,i0,a,i0,a,e20.4,a,e20.4)') &
                      & 'Sediment erosion shortage at NM ', nm, ' Fraction: ', l, &
                      & ' Mass available   : ' ,bodsed(nm, l), &
                      & ' Mass to be eroded: ', dbodsd(nm, l)
                   call addmessage(messages,message)
                   if (morlyrnum%MaxNumShortWarning == 0) then
                      message = 'Sediment erosion shortage messages suppressed'
                      call addmessage(messages,message)
                   endif
                endif
                bodsed(nm, l) = 0.0_prec
             endif
             dpsed(nm) = dpsed(nm) + real(bodsed(nm, l),fp)/rhofrac(l)
          enddo    
          dz(nm) = dpsed(nm) - seddep0
       enddo
    endselect
    if (deallocwork(this,work)) return
end function updmorlyr
!
!
!
!==============================================================================
function gettoplyr(this, dz_eros, dbodsd, messages  ) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Retrieve the sediment in the top layer (thickness dz_eros
!                in m) from the underlayer bookkeeping system and update the
!                administration.
!
!!--declarations----------------------------------------------------------------
    use precision
    use message_module
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                                           :: this
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac), intent(out) :: dbodsd  !  change in sediment composition, units : kg/m2
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                                   :: dz_eros !  change in sediment thickness, units : m
    type(message_stack)                                                                          :: messages
    integer                                                                                      :: istat
    !
    ! Local variables
    !
    integer                                 :: k
    integer                                 :: l
    integer                                 :: nm
    real(fp)                                :: dz
    real(fp)                                :: fac
    real(fp)                                :: thtrlyrnew
    real(fp)                                :: thick
    real(fp), dimension(this%settings%nfrac):: dmi 
    type (bedcomp_work)                     :: work
    !
    character(message_len)                  :: message
    real(prec) , dimension(:,:)   , pointer :: bodsed
    real(fp)   , dimension(:,:)   , pointer :: svfrac
    real(fp)   , dimension(:)     , pointer :: dpsed
    real(fp)   , dimension(:,:,:) , pointer :: msed
    real(fp)   , dimension(:,:)   , pointer :: sedshort 
    real(fp)   , dimension(:)     , pointer :: rhofrac
    real(fp)   , dimension(:,:)   , pointer :: thlyr
    real(fp)   , dimension(:)     , pointer :: thtrlyr
    !
    !! executable statements -------------------------------------------------------
    !
    thtrlyr     => this%settings%thtrlyr
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    sedshort    => this%state%sedshort
    thlyr       => this%state%thlyr
    !
    istat = 0
    if (allocwork(this,work)) return
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          if ( dz_eros(nm) < 0.0_fp ) then
             !
             ! erosion should not be negative
             !
             message = 'Negative dz_eros encountered'
             call adderror(messages,message)
             istat = -1
             return
          elseif (comparereal(dz_eros(nm),0.0_fp) == 0) then
             !
             ! skip it
             !
             do l = 1, this%settings%nfrac
                dbodsd (nm, l)    = 0.0_fp
             enddo
          elseif (.not.this%settings%exchlyr) then
             !
             ! transport layer only
             !  
             if (dz_eros(nm) <= thlyr(nm, 1)) then
                !
                ! erosion less than transport layer thickness
                !
                thick = thlyr(nm, 1) - dz_eros(nm)
                fac = dz_eros(nm)/thlyr(nm,1)
                do l = 1, this%settings%nfrac
                   dbodsd (nm, l) = msed(nm, 1, l)*fac
                   msed(nm, 1, l) = msed(nm, 1, l) - dbodsd(nm, l)
                enddo
             else
                !
                ! erosion more than transport layer thickness
                !
                do l = 1, this%settings%nfrac
                   dbodsd (nm, l) = msed(nm,1,l)
                   msed(nm, 1, l) = 0.0_fp
                enddo
                dz_eros(nm) = dz_eros(nm)-thlyr(nm, 1)
                thick  = 0.0_fp
                !
                ! get remaining dz_eros from underlayers
                ! get dmi from underlayers 
                !
                call lyrerosion(this , nm, dz_eros(nm), dmi)
                !
                do l = 1, this%settings%nfrac
                   dbodsd(nm, l) = dbodsd(nm, l) + dmi(l)
                enddo
             endif
             !
             ! get new transport layer thickness.
             !
             thtrlyrnew = thtrlyr(nm)
             !
             ! compute new thickness of top layer
             ! get sediment from or put sediment into underlayers
             ! to get transport layer of requested thickness
             !
             dz = thick-thtrlyrnew
             !
             if ( dz > 0.0_fp ) then
                !   
                ! sedimentation to underlayers
                ! 
                fac = dz/thick
                do l = 1, this%settings%nfrac
                    dmi(l) = msed(nm,1,l)*fac
                    msed(nm, 1, l) = msed(nm, 1, l) - dmi(l)
                enddo   
                !
                ! store surplus of mass in underlayers
                !
                call lyrsedimentation(this , nm, dz, dmi, svfrac(nm, 1), work)
                !
             elseif ( dz < 0.0_fp ) then
                !
                ! erosion of underlayers
                !
                dz = -dz
                call lyrerosion(this , nm, dz, dmi)
                !
                ! add to top layer
                !  
                do l = 1, this%settings%nfrac 
                    msed(nm, 1, l)   = msed(nm, 1, l) + dmi(l) 
                enddo
                !
                do l = 1, this%settings%nfrac
                    if (sedshort(nm, l) < 0.0_fp .and. msed(nm, 1, l) > 0.0_fp) then
                        sedshort(nm, l) = sedshort(nm, l) + msed(nm, 1, l)
                        if (sedshort(nm, l) > 0.0_fp) then
                            msed(nm, 1, l)  = sedshort(nm, l)
                            sedshort(nm, l) = 0.0_fp
                        else
                            msed(nm, 1, l) = 0.0_fp
                        endif
                    endif
                enddo
                !
                call updateporosity(this, nm, 1)
                thick = 0.0_fp
                do l = 1, this%settings%nfrac
                    thick = thick + msed(nm, 1, l)/rhofrac(l)
                enddo
                thick = thick/svfrac(nm, 1)
                !
                ! if there is not enough sediment in the bed then the actual
                ! thickness thick of the top layer may not reach the desired
                ! thickness thtrlyrnew, so we should here use thick as the
                ! thickness instead of thtrlyrnew
                !
                thtrlyrnew = thick
             endif
             thlyr(nm, 1) = thtrlyrnew
          else
             !
             ! transport and exchange layer
             !
             message = 'Updating exchange layer not yet implemented'
             call adderror(messages,message)
             istat = -1
             exit
          endif
       enddo
    case default
       do nm = this%settings%nmlb,this%settings%nmub
          if (dz_eros(nm)<0.0_fp) then
             !
             ! erosion should not be negative
             !
             message = 'Negative dz_eros encountered'
             call adderror(messages,message)
             istat = -1
             return
          elseif (comparereal(dz_eros(nm),0.0_fp) == 0 .or. dpsed(nm)<=0.0_fp) then
             !
             ! skip it
             !
             do l = 1, this%settings%nfrac
                dbodsd (nm, l)    = 0.0_fp
             enddo
          elseif (dz_eros(nm) < dpsed(nm)) then
             !
             ! some sediment remaining
             !
             fac = dz_eros(nm)/dpsed(nm)
             dpsed(nm) = 0.0_fp
             do l = 1, this%settings%nfrac
                dbodsd(nm, l) = real(bodsed(nm, l),fp)*fac
                bodsed(nm, l) = bodsed(nm, l) - real(dbodsd(nm, l),prec)
                dpsed (nm)    = dpsed(nm) + real(bodsed(nm, l),fp)/rhofrac(l)
             enddo
          else
             !
             ! no sediment remaining
             !
             dpsed(nm) = 0.0_fp
             do l = 1, this%settings%nfrac
                dbodsd(nm, l) = real(bodsed(nm, l),fp)
                bodsed(nm, l) = 0.0_hp
             enddo
          endif
       enddo
    endselect
    if (deallocwork(this,work)) return
end function gettoplyr
!
!
!
!==============================================================================
subroutine lyrerosion(this, nm, dzini, dmi)
!!--description-----------------------------------------------------------------
!
!    Function:
!     - lyrerosion implements the erosion of sediment from the layers below the
!       transport and exchange layers
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                    :: this    
    integer                                  , intent(in) :: nm
    real(fp)                                 , intent(in) :: dzini   !  thickness of eroded layer, units : m
    real(fp), dimension(this%settings%nfrac) , intent(out):: dmi     !  density of sediment fractions, units : kg/m3
    !
    ! Local variables
    !
    logical                                            :: remove
    integer                                            :: k
    integer                                            :: kero1  ! top-most layer that has been (partially) eroded
    integer                                            :: l
    real(fp)                                           :: dz
    real(fp)                                           :: dm
    real(fp)                                           :: fac
    real(fp)                                           :: thick
    real(fp)                                           :: thbaselyr
    real(fp), dimension(this%settings%nfrac)           :: mbaselyr  
    real(fp)                                 , pointer :: thlalyr
    integer                                  , pointer :: keuler 
    integer                                  , pointer :: nlyr
    integer                                  , pointer :: updbaselyr
    real(fp), dimension(:,:)                 , pointer :: svfrac
    real(fp), dimension(:,:,:)               , pointer :: msed
    real(fp), dimension(:,:)                 , pointer :: thlyr
!
!! executable statements -------------------------------------------------------
!
    keuler      => this%settings%keuler
    nlyr        => this%settings%nlyr
    thlalyr     => this%settings%thlalyr
    updbaselyr  => this%settings%updbaselyr
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    k   = 2
    if (this%settings%exchlyr) k = 3
    !
    thbaselyr = thlyr(nm,nlyr)
    mbaselyr  = msed(nm,nlyr,:)
    dmi = 0.0_fp
    dz  = dzini
    !
    ! initially remove sediment irrespective of layer type
    ! then fill the Lagrangian layers again up to their
    ! original thickness
    ! kero1 represents the Lagrangian layer that was eroded and needs
    ! to be replenished
    ! remove indicates that sediment should be eroded (stored in dmi)
    ! rather than shifted to another Lagrangian layer
    !
    kero1 = k-1
    remove = .true.
    do while (dz>0.0_fp .and. k<=nlyr)
        if ( thlyr(nm,k) < dz ) then
            !
            ! more sediment is needed than there is available in layer
            ! k, so all sediment should be removed from this layer
            !          
            do l = 1, this%settings%nfrac
                if (remove) then
                   dmi(l) = dmi(l) + msed(nm,k,l)
                else
                   msed(nm, kero1, l) = msed(nm,kero1,l) + msed(nm,k,l)
                endif
                msed(nm, k, l) = 0.0_fp
            enddo
            dz          = dz - thlyr(nm,k)
            if (.not.remove) then
               svfrac(nm,kero1) = svfrac(nm,kero1)*thlyr(nm,kero1) + svfrac(nm,k)*thlyr(nm,k)
               thlyr(nm,kero1)  = thlyr(nm,kero1) + thlyr(nm,k)
               svfrac(nm,kero1) = svfrac(nm,kero1)/thlyr(nm,kero1)
            endif
            thlyr(nm,k) = 0.0_fp
            k           = k+1
        else ! thlyr(k)>=dz
            !
            ! layer k contains more sediment than is need, so only part
            ! of the sediment has to be removed from the layer
            !
            fac = dz/thlyr(nm,k)
            do l = 1, this%settings%nfrac
                dm = msed(nm, k, l)*fac
                if (remove) then
                   dmi(l)             = dmi(l) + dm 
                else
                   msed(nm, kero1, l) = msed(nm,kero1,l) + dm
                endif
                msed(nm, k, l) = msed(nm, k, l) - dm
            enddo
            thlyr(nm,k)        = thlyr(nm,k)     - dz
            if (.not.remove) then
               svfrac(nm,kero1) = svfrac(nm,kero1)*thlyr(nm,kero1) + svfrac(nm,k)*dz
               thlyr(nm,kero1)  = thlyr(nm,kero1) + dz
               svfrac(nm,kero1) = svfrac(nm,kero1)/thlyr(nm,kero1)
            endif
            !
            ! erosion complete (dz=0) now continue to replenish the
            ! (partially) eroded Lagrangian layers as long as
            ! sediment is available in lower layers. Note that the
            ! Eulerian layers don't get replenished.
            !
            kero1 = kero1+1
            remove = .false.
            !
            ! do we have to fill again some of the Lagrangian layers?
            !
            if (kero1<keuler) then
                dz = max(thlalyr - thlyr(nm,kero1),0.0_fp)
                k = max(k,kero1+1)
            else
                dz = 0.0_fp
            endif
        endif
    enddo
    !
    ! update composition of base layer
    !
    select case (updbaselyr)
    case(1) ! compute separate composition for base layer
       !
       ! no change necessary
       !
    case(2) ! composition of base layer constant
       !
       ! compute new masses
       !
       fac = thlyr(nm,nlyr)/thbaselyr
       do l = 1, this%settings%nfrac
          msed(nm,nlyr,l) = mbaselyr(l)*fac
       enddo
    case(3) ! same as the (first non-empty) layer above it
       !
       ! find lowest non-empty layer
       !
       do k = nlyr-1,1,-1
          if ( thlyr(nm,k) > 0.0_fp ) exit
       enddo
       fac = thlyr(nm,nlyr)/thlyr(nm,k)
       do l = 1, this%settings%nfrac
          msed(nm,nlyr,l) = msed(nm,k,l)*fac
       enddo
    case(4) ! composition and thickness of base layer constant
       !
       ! reset thickness and masses
       !
       thlyr(nm,nlyr)  = thbaselyr
       msed(nm,nlyr,:) = mbaselyr
    case default
       !
       ! ERROR
       !
    endselect
end subroutine lyrerosion
!
!
!
!==============================================================================
subroutine lyrsedimentation(this, nm, dzini, dmi, svfracdep, work)
!!--description-----------------------------------------------------------------
!
!    Function:
!     - lyrsedimentation implements the deposition of sediment in the layers
!       below the transport and exchange layers
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                    :: this   
    integer                                  , intent(in) :: nm
    real(fp)                                 , intent(in) :: dzini
    real(fp)                                 , intent(in) :: svfracdep
    real(fp), dimension(this%settings%nfrac)              :: dmi
    type(bedcomp_work)                                    :: work
!
! Local variables
!
    integer                                     :: k
    integer                                     :: k2
    integer                                     :: kmin
    integer                                     :: kne
    integer                                     :: l
    real(fp)                                    :: depfrac
    real(fp)                                    :: dm
    real(fp)                                    :: dz
    real(fp)                                    :: dz2
    real(fp)                                    :: dzc
    real(fp)                                    :: dzk
    real(fp)                                    :: dzlmax
    real(fp)                                    :: fac
    real(fp)                                    :: newthlyr
    real(fp)                                    :: thick
    real(fp)                  , pointer         :: thlalyr
    integer                   , pointer         :: keuler
    integer                   , pointer         :: nlyr
    integer                   , pointer         :: updbaselyr
    real(fp), dimension(:,:)  , pointer         :: svfrac
    real(fp), dimension(:,:,:), pointer         :: msed
    real(fp), dimension(:,:)  , pointer         :: thlyr
    real(fp), dimension(this%settings%nfrac)    :: dmi2
!
!! executable statements -------------------------------------------------------
!
    thlalyr     => this%settings%thlalyr
    keuler      => this%settings%keuler
    nlyr        => this%settings%nlyr
    updbaselyr  => this%settings%updbaselyr
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    kmin = 2
    if (this%settings%exchlyr) kmin = 3
    dz = dzini
    !
    ! copy Lagrangian layer data to temporary array
    !
    do k = kmin,keuler-1
       do l = 1, this%settings%nfrac
          work%msed2(k,l) = msed(nm,k,l)
          msed(nm,k,l)    = 0.0_fp
       enddo
       work%svfrac2(k)  = svfrac(nm,k)
       work%thlyr2(k)   = thlyr(nm,k)
       thlyr(nm,k)      = 0.0_fp
    enddo
    !
    ! fill the Lagrangian layers from top to bottom
    !
    do k = kmin,keuler-1
       !
       ! while there is newly deposited sediment use that to fill the layers
       !
       if (dz>thlalyr) then
          !
          ! sufficient newly deposited sediment to fill whole layer
          !
          fac = thlalyr/dz
          do l = 1, this%settings%nfrac
             dm = dmi(l) * fac
             msed(nm, k, l) = dm
             dmi(l) = dmi(l) - dm
          enddo
          svfrac(nm, k) = svfracdep
          thlyr(nm, k)  = thlalyr
          dz            = dz - thlalyr
       elseif (dz>0.0_fp) then
          !
          ! last bit of newly deposited sediment to partially fill the layer
          !
          do l = 1, this%settings%nfrac
             msed(nm, k, l) = dmi(l)
             dmi(l) = 0.0_fp
          enddo
          svfrac(nm, k) = svfracdep
          thlyr(nm, k)  = dz
          dz            = 0.0_fp
       endif
       !
       ! as long as there is still space in this layer, fill it with sediment
       ! from the old Lagrangian layers
       !
       k2  = kmin
       dzc = thlalyr - thlyr(nm,k)
       do while (k2<keuler .and. dzc > 0.0_fp)
          !
          ! did this Lagrangian layer contain sediment?
          !
          if (work%thlyr2(k2)>0.0_fp) then
             if (dzc<work%thlyr2(k2)) then
                !
                ! sufficient sediment left in old layer k2 to fill the
                ! remainder of new layer k
                !
                fac = dzc/work%thlyr2(k2)
                do l = 1, this%settings%nfrac
                   dm = work%msed2(k2, l) * fac
                   msed(nm, k, l)    = msed(nm, k, l)    + dm
                   work%msed2(k2, l) = work%msed2(k2, l) - dm
                enddo
                svfrac(nm, k)   = svfrac(nm, k)*thlyr(nm, k) + work%svfrac2(k2)*dzc
                thlyr(nm, k)    = thlalyr
                svfrac(nm, k)   = svfrac(nm, k)/thlyr(nm, k)
                work%thlyr2(k2) = work%thlyr2(k2) - dzc
             else
                !
                ! all the sediment of old layer k2 fits into new layer k
                !
                do l = 1, this%settings%nfrac
                   msed(nm, k, l)    = msed(nm, k, l) + work%msed2(k2, l)
                   work%msed2(k2, l) = 0.0_fp
                enddo
                svfrac(nm, k)   = svfrac(nm, k)*thlyr(nm, k) + work%svfrac2(k2)*work%thlyr2(k2)
                thlyr(nm, k)    = thlyr(nm, k) + work%thlyr2(k2)
                svfrac(nm, k)   = svfrac(nm, k)/thlyr(nm, k)
                work%thlyr2(k2) = 0.0_fp
             endif
             dzc = thlalyr - thlyr(nm,k)
          endif
          k2 = k2+1
       enddo
    enddo
    !
    ! if there is any sediment left in the old Lagrangian layers then move it
    ! to the Eulerian layers below
    !
    do k2 = keuler-1,kmin,-1
       if (work%thlyr2(k2)>0.0_fp) then
          do l = 1, this%settings%nfrac
             dmi2(l) = work%msed2(k2,l)
          enddo
          call lyrsedimentation_eulerian(this, nm, work%thlyr2(k2), dmi2, work%svfrac2(k2))
       endif
    enddo
end subroutine lyrsedimentation
!
!
!
!==============================================================================
subroutine lyrsedimentation_eulerian(this, nm, dzini, dmi, svfracdep)
!!--description-----------------------------------------------------------------
!
!    Function:
!     - lyrsedimentation_eulerian implements the deposition of sediment in the 
!       Eulerian layers below the transport, exchange and other Lagrangian
!       layers
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                    :: this   
    integer                                  , intent(in) :: nm
    real(fp)                                 , intent(in) :: dzini
    real(fp)                                 , intent(in) :: svfracdep
    real(fp), dimension(this%settings%nfrac)              :: dmi
!
! Local variables
!
    integer                                     :: k
    integer                                     :: kmin
    integer                                     :: kne
    integer                                     :: l

    real(fp)                                    :: depfrac
    real(fp)                                    :: dm
    real(fp)                                    :: dz
    real(fp)                                    :: fac
    real(fp)                                    :: newthlyr
    real(fp)                                    :: thick
    real(fp)                  , pointer         :: theulyr
    integer                   , pointer         :: keuler
    integer                   , pointer         :: nlyr
    integer                   , pointer         :: updbaselyr
    real(fp), dimension(:,:)  , pointer         :: svfrac
    real(fp), dimension(:,:,:), pointer         :: msed
    real(fp), dimension(:,:)  , pointer         :: thlyr
!
!! executable statements -------------------------------------------------------
!
    theulyr     => this%settings%theulyr
    keuler      => this%settings%keuler
    nlyr        => this%settings%nlyr
    updbaselyr  => this%settings%updbaselyr
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    dz = dzini
    !
    ! find first (partially) filled underlayer
    !
    k = keuler
    do while (comparereal(thlyr(nm,k),0.0_fp)==0 .and. k<nlyr)
       k = k+1
    enddo
    !
    ! don't fill the last underlayer
    !
    if (k == nlyr) k = k-1
    !
    ! start filling upwards
    !
    do while ( k>=keuler .and. dz > 0.0_fp )
       if ( thlyr(nm,k) < theulyr ) then
          !
          ! sediment can be added to this layer
          !
          if ( dz > theulyr-thlyr(nm,k) ) then
             !
             ! not all sediment can be added to this layer
             !
             fac     = (theulyr-thlyr(nm,k))/dz
             dz      = dz*(1.0_fp-fac)
             do l = 1, this%settings%nfrac
                dm = dmi(l)*fac
                msed(nm, k, l) = msed(nm, k, l) + dm
                dmi(l)         = dmi(l)         - dm
             enddo
             svfrac(nm, k) = svfrac(nm, k)*thlyr(nm, k) + svfracdep*(theulyr-thlyr(nm,k))
             thlyr(nm,k)   = theulyr
             svfrac(nm, k) = svfrac(nm, k)/thlyr(nm, k)
          else
             !
             ! everything can be added to this layer
             !
             do l = 1, this%settings%nfrac            
                msed(nm, k, l) = msed(nm, k, l) + dmi(l)
                dmi(l) = 0.0_fp
             enddo
             svfrac(nm, k) = svfrac(nm, k)*thlyr(nm, k) + svfracdep*dz
             thlyr(nm, k)  = thlyr(nm, k) + dz
             svfrac(nm, k) = svfrac(nm, k)/thlyr(nm, k)
             dz            = 0.0_fp
          endif
       endif
       k = k-1
    enddo
    !
    ! the first Eulerian underlayer is completely filled or
    ! all sediment deposited
    !
    if ( dz > 0.0_fp ) then
       !
       ! still more sediment to be deposited
       !
       if (keuler == nlyr) then
          k = nlyr
          !
          ! no Eulerian underlayers, so put everything in
          ! the last (i.e. base) layer
          !
          select case (updbaselyr)
          case(1) ! compute separate composition for the base layer
             do l = 1, this%settings%nfrac           
                msed(nm, k, l) = msed(nm, k, l) + dmi(l)
             enddo
             svfrac(nm, k) = svfrac(nm, k)*thlyr(nm, k) + svfracdep*dz
             thlyr(nm, k)  = thlyr(nm, k) + dz
             svfrac(nm, k) = svfrac(nm, k)/thlyr(nm, k)
             dz            = 0.0_fp
          case(2) ! composition of base layer constant
             !
             ! composition of dz is lost, update thickness
             !
             fac = (thlyr(nm,k)+dz)/thlyr(nm,k)
             do l = 1, this%settings%nfrac
                msed(nm,k,l) = msed(nm,k,l)*fac
             enddo
             thlyr(nm, k) = thlyr(nm, k) + dz
          case(3) ! same as the (first non-empty) layer above it
             !
             ! composition of dz is lost, update thickness
             ! and set composition to that of layer nlyr-1
             !
             thlyr(nm, k) = thlyr(nm, k) + dz
             fac = thlyr(nm,k)/thlyr(nm,k-1)
             do l = 1, this%settings%nfrac
                msed(nm, k, l) = msed(nm, kmin-1, l)*fac
             enddo
          case default
             !
             ! ERROR
             !
          endselect
       else
          !
          ! thin underlayers filled shift administration and deposit
          ! remaining sediment
          !
          do while ( dz > 0.0_fp )
             !
             ! add last thin underlayer to the last (i.e. base) layer
             !
             newthlyr = thlyr(nm, nlyr)+thlyr(nm, nlyr-1)
             select case (updbaselyr)
             case(1) ! compute separate composition for the base layer
                if ( newthlyr > 0.0_fp ) then
                   do l = 1, this%settings%nfrac
                      msed(nm, nlyr, l) = msed(nm, nlyr , l) + msed(nm, nlyr-1, l) 
                   enddo
                   svfrac(nm, nlyr) = svfrac(nm, nlyr)*thlyr(nm, nlyr) + svfrac(nm, nlyr-1)*thlyr(nm, nlyr-1)
                   svfrac(nm, nlyr) = svfrac(nm, nlyr)/newthlyr
                endif
             case(2) ! composition of base layer constant
                !
                ! composition of layer nlyr-1 is lost; just the
                ! thickness of the base layer is updated
                !
                fac = newthlyr/thlyr(nm,nlyr)
                do l = 1, this%settings%nfrac
                    msed(nm,nlyr,l) = msed(nm,nlyr,l)*fac
                enddo
             case(3) ! same as the (first non-empty) layer above it
                !
                ! find lowest non-empty layer
                !
                do kne = nlyr-2,1,-1
                   if ( thlyr(nm,kne) > 0.0_fp ) exit
                enddo
                fac = newthlyr/thlyr(nm,kne)
                do l = 1, this%settings%nfrac
                   msed(nm,nlyr,l) = msed(nm,kne,l)*fac
                enddo
             case(4) ! composition and thickness of base layer constant
                !
                ! composition and sediment of layer nlyr-1 is lost
                ! make sure that the newthlyr of the base layer is equal
                ! to the old thickness
                !
                newthlyr = thlyr(nm,nlyr)
             case default
                !
                ! ERROR
                !
             end select
             thlyr(nm,nlyr) = newthlyr
             !
             ! shift layers down by one
             !
             do k = nlyr-1,keuler+1,-1
                do l = 1, this%settings%nfrac
                   msed(nm,k,l) = msed(nm,k-1,l)
                enddo
                thlyr(nm,k)  = thlyr(nm,k-1)
                svfrac(nm,k) = svfrac(nm,k-1)
             enddo
             !
             ! put all the sediment in one layer
             !
             k = keuler
             do l = 1, this%settings%nfrac 
                 msed(nm, k, l) = dmi(l)
             enddo
             thlyr(nm,k)  = dz
             svfrac(nm,k) = svfracdep
             dz           = 0.0_fp
          enddo
       endif
    endif
end subroutine lyrsedimentation_eulerian
!
!
!
!==============================================================================
subroutine detthcmud(this, sedtyp, thcmud)
!!--description-----------------------------------------------------------------
!
!    Function: - Update underlayer bookkeeping system for erosion/sedimentation
!
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                        , intent(in)  :: this
    real(fp), dimension(this%settings%nmlb:this%settings%nmub), intent(out) :: thcmud  !  Total thickness of the mud layers
    character(4), dimension(this%settings%nfrac)              , intent(in)  :: sedtyp
!
! Local variables
!
    integer                             :: l
    integer                             :: nm
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp)  , dimension(:)  , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    bodsed      => this%state%bodsed
    rhofrac     => this%settings%rhofrac
    !
    do nm = this%settings%nmlb,this%settings%nmub
       thcmud (nm) = 0.0
        do l = 1, this%settings%nfrac
           if (sedtyp(l) == 'mud ') thcmud(nm) = thcmud(nm) + real(bodsed(nm, l),fp)/rhofrac(l)
        enddo
    enddo
end subroutine detthcmud
!
!
!
!==============================================================================
subroutine getalluvthick(this, seddep)
!!--description-----------------------------------------------------------------
!
!    Function: - Update underlayer bookkeeping system for erosion/sedimentation
!
!!--declarations----------------------------------------------------------------
    use precision 
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                                             , intent(in)  :: this
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac), intent(out) :: seddep
!
! Local variables
!
    integer                              :: k
    integer                              :: l
    integer                              :: nm
    real(fp)                             :: thkl
    integer, pointer                     :: nlyr
    real(prec), dimension(:,:) , pointer :: bodsed
    real(fp), dimension(:,:) , pointer   :: thlyr
    real(fp), pointer                    :: thresh
    real(fp)  , dimension(:)  , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    nlyr                => this%settings%nlyr
    rhofrac             => this%settings%rhofrac
    bodsed              => this%state%bodsed
    thlyr               => this%state%thlyr
    !
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          thkl = 0.0_fp
          do k = 1, nlyr
             thkl = thkl + thlyr(nm, k)
          enddo
          do l = 1, this%settings%nfrac
             seddep(nm, l) = thkl
          enddo
       enddo
    case default
       do nm = this%settings%nmlb,this%settings%nmub
          do l = 1, this%settings%nfrac
             seddep(nm, l) = real(bodsed(nm, l),fp)/rhofrac(l)
          enddo
       enddo
    endselect
end subroutine getalluvthick
!
!
!
!==============================================================================
subroutine getfrac(this, frac, sedtyp, anymud, mudcnt, mudfrac)
!!--description-----------------------------------------------------------------
!
!    Function: Determines the (mass or volume) fractions
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                , intent(in)  :: this    
    logical                                                                         :: anymud
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac) :: frac
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                      :: mudfrac
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                      :: mudcnt
    character(4), dimension(this%settings%nfrac)                      , intent(in)  :: sedtyp
    !
    ! Local variables
    !
    integer  :: l
    integer  :: nm
    real(fp) :: nonmud
    !
    !! executable statements -------------------------------------------------------
    !
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(1)
       call getmfrac_allpoints(this ,frac      )
    case default
       call getvfrac_allpoints(this ,frac      )
    endselect
    !
    ! Calculate mud fraction
    !
    if (anymud) then
       !
       ! Add simulated mud fractions.
       !
       mudfrac = 0.0
       do l = 1, this%settings%nfrac
          if (sedtyp(l) == 'mud') then
             do nm = this%settings%nmlb,this%settings%nmub
                mudfrac(nm) = mudfrac(nm) + frac(nm,l)
             enddo
          endif
       enddo
    else
       !
       ! Take into account possible non-simulated
       ! mud fraction.
       !
       do nm = this%settings%nmlb,this%settings%nmub
          mudfrac(nm) = mudcnt(nm)
          nonmud      = 1.0 - mudcnt(nm)
          do l = 1, this%settings%nfrac
             frac(nm,l) = frac(nm,l) * nonmud
          enddo
       enddo
    endif
end subroutine getfrac
!
!
!
!==============================================================================
subroutine getmfrac_allpoints(this, frac)
!!--description-----------------------------------------------------------------
!
!    Function: Determines the mass fractions for all points
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                , intent(in)  :: this
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    integer                             :: nm
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: msed
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + msed(nm,1,l)
          enddo
          if (comparereal(sedtot,0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
            do l = 1, this%settings%nfrac
                frac(nm, l) = msed(nm, 1, l)/sedtot
            enddo
          endif
       enddo
    case default
       do nm = this%settings%nmlb,this%settings%nmub
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + real(bodsed(nm, l),fp)
          enddo
          if (comparereal(sedtot,0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             do l = 1, this%settings%nfrac
                frac(nm, l) = real(bodsed(nm, l),fp)/sedtot
             enddo
          endif
       enddo
    endselect
end subroutine getmfrac_allpoints
!
!
!
!==============================================================================
subroutine getmfrac_1point(this ,nm  ,frac      )
!!--description-----------------------------------------------------------------
!
!    Function: Determines the mass fractions for 1 point
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                      , intent(in)  :: this    
    integer                                 , intent(in)  :: nm
    real(fp), dimension(this%settings%nfrac), intent(out) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: msed
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       sedtot = 0.0_fp
       do l = 1, this%settings%nfrac
          sedtot = sedtot + msed(nm,1,l)
       enddo
       if (comparereal(sedtot,0.0_fp) == 0) then
          frac = 1.0_fp/this%settings%nfrac
       else
          do l = 1, this%settings%nfrac
             frac(l) = msed(nm,1,l)/sedtot
          enddo
       endif
    case default
       sedtot = 0.0_fp
       do l = 1, this%settings%nfrac
          sedtot = sedtot + real(bodsed(nm, l),fp)
       enddo
       if (comparereal(sedtot,0.0_fp) == 0) then
          frac = 1.0_fp/this%settings%nfrac
       else
          do l = 1, this%settings%nfrac
             frac(l) = real(bodsed(nm, l),fp)/sedtot
          enddo
       endif
    endselect
end subroutine getmfrac_1point
!
!
!
!==============================================================================
subroutine setmfrac_1point(this, nm, frac)
!!--description-----------------------------------------------------------------
!
!    Function: Sets the mass fractions for 1 point
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                    :: this
    integer                                 , intent(in)  :: nm
    real(fp), dimension(this%settings%nfrac), intent(in)  :: frac
    !
    ! Local variables
    !
    integer                             :: l
    real(fp)                            :: masstot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: msed
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       masstot = 0.0_fp
       do l = 1, this%settings%nfrac
          masstot = masstot + msed(nm,1,l)
       enddo
       do l = 1, this%settings%nfrac
          msed(nm, 1, l) = frac(l)*masstot
       enddo
    case default
       masstot = 1000.0_fp
       do l = 1, this%settings%nfrac
          bodsed(nm, l) = real(frac(l)*masstot,prec)
       enddo
    endselect
end subroutine setmfrac_1point
!
!
!
!==============================================================================
subroutine getvfrac_allpoints(this, frac)
!!--description-----------------------------------------------------------------
!
!    Function: Determines the volume fractions for all points
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                , intent(in)  :: this
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac) :: frac
    !
    ! Local variables
    !
    integer                               :: l
    integer                               :: nm
    real(fp)                              :: thick
    real(prec), dimension(:,:)  , pointer :: bodsed
    real(fp)  , dimension(:)    , pointer :: dpsed
    real(fp)  , dimension(:,:)  , pointer :: svfrac
    real(fp)  , dimension(:,:,:), pointer :: msed
    real(fp)  , dimension(:,:)  , pointer :: thlyr
    real(fp)  , dimension(:)    , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb, this%settings%nmub          
          if (comparereal(thlyr(nm,1),0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
            thick = svfrac(nm,1) * thlyr(nm,1)
            do l = 1, this%settings%nfrac
                frac(nm, l) = msed(nm, 1, l)/(rhofrac(l)*thick)
            enddo
          endif
       enddo
    case default
       do nm = this%settings%nmlb, this%settings%nmub
          if (comparereal(dpsed(nm),0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             do l = 1, this%settings%nfrac
                frac(nm, l) = real(bodsed(nm, l),fp)/(rhofrac(l)*dpsed(nm))
             enddo
          endif
       enddo
    endselect
end subroutine getvfrac_allpoints
!
!
!
!==============================================================================
subroutine getvfrac_1point(this ,nm ,frac      )
!!--description-----------------------------------------------------------------
!
!    Function: Determines the volume fractions for 1 point
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                      , intent(in)  :: this
    integer                                 , intent(in)  :: nm
    real(fp), dimension(this%settings%nfrac), intent(out) :: frac
    !
    ! Local variables
    !
    integer                               :: l
    real(fp)                              :: thick
    real(prec), dimension(:,:)  , pointer :: bodsed
    real(fp)  , dimension(:)    , pointer :: dpsed
    real(fp)  , dimension(:,:)  , pointer :: svfrac
    real(fp)  , dimension(:,:,:), pointer :: msed
    real(fp)  , dimension(:,:)  , pointer :: thlyr
    real(fp)  , dimension(:)    , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       if (comparereal(thlyr(nm,1),0.0_fp) == 0) then
          frac = 1.0_fp/this%settings%nfrac
       else
          thick = svfrac(nm,1) * thlyr(nm,1)
          do l = 1, this%settings%nfrac
             frac(l) = msed(nm, 1, l)/(rhofrac(l)*thick)
          enddo
       endif
    case default
       if (comparereal(dpsed(nm),0.0_fp) == 0) then
          frac = 1.0_fp/this%settings%nfrac
       else
          do l = 1, this%settings%nfrac
             frac(l) = real(bodsed(nm, l),fp)/(rhofrac(l)*dpsed(nm),fp)
          enddo
       endif
    endselect
end subroutine getvfrac_1point
!
!
!
!==============================================================================
subroutine setvfrac_1point(this, nm, vfrac)
!!--description-----------------------------------------------------------------
!
!    Function: Sets the volume fractions for 1 point
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                    :: this    
    integer                                 , intent(in)  :: nm
    real(fp), dimension(this%settings%nfrac), intent(in)  :: vfrac
    !
    ! Local variables
    !
    integer                                    :: l
    real(fp)                                   :: sum
    real(fp)                                   :: voltot
    real(fp)  , dimension(:,:)  , pointer      :: svfrac
    real(prec), dimension(:,:)  , pointer      :: bodsed
    real(fp)  , dimension(:,:,:), pointer      :: msed
    real(fp)  , dimension(:,:)  , pointer      :: thlyr
    real(fp)  , dimension(this%settings%nfrac) :: mfrac
    real(fp)  , dimension(:)    , pointer      :: rhofrac
    !
    !! executable statements -------------------------------------------------------
    !
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    sum = 0.0_fp
    do l = 1, this%settings%nfrac
       mfrac(l) = vfrac(l)*rhofrac(l)
       sum = sum + mfrac(l)
    enddo
    do l = 1, this%settings%nfrac
       mfrac(l) = mfrac(l)/sum
    enddo
    call setmfrac_1point(this, nm, mfrac)
end subroutine setvfrac_1point
!
!
!
!==============================================================================
subroutine getsedthick_allpoints(this, seddep)
!!--description-----------------------------------------------------------------
!
!    Function: Determines the volume fractions
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                          , intent(in)  :: this 
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)  , intent(out) :: seddep
    !
    ! Local variables
    !
    integer                           :: k
    integer                           :: nm
    real(fp), dimension(:), pointer   :: dpsed
    real(fp), dimension(:,:), pointer :: thlyr
    !
    !! executable statements -------------------------------------------------------
    !
    dpsed       => this%state%dpsed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
        seddep = 0.0_fp
        do nm = this%settings%nmlb, this%settings%nmub
            do k = 1, this%settings%nlyr
                seddep(nm) = seddep(nm) + thlyr(nm, k)
            enddo
        enddo
    case default
       seddep = dpsed
    endselect
end subroutine getsedthick_allpoints
!
!
!
!==============================================================================
subroutine getsedthick_1point(this, nm, seddep)
!!--description-----------------------------------------------------------------
!
!    Function: Determines the volume fractions
!
!!--declarations----------------------------------------------------------------
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                      , intent(in)  :: this    
    integer                                 , intent(in)  :: nm
    real(fp)                                , intent(out) :: seddep
    !
    ! Local variables
    !
    integer                           :: k
    real(fp), dimension(:)  , pointer :: dpsed
    real(fp), dimension(:,:), pointer :: thlyr
    !
    !! executable statements -------------------------------------------------------
    !
    dpsed       => this%state%dpsed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       seddep = 0.0_fp
       do k = 1, this%settings%nlyr
          seddep = seddep + thlyr(nm, k)
       enddo
    case default
       seddep = dpsed(nm)
    endselect
end subroutine getsedthick_1point
!
!
!
!==============================================================================
function initmorlyr(this) result (istat)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    real(fp), parameter :: rmissval = -999.0_fp
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(inout) :: this    
    integer                            :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer   :: settings
    type (bedcomp_state   ), pointer   :: state
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    if (istat == 0) allocate (settings, stat = istat)
    if (istat == 0) allocate (state   , stat = istat)
    if (istat /= 0) then
       !error
       return
    endif
    !
    allocate (settings%morlyrnum , stat = istat)
    if (istat == 0) then
       settings%morlyrnum%MinMassShortWarning = 0.0_fp
       settings%morlyrnum%MaxNumShortWarning = 100
    endif
    !
    settings%keuler     = 2
    settings%nfrac      = 0
    settings%nlyr       = 0
    settings%nmlb       = 0
    settings%nmub       = 0
    settings%iunderlyr  = 1
    settings%iporosity  = 0
    settings%exchlyr    = .false.
    settings%neulyr     = 0
    settings%nlalyr     = 0
    settings%theulyr    = rmissval
    settings%thlalyr    = rmissval
    settings%updbaselyr = 1
    !
    nullify(settings%thexlyr)
    nullify(settings%thtrlyr)
    !
    nullify(state%svfrac)
    nullify(state%bodsed)
    nullify(state%dpsed)
    nullify(state%msed)
    nullify(state%thlyr)
    nullify(state%sedshort)
    !
    this%settings => settings
    this%state    => state
end function initmorlyr
!
!
!
!==============================================================================
function allocmorlyr(this, nmlb, nmub, nfrac) result (istat)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    integer            , intent(in) :: nmlb
    integer            , intent(in) :: nmub
    integer            , intent(in) :: nfrac
    type (bedcomp_data)             :: this    
    integer                         :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer :: settings
    type (bedcomp_state), pointer    :: state
    !
    !! executable statements -------------------------------------------------------
    !
    settings => this%settings
    state => this%state
    !
    ! Number of layers: 1       transport layer
    !                   1       exchange layer (optional)
    !                   nlalyr  lagrangian underlayers
    !                   neulyr  eulerian underlayers
    !                   1       persistent base layer
    !
    settings%nlyr   = 2 + settings%nlalyr + settings%neulyr
    settings%keuler = 2 + settings%nlalyr
    if (settings%exchlyr) then
       settings%nlyr   = settings%nlyr + 1
       settings%keuler = settings%keuler + 1
    endif
    !
    settings%nmlb = nmlb
    settings%nmub = nmub
    settings%nfrac = nfrac
    !
    istat = 0
    if (istat == 0) allocate (state%bodsed(nmlb:nmub,nfrac), stat = istat)
    if (istat == 0) state%bodsed = 0.0_fp
    if (istat == 0) allocate (state%dpsed(nmlb:nmub), stat = istat)
    if (istat == 0) state%dpsed = 0.0_fp
    if (settings%iunderlyr==2) then
       if (istat == 0) allocate (settings%thtrlyr(nmlb:nmub), stat = istat)
       if (istat == 0) settings%thtrlyr = 0.0_fp
       if (settings%exchlyr) then
          if (istat == 0) allocate (settings%thexlyr(nmlb:nmub), stat = istat)
          if (istat == 0) settings%thexlyr = 0.0_fp
       endif
       if (istat == 0) allocate (settings%rhofrac(nfrac), stat = istat)
       if (istat == 0) settings%rhofrac = 0.0_fp
       if (istat == 0) allocate (settings%phi(nfrac), stat = istat)
       if (istat == 0) settings%phi = 0.0_fp
       if (istat == 0) allocate (settings%sigphi(nfrac), stat = istat)
       if (istat == 0) settings%sigphi = 0.0_fp
       !
       if (istat == 0) allocate (state%msed(nmlb:nmub,settings%nlyr,nfrac), stat = istat)
       if (istat == 0) state%msed = 0.0_fp
       if (istat == 0) allocate (state%thlyr(nmlb:nmub,settings%nlyr), stat = istat)
       if (istat == 0) state%thlyr = 0.0_fp
       if (istat == 0) allocate (state%sedshort(nmlb:nmub,nfrac), stat = istat)
       if (istat == 0) state%sedshort = 0.0_fp
       if (istat == 0) allocate (state%svfrac(nmlb:nmub,settings%nlyr), stat = istat)
       if (istat == 0) state%svfrac = 1.0_fp
    endif
end function allocmorlyr
!
!
!
!==============================================================================
function allocwork(this, work) result (istat)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(in)  :: this    
    type (bedcomp_work), intent(out) :: work
    integer                          :: istat
    !
    ! Local variables
    !
    integer, pointer :: nfrac
    integer, pointer :: nlyr
    !
    !! executable statements -------------------------------------------------------
    !
    nfrac => this%settings%nfrac
    nlyr  => this%settings%nlyr
    !
    istat = 0
    if (istat == 0) allocate (work%msed2(nlyr,nfrac), stat = istat)
    if (istat == 0) allocate (work%thlyr2(nlyr)     , stat = istat)
    if (istat == 0) allocate (work%svfrac2(nlyr)    , stat = istat)
end function allocwork
!
!
!
!==============================================================================
function deallocwork(this, work) result (istat)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(in)  :: this    
    type (bedcomp_work), intent(out) :: work
    integer                          :: istat
    !
    ! Local variables
    !
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    if (istat == 0) deallocate (work%msed2  , stat = istat)
    if (istat == 0) deallocate (work%thlyr2 , stat = istat)
    if (istat == 0) deallocate (work%svfrac2, stat = istat)
end function deallocwork
!
!
!
!==============================================================================
function clrmorlyr(this) result (istat)
!!--description-----------------------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data)             :: this    
    integer                         :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer :: settings
    type (bedcomp_state)   , pointer :: state
    !
    !! executable statements -------------------------------------------------------
    !
    if (associated(this%settings)) then
       settings => this%settings
       if (associated(settings%morlyrnum)) deallocate(settings%morlyrnum, STAT = istat)
       if (associated(settings%thexlyr))   deallocate(settings%thexlyr , STAT = istat)
       if (associated(settings%thtrlyr))   deallocate(settings%thtrlyr , STAT = istat)
       !
       deallocate(this%settings, STAT = istat)
       nullify(this%settings)
    endif
    !
    if (associated(this%state)) then
       state => this%state
       if (associated(state%svfrac))       deallocate(state%svfrac  , STAT = istat)
       if (associated(state%bodsed))       deallocate(state%bodsed  , STAT = istat)
       if (associated(state%dpsed))        deallocate(state%dpsed   , STAT = istat)
       if (associated(state%msed))         deallocate(state%msed    , STAT = istat)
       if (associated(state%thlyr))        deallocate(state%thlyr   , STAT = istat)
       if (associated(state%sedshort))     deallocate(state%sedshort, STAT = istat)
       !
       deallocate(this%state, STAT = istat)
       nullify(this%state)
    endif
end function clrmorlyr
!
!
!
!==============================================================================
subroutine setbedfracprop(this, sedd50, logsedsig, rhofrac)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a scalar logical
!
!!--declarations----------------------------------------------------------------
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                  :: this    
    real(fp), dimension(:), intent(in)  :: sedd50
    real(fp), dimension(:), intent(in)  :: logsedsig
    real(fp), dimension(:), intent(in)  :: rhofrac
    !
    ! Local variables
    !
    integer :: l
    !
    !! executable statements -------------------------------------------------------
    !
    do l = 1, this%settings%nfrac
       this%settings%phi(l)     = -log(sedd50(l))/log(2.0_fp)
       this%settings%sigphi(l)  = logsedsig(l)/log(2.0_fp)
       this%settings%rhofrac(l) = rhofrac(l) ! either rhosol or cdryb
    enddo
end subroutine
!
!
!
!==============================================================================
function bedcomp_getpointer_logical_scalar(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a scalar logical
!
!!--declarations----------------------------------------------------------------
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)    , intent(in)  :: this    
    character(*)          , intent(in)  :: variable
    logical, pointer                    :: val
    integer                             :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('exchlyr')
       val => this%settings%exchlyr
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function
!
!
!
!==============================================================================
function bedcomp_getpointer_integer_scalar(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a scalar integer
!
!!--declarations----------------------------------------------------------------
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)    , intent(in)  :: this    
    character(*)          , intent(in)  :: variable
    integer, pointer                    :: val
    integer                             :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('iunderlyr')
       val => this%settings%iunderlyr
    case ('iporosity')
       val => this%settings%iporosity
    case ('keuler')
       val => this%settings%keuler
    case ('nlyr')
       val => this%settings%nlyr
    case ('maxnumshortwarning')
       val => this%settings%morlyrnum%MaxNumShortWarning
    case ('neulyr')
       val => this%settings%neulyr
    case ('nlalyr')
       val => this%settings%nlalyr
    case ('updbaselyr')
       val => this%settings%updbaselyr
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function
!
!
!
!==============================================================================
function bedcomp_getpointer_fp_scalar(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a scalar real(fp)
!
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)    , intent(in)  :: this    
    character(*)          , intent(in)  :: variable
    real(fp), pointer                   :: val
    integer                             :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('theulyr')
       val => this%settings%theulyr
    case ('thlalyr')
       val => this%settings%thlalyr
    case ('minmassshortwarning')
       val => this%settings%morlyrnum%MinMassShortWarning
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function
!
!
!
!==============================================================================
function bedcomp_getpointer_fp_1darray(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a pointer to a 1D real(fp) array
!
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)             , intent(in)  :: this    
    character(*)                   , intent(in)  :: variable
    real(fp), dimension(:), pointer              :: val
    integer                                      :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('dpsed')
       val => this%state%dpsed
    case ('thexlyr')
       val => this%settings%thexlyr
    case ('thtrlyr')
       val => this%settings%thtrlyr
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1    
end function
!
!
!
!==============================================================================
function bedcomp_getpointer_fp_2darray(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a pointer to a 2D real(fp) array
!
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)               , intent(in)  :: this    
    character(*)                     , intent(in)  :: variable
    real(fp), dimension(:,:), pointer              :: val
    integer                                        :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('svfrac')
       val => this%state%svfrac
    case ('thlyr')
       val => this%state%thlyr
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function
!
!
!
!==============================================================================
function bedcomp_getpointer_fp_3darray(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a pointer to a 3D real(fp) array
!
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                 , intent(in)  :: this    
    character(*)                       , intent(in)  :: variable
    real(fp), dimension(:,:,:), pointer              :: val
    integer                                          :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('msed')
       val => this%state%msed
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function
!
!
!
!==============================================================================
function bedcomp_getpointer_prec_2darray(this, variable, val) result (istat)
!!--description-----------------------------------------------------------------
!
!    Function: - Get a pointer to a 2D real(prec) array
!
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                 , intent(in)  :: this    
    character(*)                       , intent(in)  :: variable
    real(prec), dimension(:,:), pointer              :: val
    integer                                          :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('bodsed')
       val => this%state%bodsed
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function
!
!
!
!==============================================================================
subroutine bedcomp_use_bodsed(this)
!!--description-----------------------------------------------------------------
!
!    Function: - Use the values of BODSED to compute other quantities
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                         :: this
    !
    ! Local variables
    !
    integer                                    :: ised
    integer                                    :: k
    integer                                    :: kstart
    integer                                    :: nm
    real(fp)                                   :: fac
    real(fp)  , dimension(this%settings%nfrac) :: mfrac
    real(fp)                                   :: poros
    real(fp)                                   :: sedthick
    real(fp)                                   :: sedthicklim
    real(fp)                                   :: svf
    real(fp)                                   :: thsed
    real(fp)                                   :: totsed
    real(prec), dimension(:,:)       , pointer :: bodsed
    real(fp)  , dimension(:)         , pointer :: dpsed
    real(fp)  , dimension(:,:,:)     , pointer :: msed
    real(fp)  , dimension(:,:)       , pointer :: svfrac
    real(fp)  , dimension(:,:)       , pointer :: thlyr
    real(fp)  , dimension(:)         , pointer :: thtrlyr
    real(fp)  , dimension(:)         , pointer :: thexlyr
    real(fp)  , dimension(:)         , pointer :: rhofrac
    !
    !! executable statements -------------------------------------------------------
    thtrlyr    => this%settings%thtrlyr
    thexlyr    => this%settings%thexlyr
    rhofrac    => this%settings%rhofrac
    bodsed     => this%state%bodsed
    dpsed      => this%state%dpsed
    msed       => this%state%msed
    svfrac     => this%state%svfrac
    thlyr      => this%state%thlyr
    !
    ! Fill initial values of DPSED
    !
    do nm = this%settings%nmlb, this%settings%nmub
       !
       ! Compute thickness correctly in case of
       ! multiple fractions with different rhofrac
       !
       dpsed(nm) = 0.0_fp
       do ised = 1, this%settings%nfrac
          dpsed(nm) = dpsed(nm) + real(bodsed(nm, ised),fp)/rhofrac(ised)
       enddo
    enddo
    select case(this%settings%iunderlyr)
    case(2)
       !
       ! No file specified for initial bed composition: extract data from
       ! the BODSED data read above.
       !
       msed = 0.0_fp
       thlyr = 0.0_fp
       do nm = this%settings%nmlb, this%settings%nmub
          !if (kcs(nm)<1 .or. kcs(nm)>2) cycle  !TODO: find a solution for this line
          !
          ! nm = (m-1)*nmax + n
          !
          totsed = 0.0_fp
          do ised = 1, this%settings%nfrac
             totsed = totsed + real(bodsed(nm, ised),fp)
          enddo
          do ised = 1, this%settings%nfrac
             mfrac(ised) = real(bodsed(nm, ised),fp)/totsed
          enddo
          !
          call getporosity(this, mfrac, poros)
          svf = 1.0_fp - poros
          !
          thsed = 0.0_fp
          do ised = 1, this%settings%nfrac
             thsed = thsed + real(bodsed(nm, ised),fp)/rhofrac(ised)
          enddo
          thsed         = thsed/svf
          sedthick      = thsed
          thsed         = max(thsed,1.0e-20_fp) ! avoid division by zero
          !
          ! active/transport layer
          !
          thlyr(nm, 1)  = min(thtrlyr(nm),sedthick)
          fac = thlyr(nm,1)/thsed
          do ised = 1, this%settings%nfrac
             msed(nm, 1, ised) = real(bodsed(nm, ised),fp)*fac
          enddo
          svfrac(nm, 1) = svf
          sedthick      = sedthick - thlyr(nm, 1)
          !
          ! exchange layer
          !
          kstart        = 1
          if (this%settings%exchlyr) then
             kstart       = 2
             thlyr(nm, 2) = min(thexlyr(nm),sedthick)
             sedthick     = sedthick - thlyr(nm, 2)
             fac = thlyr(nm,2)/thsed
             do ised = 1, this%settings%nfrac
                msed(nm, 2, ised) = real(bodsed(nm, ised),fp)*fac
             enddo
             svfrac(nm, 2) = svf
          endif
          !
          ! Lagrangian layers
          !
          do k = kstart+1, this%settings%keuler-1
             thlyr(nm, k) = min(this%settings%thlalyr,sedthick)
             sedthick     = sedthick - thlyr(nm, k)
             fac = thlyr(nm,k)/thsed
             do ised = 1, this%settings%nfrac
                msed(nm, k, ised) = real(bodsed(nm, ised),fp)*fac
             enddo
             svfrac(nm, k) = svf
          enddo
          !
          ! Eulerian layers
          !
          do k = this%settings%keuler,this%settings%nlyr-1
             thlyr(nm, k) = min(this%settings%theulyr,sedthick)
             sedthick     = sedthick - thlyr(nm, k)
             fac = thlyr(nm,k)/thsed
             do ised = 1, this%settings%nfrac
                msed(nm, k, ised) = real(bodsed(nm, ised),fp)*fac
             enddo
             svfrac(nm, k) = svf
          enddo
          !
          ! base layer
          !
          thlyr(nm, this%settings%nlyr) = sedthick
          fac = thlyr(nm,this%settings%nlyr)/thsed
          do ised = 1, this%settings%nfrac
             msed(nm, this%settings%nlyr, ised) = real(bodsed(nm, ised),fp)*fac
          enddo
          svfrac(nm, this%settings%nlyr) = svf
       enddo
    case default
       !
       ! nothing to do, using bodsed as uniformly mixed sediment
       !
    endselect
end subroutine
!
!
!
!==============================================================================
subroutine copybedcomp(this, nmfrom, nmto)
!!--description-----------------------------------------------------------------
!
!    Function: - Copy the bed composition from nmfrom to nmto
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                   :: this
    integer                , intent(in)  :: nmfrom
    integer                , intent(in)  :: nmto
    !
    ! Local variables
    !
    integer                              :: k
    integer                              :: l
    real(prec), dimension(:,:) , pointer :: bodsed
    real(fp) , dimension(:)    , pointer :: dpsed
    real(fp) , dimension(:,:,:), pointer :: msed
    real(fp) , dimension(:,:)  , pointer :: svfrac
    real(fp) , dimension(:,:)  , pointer :: thlyr
    !
    !! executable statements -------------------------------------------------------
    bodsed     => this%state%bodsed
    dpsed      => this%state%dpsed
    msed       => this%state%msed
    svfrac     => this%state%svfrac
    thlyr      => this%state%thlyr
    !
    select case(this%settings%iunderlyr)
    case(2)
       do k = 1, this%settings%nlyr
          do l = 1, this%settings%nfrac
             msed(nmto,k,l) = msed(nmfrom,k,l)
          enddo
          thlyr(nmto,k) = thlyr(nmfrom,k)
          svfrac(nmto,k) = svfrac(nmfrom,k)
       enddo
    case default
       do l = 1, this%settings%nfrac
          bodsed(nmto,l) = bodsed(nmfrom,l)
       enddo
       dpsed(nmto) = dpsed(nmfrom)
    end select
end subroutine
!
!
!
!==============================================================================
subroutine updateporosity(this, nm, k)
!!--description-----------------------------------------------------------------
!
!    Function: - Update the porosity for layer k in column nm
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                   :: this
    integer                , intent(in)  :: nm
    integer                , intent(in)  :: k
    !
    ! Local variables
    !
    integer                                   :: l
    real(fp)                                  :: poros
    real(fp)                                  :: totmass
    real(fp) , dimension(:,:,:), pointer      :: msed
    real(fp) , dimension(:,:)  , pointer      :: svfrac
    real(fp) , dimension(:,:)  , pointer      :: thlyr
    real(fp) , dimension(this%settings%nfrac) :: mfrac
    !
    !! executable statements -------------------------------------------------------
    msed       => this%state%msed
    svfrac     => this%state%svfrac
    thlyr      => this%state%thlyr
    !
    select case(this%settings%iunderlyr)
    case(2)
       totmass       = 0.0_fp
       do l = 1, this%settings%nfrac
          totmass    = totmass + msed(nm,k,l)
       enddo
       if (totmass>0.0_fp) then
          do l = 1, this%settings%nfrac
            mfrac(l)   = msed(nm,k,l)/totmass
          enddo
          !
          call getporosity(this, mfrac, poros)
       else
          poros = 0.0_fp
       endif
       svfrac(nm, k) = 1.0_fp - poros
    case default
       ! option not available for this bed composition model
    end select
end subroutine
!
!
!
!==============================================================================
subroutine getporosity(this, mfrac, poros)
!!--description-----------------------------------------------------------------
!
!    Function: - Compute the porosity
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                       , intent(in)  :: this
    real(fp) , dimension(this%settings%nfrac), intent(in)  :: mfrac
    real(fp)                                 , intent(out) :: poros
    !
    ! Local variables
    !
    integer                              :: l
    real(fp)                             :: a
    real(fp)                             :: b
    real(fp)                             :: phim
    real(fp)                             :: sigmix
    real(fp)                             :: x
    real(fp) , dimension(:)    , pointer :: phi
    real(fp) , dimension(:)    , pointer :: sigphi
    !
    !! executable statements -------------------------------------------------------
    phi        => this%settings%phi
    sigphi     => this%settings%sigphi
    !
    phim = 0.0_fp
    do l = 1, this%settings%nfrac
       phim   = phim + phi(l)*mfrac(l)
    enddo
    sigmix = 0.0_fp
    do l = 1, this%settings%nfrac
       sigmix = sigmix + mfrac(l)*((phi(l)-phim)**2 + sigphi(l)**2)
    enddo
    sigmix = sqrt(sigmix)
    !
    select case (this%settings%iporosity)
    case (1)
       !
       ! R. Frings (May 2009)
       !
       a = -0.06_fp
       b = 0.36_fp
       poros = max(0.0_fp,a*sigmix + b)
    case (2)
       !
       ! G.J. Weltje based on data by Beard & Weyl (AAPG Bull., 1973)
       !
       x             = 3.7632_fp * sigmix**(-0.7552_fp)
       poros         = 0.45_fp*x/(1+x)
    case default
       poros         = 0.0_fp
    end select
end subroutine

end module bedcomposition_module
