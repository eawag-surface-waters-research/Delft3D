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
public bedcomposition_module_info
public updmorlyr
public gettoplyr
public lyrerosion
public lyrsedimentation
public detthcmud
public getalluvthick
public getfrac
public getmfrac
public setmfrac
public getvfrac
public setvfrac
public getsedthick
public initmorlyr
public allocmorlyr
public clrmorlyr
public bedcomp_use_bodsed
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

!
! morphology layers numerical settings
!
type morlyrnumericstype
    real(fp) :: MinThickShortWarning ! minimum erosion thickness for a shortage warning
    integer  :: MaxNumShortWarning   ! maximum number of shortage warnings remaining
end type morlyrnumericstype

   
type bedcomp_settings
    !
    ! doubles
    !
    real(fp) :: thunlyr   !  thickness of underlayer layers
    !
    ! reals
    !
    !
    ! integers
    !
    integer :: iunderlyr  !  switch for underlayer concept
                          !  1: standard fully mixed concept
                          !  2: graded sediment concept
    integer :: nfrac      !  number of sediment fractions
    integer :: mxnulyr    !  maximum number of underlayers
    integer :: nlyr       !  number of layers (transport + exchange +
                          !  under layers)
    integer :: nmlb       !  start index of segments
    integer :: nmub       !  nm end index
    integer :: updbaselyr !  switch for computing composition of base layer
                          !  1: base layer is an independent layer
                          !  2: base layer composition is kept fixed
                          !  3: base layer composition is set equal to the
                          !     composition of layer above it
    !
    ! pointers
    !
    type (morlyrnumericstype) , pointer :: morlyrnum ! structure containing numerical settings
    real(fp) , dimension(:) , pointer :: thexlyr  ! thickness of exchange layer
    real(fp) , dimension(:) , pointer :: thtrlyr  ! thickness of transport layer
    ! 
    ! logicals
    !
    logical :: exchlyr    !  flag for use of exchange layer (underlayer
                          !  bookkeeping system)
    !
    ! characters
    !
end type bedcomp_settings

type bedcomp_work
    real(fp) , dimension(:) , pointer :: dzi     !  temporary array for passing sediment through
                                                            !  underlayers
end type bedcomp_work

type bedcomp_state
    real(prec) , dimension(:,:) , pointer :: bodsed   ! Array with total sediment
                                                             ! units : kg /m2
    real(fp) , dimension(:) , pointer :: dpsed    ! Total depth sediment layer
    real(fp) , dimension(:,:,:), pointer :: lyrfrac  ! composition of morphological layers:
                                                             ! volume fractions
    real(fp) , dimension(:,:) , pointer :: thlyr    ! thickness of morphological layers
    real(fp) , dimension(:,:) , pointer :: sedshort ! sediment shortage in transport layer
end type bedcomp_state

type bedcomp_data
   private
   type (bedcomp_settings), pointer :: settings
   type (bedcomp_work), pointer :: work
   type (bedcomp_state), pointer :: state
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
    call addmessage(messages,'$Id: bedcomposition_module.f90 14207 2010-12-21 17:25:13Z mourits $')
    call addmessage(messages,'$URL: https://repos.deltares.nl/repos/ds/branches/feature/delft3d/modules/d3d_flow/20101104_14542_open_source_testbranch/open_trunk/src/engines_gpl/flow2d3d/packages/data/src/basics/bedcomposition_module.f90 $')
end subroutine bedcomposition_module_info
!
!
!
!==============================================================================
function updmorlyr(this, dbodsd, cdryb, messages) result (istat)
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
    type(bedcomp_data), intent(in)                                                              :: this    
    real(fp), dimension(this%settings%nfrac), intent(in)                                        :: cdryb   !  Description and declaration in rjdim.f90
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac), intent(in) :: dbodsd  !  Description and declaration in rjdim.f90
    type(message_stack)                                                                         :: messages
    integer                                                                                     :: istat
    !
    ! Local variables
    !
    integer  :: l
    integer  :: nm
    real(fp) :: localthlyr
    real(fp) :: thtrlyrnew
    real(fp) :: temp
    !
    character(message_len)                :: message
    type (morlyrnumericstype) , pointer   :: morlyrnum
    real(prec) , dimension(:,:) , pointer :: bodsed
    real(fp) , dimension(:) , pointer     :: dpsed
    real(fp) , dimension(:) , pointer     :: dzi
    real(fp) , dimension(:,:,:) , pointer :: lyrfrac
    real(fp) , dimension(:,:) , pointer   :: thlyr
    real(fp) , dimension(:) , pointer     :: thtrlyr
    real(fp) , dimension(:,:) , pointer   :: sedshort
!
!! executable statements -------------------------------------------------------
!
    morlyrnum   => this%settings%morlyrnum
    thtrlyr     => this%settings%thtrlyr
    dzi         => this%work%dzi
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    lyrfrac     => this%state%lyrfrac
    thlyr       => this%state%thlyr
    sedshort    => this%state%sedshort
    !
    istat = 0
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          if (.not.this%settings%exchlyr) then
             !
             ! transport layer only: lyrfrac will temporarily contain thicknesses!
             !
             localthlyr = 0.0_fp
             do l = 1, this%settings%nfrac
                temp = lyrfrac(nm, 1, l)*thlyr(nm, 1) + dbodsd(nm, l)/cdryb(l)
                if (temp < 0.0_fp) then
                   if (temp < -morlyrnum%MinThickShortWarning .and. morlyrnum%MaxNumShortWarning>0) then
                      morlyrnum%MaxNumShortWarning = morlyrnum%MaxNumShortWarning - 1
                      write(message,'(a,i5,a,i3,a,e20.4,a,e20.4)') &
                         & 'Sediment erosion shortage at NM ', nm, ' Fraction: ', l, &
                         & ' Available: ' ,lyrfrac(nm, 1, l)*thlyr(nm, 1), &
                         & ' Erosion: ', dbodsd(nm, l)/cdryb(l)
                      call addmessage(messages,message)
                      if (morlyrnum%MaxNumShortWarning == 0) then
                         message = 'Sediment erosion shortage messages suppressed'
                         call addmessage(messages,message)
                      endif
                   endif
                   sedshort(nm, l) = sedshort(nm, l) + temp
                   temp = 0.0_fp
                elseif (sedshort(nm, l)<0.0) then
                   temp = temp + sedshort(nm, l)
                   if (temp < 0.0_fp) then
                      sedshort(nm, l) = temp
                      temp = 0.0_fp
                   else
                      sedshort(nm, l) = 0.0_fp
                   endif
                endif
                lyrfrac(nm, 1, l) = temp
                localthlyr        = localthlyr + lyrfrac(nm, 1, l)
             enddo
             !
             ! get new transport layer thickness.
             ! preliminary solution, keep it fixed
             !
             thtrlyrnew = thtrlyr(nm)
             !
             ! convert lyrfrac back from thicknesses to fractions
             ! get sediment from or put sediment into underlayers
             ! to get transport layer of requested thickness
             !
             if (localthlyr>thtrlyrnew) then
                !
                ! sedimentation to underlayers
                !
                do l = 1, this%settings%nfrac
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)/localthlyr
                   dzi(l)            = lyrfrac(nm, 1, l) * (localthlyr-thtrlyrnew)
                enddo
                !
                ! store dzi in underlayers
                !
                call lyrsedimentation(this , nm)
                !
             else
                !
                ! erosion of underlayers
                ! get dzi from underlayers (sum should be thtrlyrnew-localthlyr)
                !
                call lyrerosion(this , nm, thtrlyrnew-localthlyr)
                !
                do l = 1, this%settings%nfrac
                   if (sedshort(nm, l) < 0.0_fp .and. dzi(l) > 0.0_fp) then
                      sedshort(nm, l) = sedshort(nm, l) + dzi(l)
                      if (sedshort(nm, l) > 0.0_fp) then
                         dzi(l) = sedshort(nm, l)
                         sedshort(nm, l) = 0.0_fp
                      else
                         dzi(l) = 0.0_fp
                      endif
                   endif
                   localthlyr        = localthlyr + dzi(l)
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)+dzi(l)
                enddo
                !
                ! if there is not enough sediment in the bed localthlyr may not
                ! match thtrlyrnew, so we should use localthlyr instead of the
                ! desired value of thtrlyrnew
                !
                thtrlyrnew = localthlyr
                localthlyr = max(localthlyr,1.0e-20_fp)
                do l = 1, this%settings%nfrac
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)/localthlyr
                enddo
             endif
             thlyr(nm, 1) = thtrlyrnew
          else
             !
             ! transport and exchange layer
             !
             message = 'Updating exchange layer not yet implemented'
             call adderror(messages,message)
             istat = -1
             return
          endif
       enddo
    case default
       do nm = this%settings%nmlb,this%settings%nmub
          dpsed(nm) = 0.0
          do l = 1, this%settings%nfrac
             bodsed(nm, l) = bodsed(nm, l) + real(dbodsd(nm, l),prec)
             if (bodsed(nm, l) < 0.0_prec) then
                if (bodsed(nm, l) < real(-morlyrnum%MinThickShortWarning,prec) .and. morlyrnum%MaxNumShortWarning>0) then
                   morlyrnum%MaxNumShortWarning = morlyrnum%MaxNumShortWarning - 1
                   write(message,'(a,i0,a,i0,a,e20.4,a,e20.4)') &
                      & 'Sediment erosion shortage at NM ', nm, ' Fraction: ', l, &
                      & ' Available: ' ,bodsed(nm, l), &
                      & ' Erosion: ', dbodsd(nm, l)
                   call addmessage(messages,message)
                   if (morlyrnum%MaxNumShortWarning == 0) then
                      message = 'Sediment erosion shortage messages suppressed'
                      call addmessage(messages,message)
                   endif
                endif
                bodsed(nm, l) = 0.0_prec
             endif
             dpsed(nm) = dpsed(nm) + real(bodsed(nm, l),fp)/cdryb(l)
          enddo
       enddo
    endselect
end function updmorlyr
!
!
!
!==============================================================================
function gettoplyr(this, dz_eros, dbodsd, cdryb, messages  ) result (istat)
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
    type(bedcomp_data), intent(in)                                                               :: this
    real(fp), dimension(this%settings%nfrac), intent(in)                                         :: cdryb
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac), intent(out) :: dbodsd
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                                   :: dz_eros
    type(message_stack)                                                                          :: messages
    integer                                                                                      :: istat
    !
    ! Local variables
    !
    integer                               :: l
    integer                               :: nm
    real(fp)                              :: localthlyr
    real(fp)                              :: thtrlyrnew
    real(fp)                              :: thck_ratio
    character(message_len)                :: message
    real(prec) , dimension(:,:) , pointer :: bodsed
    real(fp) , dimension(:) , pointer     :: dpsed
    real(fp) , dimension(:) , pointer     :: dzi
    real(fp) , dimension(:,:,:) , pointer :: lyrfrac
    real(fp) , dimension(:,:) , pointer   :: thlyr
    real(fp) , dimension(:) , pointer     :: thtrlyr
    !
    !! executable statements -------------------------------------------------------
    !
    thtrlyr     => this%settings%thtrlyr
    dzi         => this%work%dzi
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    lyrfrac     => this%state%lyrfrac
    thlyr       => this%state%thlyr
    !
    istat = 0
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          if (dz_eros(nm)<0.0_fp) then
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
             ! transport layer only: lyrfrac will temporarily contain thicknesses!
             !
             if (dz_eros(nm)<=thlyr(nm, 1)) then
                !
                ! erosion less than transport layer thickness
                !
                localthlyr = thlyr(nm, 1) - dz_eros(nm)
                do l = 1, this%settings%nfrac
                   dbodsd (nm, l)    = lyrfrac(nm, 1, l)*dz_eros(nm)*cdryb(l)
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)*localthlyr
                enddo
             else
                !
                ! erosion more than transport layer thickness
                !
                do l = 1, this%settings%nfrac
                   dbodsd (nm, l)    = lyrfrac(nm, 1, l)*thlyr(nm, 1)*cdryb(l)
                   lyrfrac(nm, 1, l) = 0.0_fp
                enddo
                dz_eros(nm) = dz_eros(nm)-thlyr(nm, 1)
                localthlyr  = 0.0_fp
                !
                ! get remaining dz_eros from underlayers
                ! get dzi from underlayers (sum should be thtrlyrnew-localthlyr)
                !
                call lyrerosion(this , nm, dz_eros(nm))
                !
                do l = 1, this%settings%nfrac
                   dbodsd(nm, l) = dbodsd(nm, l) + dzi(l)*cdryb(l)
                enddo
             endif
             !
             ! get new transport layer thickness.
             ! preliminary solution, keep it fixed
             !
             thtrlyrnew = thtrlyr(nm)
             !
             ! convert lyrfrac back from thicknesses to fractions
             ! get sediment from or put sediment into underlayers
             ! to get transport layer of requested thickness
             !
             if (localthlyr>thtrlyrnew) then
                !
                ! sedimentation to underlayers
                !
                do l = 1, this%settings%nfrac
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)/localthlyr
                   dzi(l)            = lyrfrac(nm, 1, l) * (localthlyr-thtrlyrnew)
                enddo
                !
                ! store dzi in underlayers
                !
                call lyrsedimentation(this , nm)
                !
             else
                !
                ! erosion of underlayers
                ! get dzi from underlayers (sum should be thtrlyrnew-localthlyr)
                !
                call lyrerosion(this , nm, thtrlyrnew-localthlyr)
                !
                do l = 1, this%settings%nfrac
                   localthlyr        = localthlyr + dzi(l)
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)+dzi(l)
                enddo
                !
                ! if there is not enough sediment in the bed localthlyr may not
                ! match thtrlyrnew, so we should use localthlyr instead of the
                ! desired value of thtrlyrnew
                !
                thtrlyrnew = localthlyr
                localthlyr = max(localthlyr,1.0e-20_fp)
                do l = 1, this%settings%nfrac
                   lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)/localthlyr
                enddo
             endif
             thlyr(nm, 1) = thtrlyrnew
          else
             !
             ! transport and exchange layer
             !
             message = 'Updating exchange layer not yet implemented'
             call adderror(messages,message)
             istat = -1
             return
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
             thck_ratio = dz_eros(nm)/dpsed(nm)
             dpsed(nm) = 0.0_fp
             do l = 1, this%settings%nfrac
                dbodsd(nm, l) = real(bodsed(nm, l),fp)*thck_ratio
                bodsed(nm, l) = bodsed(nm, l) - real(dbodsd(nm, l),prec)
                dpsed (nm)    = dpsed(nm) + real(bodsed(nm, l),fp)/cdryb(l)
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
end function gettoplyr
!
!
!
!==============================================================================
subroutine lyrerosion(this, nm, dzini)
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
    type(bedcomp_data), intent(in) :: this    
    integer , intent(in)           :: nm
    real(fp) , intent(in)          :: dzini
    !
    ! Local variables
    !
    integer                             :: k
    integer                             :: l
    real(fp)                            :: dz
    real(fp)                            :: thbaselyr
    integer, pointer                    :: nlyr
    integer, pointer                    :: updbaselyr
    real(fp), dimension(:), pointer     :: dzi
    real(fp), dimension(:,:,:), pointer :: lyrfrac
    real(fp), dimension(:,:), pointer   :: thlyr
!
!! executable statements -------------------------------------------------------
!
    nlyr        => this%settings%nlyr
    updbaselyr  => this%settings%updbaselyr
    dzi         => this%work%dzi
    lyrfrac     => this%state%lyrfrac
    thlyr       => this%state%thlyr
    !
    k   = 2
    if (this%settings%exchlyr) k = 3
    !
    thbaselyr = thlyr(nm,nlyr)
    !
    dzi = 0.0
    dz  = dzini
    do while (dz>0.0 .and. k<=nlyr)
       if (thlyr(nm,k) < dz) then
          !
          ! all sediment should be removed from the layer
          !
          do l = 1, this%settings%nfrac
             dzi(l) = dzi(l) + thlyr(nm,k)*lyrfrac(nm,k,l)
          enddo
          dz          = dz - thlyr(nm,k)
          thlyr(nm,k) = 0.0
          k           = k+1
       else ! thlyr(k)>=dz
          !
          ! only part of the sediment has to be removed from
          ! the layer
          !
          do l = 1, this%settings%nfrac
             dzi(l) = dzi(l) + dz*lyrfrac(nm,k,l)
          enddo
          thlyr(nm,k) = thlyr(nm,k) - dz
          dz          = 0.0
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
       ! no change necessary
       !
    case(3) ! same as the (first non-empty) layer above it
       !
       ! find lowest non-empty layer
       !
       do k = nlyr-1,1,-1
          if (thlyr(nm,k)>0.0) exit
       enddo
       do l = 1, this%settings%nfrac
          lyrfrac(nm,nlyr,l) = lyrfrac(nm,k,l)
       enddo
    case(4) ! composition and thickness of base layer constant
       !
       ! reset thickness
       !
       thlyr(nm,nlyr) = thbaselyr
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
subroutine lyrsedimentation(this, nm)
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
    type(bedcomp_data), intent(in) :: this    
    integer, intent(in)            :: nm
!
! Local variables
!
    integer                             :: k
    integer                             :: kmin
    integer                             :: kne
    integer                             :: l
    real(fp)                            :: dz
    real(fp)                            :: depfrac
    real(fp)                            :: newthlyr
    real(fp), pointer                   :: thunlyr
    integer, pointer                    :: nlyr
    integer, pointer                    :: updbaselyr
    real(fp), dimension(:), pointer     :: dzi
    real(fp), dimension(:,:,:), pointer :: lyrfrac
    real(fp), dimension(:,:), pointer   :: thlyr
!
!! executable statements -------------------------------------------------------
!
    thunlyr     => this%settings%thunlyr
    nlyr        => this%settings%nlyr
    updbaselyr  => this%settings%updbaselyr
    dzi         => this%work%dzi
    lyrfrac     => this%state%lyrfrac
    thlyr       => this%state%thlyr
    !
    dz = 0.0
    do l = 1, this%settings%nfrac
       dz = dz + dzi(l)
    enddo
    kmin = 2
    if (this%settings%exchlyr) kmin = 3
    !
    ! find first (partially) filled underlayer
    !
    k = kmin
    do while (thlyr(nm,k) == 0.0 .and. k<nlyr)
       k = k+1
    enddo
    !
    ! don't fill the last underlayer
    !
    if (k == nlyr) k = k-1
    !
    ! start filling upwards
    !
    do while (k>=kmin .and. dz>0.0)
       if (thlyr(nm,k) < thunlyr) then
          !
          ! sediment can be added to this layer
          !
          if (dz > thunlyr-thlyr(nm,k) .and. thlyr(nm,k) > 0) then
             !
             ! not all sediment can be added to this layer
             !
             depfrac = (thunlyr-thlyr(nm,k))/dz
             dz      = dz*(1.0-depfrac)
             do l = 1,this%settings%nfrac
                lyrfrac(nm, k, l) = (lyrfrac(nm, k, l)*thlyr(nm, k) + dzi(l)*depfrac)/thunlyr
                dzi(l)            = dzi(l)*(1.0-depfrac)
             enddo
             thlyr(nm,k) = thunlyr
          else
             !
             ! everything can be added to this layer
             !
             newthlyr = thlyr(nm, k)
             do l = 1,this%settings%nfrac
                lyrfrac(nm, k, l) = lyrfrac(nm, k, l)*thlyr(nm, k) + dzi(l)
                newthlyr          = newthlyr + dzi(l)
             enddo
             do l = 1,this%settings%nfrac
                lyrfrac(nm, k, l) = lyrfrac(nm, k, l)/newthlyr
             enddo
             thlyr(nm, k) = thlyr(nm, k) + dz
             dz           = 0.0
          endif
       endif
       k = k-1
    enddo
    !
    ! the first underlayer is completely filled or
    ! all sediment deposited
    !
    if (dz > 0.0) then
       !
       ! still more sediment to be deposited
       !
       if (nlyr == kmin) then
          !
          ! no thin underlayers, so put everything in
          ! the last (i.e. base) layer
          !
          select case (updbaselyr)
          case(1) ! compute separate composition for the base layer
             k        = nlyr
             newthlyr = thlyr(nm, k)
             do l = 1,this%settings%nfrac
                lyrfrac(nm, k, l) = lyrfrac(nm, k, l)*thlyr(nm, k) + dzi(l)
                newthlyr          = newthlyr + dzi(l)
             enddo
             thlyr(nm, k) = newthlyr
             do l = 1,this%settings%nfrac
                lyrfrac(nm, k, l) = lyrfrac(nm, k, l)/newthlyr
             enddo
             dz = 0.0
          case(2) ! composition of base layer constant
             !
             ! composition of dz is lost, update thickness
             !
             k            = nlyr
             thlyr(nm, k) = thlyr(nm, k) + dz
          case(3) ! same as the (first non-empty) layer above it
             !
             ! composition of dz is lost, update thickness
             ! and set composition to that of transp. or exchange
             ! layer
             !
             k            = nlyr
             thlyr(nm, k) = thlyr(nm, k) + dz
             do l = 1,this%settings%nfrac
                lyrfrac(nm, k, l) = lyrfrac(nm, kmin-1, l)
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
          do while (dz > 0.0)
             !
             ! add last thin underlayer to the last (i.e. base) layer
             !
             newthlyr = thlyr(nm, nlyr)+thlyr(nm, nlyr-1)
             select case (updbaselyr)
             case(1) ! compute separate composition for the base layer
                if (newthlyr > 0.0) then
                   do l = 1,this%settings%nfrac
                      lyrfrac(nm, nlyr, l) =                            &
                         & (  lyrfrac(nm, nlyr , l) * thlyr(nm,nlyr)   &
                         &  + lyrfrac(nm, nlyr-1, l) * thlyr(nm,nlyr-1)) / newthlyr
                   enddo
                endif
             case(2) ! composition of base layer constant
                !
                ! composition of layer nlyr-1 is lost; just the
                ! thickness of the base layer is updated
                !
             case(3) ! same as the (first non-empty) layer above it
                !
                ! find lowest non-empty layer
                !
                do kne = nlyr-2,1,-1
                   if (thlyr(nm,kne)>0.0) exit
                enddo
                do l = 1, this%settings%nfrac
                   lyrfrac(nm,nlyr,l) = lyrfrac(nm,kne,l)
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
             do k = nlyr-1,kmin+1,-1
                do l = 1,this%settings%nfrac
                   lyrfrac(nm,k,l) = lyrfrac(nm,k-1,l)
                enddo
                thlyr(nm,k) = thlyr(nm,k-1)
             enddo
             !
             ! put all the sediment in one layer
             !
             thlyr(nm,kmin) = dz
             do l = 1, this%settings%nfrac
                lyrfrac(nm,kmin,l) = dzi(l)/dz
             enddo
             dz = 0.0
          enddo
       endif
    endif
end subroutine lyrsedimentation
!
!
!
!==============================================================================
subroutine detthcmud(this, cdryb, sedtyp, thcmud)
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
    type(bedcomp_data), intent(in)                                          :: this
    real(fp), dimension(this%settings%nfrac), intent(in)                    :: cdryb
    real(fp), dimension(this%settings%nmlb:this%settings%nmub), intent(out) :: thcmud  !  Total thickness of the mud layers
    character(4), dimension(this%settings%nfrac), intent(in)                :: sedtyp
!
! Local variables
!
    integer                             :: l
    integer                             :: nm
    real(prec), dimension(:,:), pointer :: bodsed
!
!! executable statements -------------------------------------------------------
!
    bodsed      => this%state%bodsed
    !
    do nm = this%settings%nmlb,this%settings%nmub
       thcmud (nm) = 0.0
        do l = 1, this%settings%nfrac
           if (sedtyp(l) == 'mud ') thcmud(nm) = thcmud(nm) + real(bodsed(nm, l),fp)/cdryb(l)
        enddo
    enddo
end subroutine detthcmud
!
!
!
!==============================================================================
subroutine getalluvthick(this, cdryb, seddep)
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
    type(bedcomp_data), intent(in)                                                               :: this
    real(fp), dimension(this%settings%nfrac), intent(in)                                         :: cdryb
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
!
!! executable statements -------------------------------------------------------
!
    nlyr                => this%settings%nlyr
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
             seddep(nm, l) = real(bodsed(nm, l),fp)/cdryb(l)
          enddo
       enddo
    endselect
end subroutine getalluvthick
!
!
!
!==============================================================================
subroutine getfrac(this, cdryb, frac, sedtyp, anymud, mudcnt, mudfrac)
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
    type(bedcomp_data), intent(in)                                                  :: this    
    logical                                                                         :: anymud
    real(fp), dimension(this%settings%nfrac), intent(in)                            :: cdryb
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac) :: frac
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                      :: mudfrac
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                      :: mudcnt
    character(4), dimension(this%settings%nfrac), intent(in)                        :: sedtyp
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
       call getmfrac_allpoints(this ,cdryb ,frac      )
    case default
       call getvfrac_allpoints(this ,cdryb ,frac      )
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
subroutine getmfrac_allpoints(this, cdryb, frac)
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
    type(bedcomp_data), intent(in)                                                  :: this
    real(fp), dimension(this%settings%nfrac), intent(in)                            :: cdryb
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    integer                             :: nm
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: lyrfrac
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    lyrfrac     => this%state%lyrfrac
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb,this%settings%nmub
          sedtot = 0.0
          do l = 1, this%settings%nfrac
             frac(nm, l) = lyrfrac(nm, 1, l)
             sedtot      = sedtot + frac(nm, l)*cdryb(l)
          enddo
          if (comparereal(sedtot,0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             do l = 1, this%settings%nfrac
                frac(nm, l) = frac(nm, l)*cdryb(l)/sedtot
             enddo
          endif
       enddo
    case default
       do nm = this%settings%nmlb,this%settings%nmub
          sedtot = 0.0
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
subroutine getmfrac_1point(this ,nm ,cdryb ,frac      )
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
    type(bedcomp_data), intent(in)                        :: this    
    integer, intent(in)                                   :: nm
    real(fp), dimension(this%settings%nfrac), intent(in)  :: cdryb
    real(fp), dimension(this%settings%nfrac), intent(out) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: lyrfrac
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    lyrfrac     => this%state%lyrfrac
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       sedtot = 0.0
       do l = 1, this%settings%nfrac
          frac(l) = lyrfrac(nm, 1, l)
          sedtot      = sedtot + frac(l)*cdryb(l)
       enddo
       if (comparereal(sedtot,0.0_fp) == 0) then
          frac = 1.0_fp/this%settings%nfrac
       else
          do l = 1, this%settings%nfrac
             frac(l) = frac(l)*cdryb(l)/sedtot
          enddo
       endif
    case default
       sedtot = 0.0
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
subroutine setmfrac_1point(this, nm, cdryb, frac)
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
    type(bedcomp_data), intent(in)                        :: this
    integer, intent(in)                                   :: nm
    real(fp), dimension(this%settings%nfrac), intent(in)  :: cdryb
    real(fp), dimension(this%settings%nfrac), intent(in)  :: frac
    !
    ! Local variables
    !
    integer                             :: l
    real(fp)                            :: masstot
    real(fp)                            :: voltot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: lyrfrac
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    lyrfrac     => this%state%lyrfrac
    !
    ! Calculate total bottom sediments and fractions
    !
    masstot = 1000.0_fp
    select case(this%settings%iunderlyr)
    case(2)
       voltot = 0.0_fp
       do l = 1, this%settings%nfrac
          lyrfrac(nm, 1, l) = frac(l)*masstot/cdryb(l)
          voltot = voltot + lyrfrac(nm, 1, l)
       enddo
       !
       do l = 1, this%settings%nfrac
          lyrfrac(nm, 1, l) = lyrfrac(nm, 1, l)/voltot
       enddo
    case default
       do l = 1, this%settings%nfrac
          bodsed(nm, l) = real(frac(l)*masstot,prec)
       enddo
    endselect
end subroutine setmfrac_1point
!
!
!
!==============================================================================
subroutine getvfrac_allpoints(this, cdryb, frac)
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
    type(bedcomp_data), intent(in)                                                  :: this    
    real(fp), dimension(this%settings%nfrac), intent(in)                            :: cdryb
    real(fp), dimension(this%settings%nmlb:this%settings%nmub, this%settings%nfrac) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    integer                             :: nm
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: lyrfrac
!
!! executable statements -------------------------------------------------------
!
    bodsed      => this%state%bodsed
    lyrfrac     => this%state%lyrfrac
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       do nm = this%settings%nmlb, this%settings%nmub
          do l = 1, this%settings%nfrac
             frac(nm, l) = lyrfrac(nm, 1, l)
          enddo
       enddo
    case default
       do nm = this%settings%nmlb, this%settings%nmub
          sedtot = 0.0
          do l = 1, this%settings%nfrac
             sedtot = sedtot + real(bodsed(nm, l),fp)/cdryb(l)
          enddo
          if (comparereal(sedtot,0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             do l = 1, this%settings%nfrac
                frac(nm, l) = real(bodsed(nm, l),fp)/sedtot/cdryb(l)
             enddo
          endif
       enddo
    endselect
end subroutine getvfrac_allpoints
!
!
!
!==============================================================================
subroutine getvfrac_1point(this ,nm ,cdryb ,frac      )
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
    type(bedcomp_data), intent(in)                        :: this
    integer, intent(in)                                   :: nm
    real(fp), dimension(this%settings%nfrac), intent(in)  :: cdryb
    real(fp), dimension(this%settings%nfrac), intent(out) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: lyrfrac
!
!! executable statements -------------------------------------------------------
!
    bodsed      => this%state%bodsed
    lyrfrac     => this%state%lyrfrac
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       do l = 1, this%settings%nfrac
          frac(l) = lyrfrac(nm, 1, l)
       enddo
    case default
       sedtot = 0.0
       do l = 1, this%settings%nfrac
          sedtot = sedtot + real(bodsed(nm, l),fp)/cdryb(l)
       enddo
       if (comparereal(sedtot,0.0_fp) == 0) then
          frac = 1.0_fp/this%settings%nfrac
       else
          do l = 1, this%settings%nfrac
             frac(l) = real(bodsed(nm, l),fp)/sedtot/cdryb(l)
          enddo
       endif
    endselect
end subroutine getvfrac_1point
!
!
!
!==============================================================================
subroutine setvfrac_1point(this, nm, cdryb, frac)
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
    type(bedcomp_data), intent(in)                        :: this    
    integer, intent(in)                                   :: nm
    real(fp), dimension(this%settings%nfrac), intent(in)  :: cdryb
    real(fp), dimension(this%settings%nfrac), intent(in)  :: frac
    !
    ! Local variables
    !
    integer                             :: l
    real(fp)                            :: voltot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: lyrfrac
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    lyrfrac     => this%state%lyrfrac
    !
    ! Calculate total bottom sediments and fractions
    !
    select case(this%settings%iunderlyr)
    case(2)
       do l = 1, this%settings%nfrac
          lyrfrac(nm, 1, l) = frac(l)
       enddo
    case default
       voltot = 1000.0_fp
       do l = 1, this%settings%nfrac
          bodsed(nm, l) = real(frac(l)*voltot*cdryb(l),prec)
       enddo
    endselect
end subroutine setvfrac_1point
!
!
!
!==============================================================================
subroutine getsedthick(this, nm, seddep)
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
    type(bedcomp_data), intent(in) :: this    
    integer, intent(in)            :: nm
    real(fp), intent(out)          :: seddep
    !
    ! Local variables
    !
    integer                           :: k
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
       do k = 1, this%settings%nlyr
          seddep = seddep + thlyr(nm, k)
       enddo
    case default
       seddep = dpsed(nm)
    endselect
end subroutine getsedthick
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
    type (bedcomp_data), intent(in) :: this    
    integer                         :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer :: settings
    type (bedcomp_work), pointer     :: work
    type (bedcomp_state), pointer    :: state
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    if (istat == 0) allocate (this%settings, stat = istat)
    if (istat == 0) allocate (this%work , stat = istat)
    if (istat == 0) allocate (this%state , stat = istat)
    if (istat /= 0) then
       !error
       return
    endif
    !
    settings => this%settings
    allocate (settings%morlyrnum , stat = istat)
    if (istat == 0) then
       settings%morlyrnum%MinThickShortWarning = 0.0_fp
       settings%morlyrnum%MaxNumShortWarning = 100
    endif
    !
    settings%nfrac      = 0
    settings%nlyr       = 0
    settings%nmlb       = 0
    settings%nmub       = 0
    settings%iunderlyr  = 1
    settings%exchlyr    = .false.
    settings%mxnulyr    = 0
    settings%thunlyr    = rmissval
    settings%updbaselyr = 1
    !
    work => this%work
    nullify(work%dzi)
    nullify(settings%thexlyr)
    nullify(settings%thtrlyr)
    !
    state => this%state
    nullify(state%bodsed)
    nullify(state%dpsed)
    nullify(state%lyrfrac)
    nullify(state%thlyr)
    nullify(state%sedshort)
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
    integer, intent(in)             :: nmlb
    integer, intent(in)             :: nmub
    integer, intent(in)             :: nfrac
    type (bedcomp_data), intent(in) :: this    
    integer                         :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer :: settings
    type (bedcomp_work), pointer     :: work
    type (bedcomp_state), pointer    :: state
    !
    !! executable statements -------------------------------------------------------
    !
    settings => this%settings
    work => this%work
    state => this%state
    !
    ! Number of layers: 1       transport layer
    !                   1       exchange layer (optional)
    !                   mxnulyr underlayers
    !                   1       persistent base layer
    !
    settings%nlyr = 2 + settings%mxnulyr
    if (settings%exchlyr) then
       settings%nlyr = settings%nlyr + 1
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
       !
       if (istat == 0) allocate (work%dzi(nfrac), stat = istat)
       if (istat == 0) work%dzi = 0.0_fp
       !
       if (istat == 0) allocate (state%lyrfrac(nmlb:nmub,settings%nlyr,nfrac), stat = istat)
       if (istat == 0) then
          state%lyrfrac = 0.0_fp
          state%lyrfrac(:,:,nfrac) = 1.0_fp
       endif
       if (istat == 0) allocate (state%thlyr(nmlb:nmub,settings%nlyr), stat = istat)
       if (istat == 0) state%thlyr = 0.0_fp
       if (istat == 0) allocate (state%sedshort(nmlb:nmub,nfrac), stat = istat)
       if (istat == 0) state%sedshort = 0.0_fp
    endif
end function allocmorlyr
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
    type (bedcomp_work), pointer     :: work
    type (bedcomp_state), pointer    :: state
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
    if (associated(this%work)) then
       work => this%work
       if (associated(work%dzi))           deallocate(work%dzi , STAT = istat)
       !
       deallocate(this%work, STAT = istat)
       nullify(this%work)
    endif
    !
    if (associated(this%state)) then
       state => this%state
       if (associated(state%bodsed))       deallocate(state%bodsed , STAT = istat)
       if (associated(state%dpsed))        deallocate(state%dpsed , STAT = istat)
       if (associated(state%lyrfrac))      deallocate(state%lyrfrac , STAT = istat)
       if (associated(state%thlyr))        deallocate(state%thlyr , STAT = istat)
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
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
    case ('nlyr')
       val => this%settings%nlyr
    case ('maxnumshortwarning')
       val => this%settings%morlyrnum%MaxNumShortWarning
    case ('mxnulyr')
       val => this%settings%mxnulyr
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
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
    case ('thunlyr')
       val => this%settings%thunlyr
    case ('minthickshortwarning')
       val => this%settings%morlyrnum%MinThickShortWarning
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
    real(fp), dimension(:), pointer     :: val
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
    real(fp), dimension(:,:), pointer   :: val
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
    real(fp), dimension(:,:,:), pointer :: val
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
    case ('lyrfrac')
       val => this%state%lyrfrac
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
    type(bedcomp_data), intent(in)      :: this    
    character(*), intent(in)            :: variable
    real(prec), dimension(:,:), pointer :: val
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
subroutine bedcomp_use_bodsed(this, cdryb)
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
    type(bedcomp_data), intent(in)                       :: this
    real(fp), dimension(this%settings%nfrac), intent(in) :: cdryb
    !
    ! Local variables
    !
    integer :: ised
    integer :: k
    integer :: kstart
    integer :: nm
    real(fp) :: sedthick
    real(fp) :: sedthicklim
    real(prec), dimension(:,:) , pointer :: bodsed
    real(fp) , dimension(:) , pointer :: dpsed
    real(fp) , dimension(:,:,:), pointer :: lyrfrac
    real(fp) , dimension(:,:) , pointer :: thlyr
    real(fp) , dimension(:) , pointer :: thtrlyr
    real(fp) , dimension(:) , pointer :: thexlyr
    !
    !! executable statements -------------------------------------------------------
    thtrlyr => this%settings%thtrlyr
    thexlyr => this%settings%thexlyr
    bodsed => this%state%bodsed
    dpsed => this%state%dpsed
    lyrfrac => this%state%lyrfrac
    thlyr => this%state%thlyr
    !
    ! Fill initial values of DPSED
    !
    ! calculate density of the mixing layer based on mixture of grain
    ! sizes present. This should be a function of x,y,t.
    ! Note the current expression is just a trivial version of this.
    !
    do nm = this%settings%nmlb, this%settings%nmub
       !
       ! Compute thickness correctly in case of
       ! multiple fractions with different cdryb
       !
       dpsed(nm) = 0.0
       do ised = 1, this%settings%nfrac
          dpsed(nm) = dpsed(nm) + real(bodsed(nm, ised),fp)/cdryb(ised)
       enddo
    enddo
    select case(this%settings%iunderlyr)
    case(2)
       !
       ! No file specified for initial bed composition: extract data from
       ! the BODSED data read above.
       !
       do nm = this%settings%nmlb, this%settings%nmub
          !if (kcs(nm)<1 .or. kcs(nm)>2) cycle  !TODO: find a solution for this line
          !
          ! nm = (m-1)*nmax + n
          !
          sedthick = 0.0_fp
          do ised = 1, this%settings%nfrac
             sedthick = sedthick + real(bodsed(nm, ised),fp)/cdryb(ised)
          enddo
          sedthicklim = max(sedthick, 1.0e-20_fp)
          do ised = 1, this%settings%nfrac
             lyrfrac(nm, :, ised) = real(bodsed(nm, ised),fp)/cdryb(ised) / sedthicklim
          enddo
          thlyr(nm, 1) = min(thtrlyr(nm),sedthick)
          sedthick     = sedthick - thlyr(nm, 1)
          kstart       = 1
          if (this%settings%exchlyr) then
             kstart       = 2
             thlyr(nm, 2) = min(thexlyr(nm),sedthick)
             sedthick     = sedthick - thlyr(nm, 2)
          endif
          do k = kstart+1,this%settings%nlyr-1
             thlyr(nm, k) = min(this%settings%thunlyr,sedthick)
             sedthick     = sedthick - thlyr(nm, k)
          enddo
          thlyr(nm, this%settings%nlyr) = sedthick
       enddo
    case default
       !
       ! nothing to do, using bodsed as uniformly mixed sediment
       !
    endselect
end subroutine


end module bedcomposition_module
