!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id: monitoring.f90 54191 2018-01-22 18:57:53Z dam_ar $
! $HeadURL: https://repos.deltares.nl/repos/ds/trunk/additional/unstruc/src/monitoring.f90 $

!> @file monitoring.f90
!! Monitoring modules (data+routines).
!! m_observations and m_monitoring_crosssections
!<

!> Observation stations can be used to monitor flow data at fixed points
!! in the domain. Which data is monitored is configured elsewhere
!! (output routine history file)
!! In arrays: (1:numobs = normal obs, numobs+1:numobs+nummovobs = moving obs)
module m_observations

use m_alloc
use m_missing
use m_flowexternalforcings

implicit none

    integer                           :: numobs    = 0  !< nr of observation stations
    integer                           :: nummovobs = 0  !< nr of *moving* observation stations
    double precision, allocatable     :: xobs(:)        !< x-coord of observation points (1:numobs = normal obs, numobs+1:numobs+nummovobs = moving obs)
    double precision, allocatable     :: yobs(:)        !< y-coord of observation points
    double precision, allocatable, target :: xyobs(:)   !< xy-coord of *moving* observation points (work array for meteo)
    double precision, allocatable     :: smxobs(:)      !< maximum waterlevel of observation points
    double precision, allocatable     :: cmxobs(:)      !< maximum 2D flow velocity of observation points, 3D: maximum over all layers and time
    integer, allocatable              :: kobs(:)        !< node nrs of ACTIVE observation points
    ! NOTE: kobs is not maintained here (so also not after deleteObservation, etc.) All done once by obs_on_flowgrid.
    character(len=40), allocatable    :: namobs(:)      ! names of observation points

    integer, parameter, private       :: capacity_ = 1  !< Nr of additionally allocated elements when lists are full
    integer, private                  :: iUniq_ = 1
    character(len=*), parameter, private :: defaultName_ = 'Obs'
    integer                           :: mxls           !< Unit nr hisdump to excel
    integer                           :: jafahrenheit=0 !< Output in Celsius, otherwise Fahrenheit 
    

    double precision                  :: tlastupd_valobs !< Time at which the valobs array was last updated.
    double precision, dimension(:,:), allocatable, target :: valobs     !< work array with 2d and 3d values stored at observation stations, dim(MAXNUMVALOBS2D+MAXNUMVALOBS3D*max(kmx,1)+MAXNUMVALOBS3Dw*(max(kmx,1)+1),numobs+nummovobs)
    double precision, dimension(:,:), allocatable         :: valobs_all !< work array with 2d and 3d values stored at observation stations, dim(MAXNUMVALOBS2D+MAXNUMVALOBS3D*max(kmx,1)+MAXNUMVALOBS3Dw*(max(kmx,1)+1),numobs+nummovobs)
    
    integer                           :: MAXNUMVALOBS2D   ! maximum number of outputted values at observation stations
    integer                           :: MAXNUMVALOBS3D   ! maximum number of outputted values at observation stations, 3D layer centers
    integer                           :: MAXNUMVALOBS3Dw  ! maximum number of outputted values at observation stations, 3D layer interfaces (e.g. zws)
    integer                           :: IVAL_S1          ! 2D first
    integer                           :: IVAL_HS
    integer                           :: IVAL_BL
    integer                           :: IVAL_SMX
    integer                           :: IVAL_CMX
    integer                           :: IVAL_WX  
    integer                           :: IVAL_WY
    integer                           :: IVAL_PATM
    integer                           :: IVAL_RAIN
    integer                           :: IVAL_WAVEH
    integer                           :: IVAL_WAVER
    integer                           :: IVAL_UCX         ! 3D, layer centered after 2D
    integer                           :: IVAL_UCY
    integer                           :: IVAL_UCZ
    integer                           :: IVAL_SA1
    integer                           :: IVAL_TEM1
    integer                           :: IVAL_TRA1
    integer                           :: IVAL_TRAN
    integer                           :: IVAL_SED ! HK code
    integer                           :: IVAL_SF1 ! stm code
    integer                           :: IVAL_SFN
    integer                           :: IVAL_ZCS
    integer                           :: IVAL_ZWS         ! 3D, layer interfaces after layer centered
    integer                           :: IVAL_TKIN
    integer                           :: IVAL_TEPS
    integer                           :: IVAL_VICWW
    integer                           :: IVAL_WS1
    integer                           :: IVAL_WSN
    integer                           :: IVAL_SEDDIF1
    integer                           :: IVAL_SEDDIFN
    integer                           :: IVAL_RICH
    integer                           :: IVAL_TAIR
    integer                           :: IVAL_WIND
    integer                           :: IVAL_RHUM
    integer                           :: IVAL_CLOU
    integer                           :: IVAL_QSUN
    integer                           :: IVAL_QEVA
    integer                           :: IVAL_QCON
    integer                           :: IVAL_QLON
    integer                           :: IVAL_QFRE
    integer                           :: IVAL_QFRC
    integer                           :: IVAL_QTOT
    integer                           :: IVAL_RHO
    
    integer                           :: IPNT_S1            ! pointers in valobs work array
    integer                           :: IPNT_HS
    integer                           :: IPNT_BL
    integer                           :: IPNT_SMX
    integer                           :: IPNT_CMX
    integer                           :: IPNT_WX
    integer                           :: IPNT_WY
    integer                           :: IPNT_RAIN
    integer                           :: IPNT_PATM
    integer                           :: IPNT_WAVEH
    integer                           :: IPNT_WAVER
    integer                           :: IPNT_UCX
    integer                           :: IPNT_UCY
    integer                           :: IPNT_UCZ
    integer                           :: IPNT_SA1
    integer                           :: IPNT_TEM1
    integer                           :: IPNT_TRA1
    integer                           :: IPNT_TRAN
!    integer                           :: IPNT_SPIR1
    integer                           :: IPNT_SF1
    integer                           :: IPNT_SFN
    integer                           :: IPNT_SED
    integer                           :: IPNT_ZCS
    integer                           :: IPNT_ZWS
    integer                           :: IPNT_TKIN
    integer                           :: IPNT_TEPS
    integer                           :: IPNT_VICWW
    integer                           :: IPNT_WS1
    integer                           :: IPNT_WSN
    integer                           :: IPNT_SEDDIF1
    integer                           :: IPNT_SEDDIFN
    integer                           :: IPNT_RICH
    integer                           :: IPNT_TAIR
    integer                           :: IPNT_WIND
    integer                           :: IPNT_RHUM
    integer                           :: IPNT_CLOU
    integer                           :: IPNT_QSUN
    integer                           :: IPNT_QEVA
    integer                           :: IPNT_QCON
    integer                           :: IPNT_QLON
    integer                           :: IPNT_QFRE
    integer                           :: IPNT_QFRC
    integer                           :: IPNT_QTOT
    integer                           :: IPNT_NUM
    integer                           :: IPNT_RHO
contains

!> (re)initialize valobs and set pointers for observation stations
subroutine init_valobs()
   implicit none
   
   tlastupd_valobs = dmiss
   call init_valobs_pointers()
   
   call alloc_valobs()
   
   return
end subroutine init_valobs

!> (re)allocate valobs work array
subroutine alloc_valobs()
   use m_partitioninfo
   implicit none
   
   if ( allocated(valobs) ) then
      deallocate(valobs)
   end if
   
   if ( IPNT_NUM.gt.0 ) then
      allocate(valobs(IPNT_NUM,numobs+nummovobs))
      valobs = 0d0   ! should not be DMISS, since DMISS is used for global reduction in parallel computations
   end if
   
   if ( jampi.eq.1 ) then
      if ( allocated(valobs_all) ) then
         deallocate(valobs_all)
      end if
      allocate(valobs_all(IPNT_NUM,numobs+nummovobs))
   end if
   
   return
end subroutine alloc_valobs

!> set the pointers in the valobs work array
!! only include variables that are available
!! IVAL_XXX are the enumerators >0 when available
!! IPNT_XXX are the pointers in the "valobs" array,
!! which is being reduced in parallel runs
subroutine init_valobs_pointers()
   use m_flowparameters
   use m_flow, only: iturbulencemodel, idensform, kmx
   use m_transport, only: ITRA1, ITRAN, ISED1, ISEDN
   use m_sediment, only: stm_included
   implicit none
   
   integer             :: i, i0
   
   MAXNUMVALOBS2D  = 0
   MAXNUMVALOBS3D  = 0
   MAXNUMVALOBS3Dw = 0
   
!  initialize
   IVAL_S1         = 0
   IVAL_HS         = 0
   IVAL_BL         = 0
   IVAL_SMX        = 0
   IVAL_CMX        = 0
   IVAL_WX         = 0
   IVAL_WY         = 0
   IVAL_PATM       = 0
   IVAL_WAVEH      = 0
   IVAL_WAVER      = 0
   IVAL_UCX        = 0
   IVAL_UCY        = 0
   IVAL_UCZ        = 0
   IVAL_SA1        = 0
   IVAL_TEM1       = 0
   IVAL_TRA1       = 0
   IVAL_TRAN       = 0
   IVAL_SF1        = 0
   IVAL_SFN        = 0
   IVAL_SED        = 0
   IVAL_ZCS        = 0
   IVAL_ZWS        = 0
   IVAL_TKIN       = 0
   IVAL_TEPS       = 0
   IVAL_VICWW      = 0
   IVAL_RICH       = 0
   IVAL_WS1        = 0
   IVAL_WSN        = 0
   IVAL_SEDDIF1    = 0
   IVAL_SEDDIFN    = 0
   IVAL_TAIR       = 0
   IVAL_WIND       = 0
   IVAL_RHUM       = 0
   IVAL_CLOU       = 0
   IVAL_QSUN       = 0
   IVAL_QEVA       = 0
   IVAL_QCON       = 0
   IVAL_QLON       = 0
   IVAL_QFRE       = 0
   IVAL_QFRC       = 0
   IVAL_QTOT       = 0
   IVAL_RAIN       = 0
   IVAL_RHO        = 0
   
!  2D
   i=0
   i0=i;
   i=i+1;               IVAL_S1         = i
   i=i+1;               IVAL_HS         = i
   i=i+1;               IVAL_BL         = i
   i=i+1;               IVAL_SMX        = i
   i=i+1;               IVAL_CMX        = i
   if ( jawind.gt.0 ) then
      i=i+1;            IVAL_WX         = i
      i=i+1;            IVAL_WY         = i
   end if
   if ( japatm.gt.0 ) then
      i=i+1;            IVAL_PATM       = i
   end if
   if ( jawave.gt.0 ) then
      i=i+1;            IVAL_WAVEH      = i
      i=i+1;            IVAL_WAVER      = i
   end if
   if ( jatem.gt.1 ) then
      i=i+1;            IVAL_TAIR       = i
   end if
   if ( jawind.gt.0 ) then
      i=i+1;            IVAL_WIND       = i
   end if
   if ( jatem.eq.5 ) then
      i=i+1;            IVAL_RHUM       = i
      i=i+1;            IVAL_CLOU       = i
      i=i+1;            IVAL_QSUN       = i
      i=i+1;            IVAL_QEVA       = i
      i=i+1;            IVAL_QCON       = i
      i=i+1;            IVAL_QLON       = i
      i=i+1;            IVAL_QFRE       = i
      i=i+1;            IVAL_QFRC       = i
   end if
   if ( jatem.gt.1 ) then
      i=i+1;            IVAL_QTOT       = i
   end if
   if ( jahisrain.gt.0 ) then
      i=i+1;            IVAL_RAIN       = i
   end if
   MAXNUMVALOBS2D                       = i-i0
   
!  3D, layer centered
   i0=i;
   i=i+1;               IVAL_UCX        = i
   i=i+1;               IVAL_UCY        = i
   if ( kmx.gt.0 ) then
      i=i+1;            IVAL_UCZ        = i
   end if
   if ( jasal.gt.0 ) then
      i=i+1;            IVAL_SA1        = i
   end if
   if ( jatem.gt.0 ) then
      i=i+1;            IVAL_TEM1       = i
   end if
   if ( ITRA1.gt.0 ) then
      i=i+1;            IVAL_TRA1       = i
      i=i+ITRAN-ITRA1;  IVAL_TRAN       = i  !< All tracers (NOT only the ones with bnd)
   end if
   if ( stm_included .and. ISED1.gt.0 ) then
      i=i+1;              IVAL_SF1       = i
      i=i+ISEDN-ISED1;    IVAL_SFN       = i 
   end if
   if ( jased.gt.0 .and. .not. stm_included) then
      i=i+1;            IVAL_SED        = i
   end if
   if ( kmx.gt.0 ) then
      i=i+1;            IVAL_ZCS        = i
   end if
   if( jasal > 0 .or. jatem > 0 .or. jased > 0 ) then
      i=i+1;            IVAL_RHO        = i
   endif
   MAXNUMVALOBS3D                       = i-i0

!  3D, layer interfaces
   i0=i;
   if ( kmx.gt.0 ) then
      i=i+1;            IVAL_ZWS        = i
      if ( iturbulencemodel.gt.0 ) then
         i=i+1;         IVAL_TKIN       = i
         i=i+1;         IVAL_TEPS       = i
         i=i+1;         IVAL_VICWW      = i
      end if
      if ( idensform.gt.0 ) then
         i=i+1;         IVAL_RICH       = i
      end if
      if ( stm_included .and. ISED1.gt.0 ) then
         i=i+1;                   IVAL_WS1        = i
         i=i+ISEDN-ISED1;         IVAL_WSN        = i
         i=i+1;                   IVAL_SEDDIF1    = i
         i=i+ISEDN-ISED1;         IVAL_SEDDIFN    = i
      end if
   end if
   MAXNUMVALOBS3Dw                      = i-i0
   
!  set pointers in valobs array   
   IPNT_S1    = ivalpoint(IVAL_S1,    kmx)  ! kmx > 1 for non 3D quantitites?  antwoord: nee, omdat bijv. IVAL_S1 <= MAXNUMVALOBS2D
   IPNT_HS    = ivalpoint(IVAL_HS,    kmx)
   IPNT_BL    = ivalpoint(IVAL_BL,    kmx)
   IPNT_SMX   = ivalpoint(IVAL_SMX,   kmx)
   IPNT_CMX   = ivalpoint(IVAL_CMX,   kmx)
   IPNT_UCX   = ivalpoint(IVAL_UCX,   kmx)
   IPNT_UCY   = ivalpoint(IVAL_UCY,   kmx)
   IPNT_UCZ   = ivalpoint(IVAL_UCZ,   kmx)   
   IPNT_SA1   = ivalpoint(IVAL_SA1,   kmx)
   IPNT_TEM1  = ivalpoint(IVAL_TEM1,  kmx)
   IPNT_TRA1  = ivalpoint(IVAL_TRA1,  kmx)
   IPNT_TRAN  = ivalpoint(IVAL_TRAN,  kmx)
   IPNT_SF1   = ivalpoint(IVAL_SF1,   kmx)
   IPNT_SFN   = ivalpoint(IVAL_SFN,   kmx)
!   IPNT_SPIR1 = ivalpoint(IVAL_SPIR1, kmx)
   IPNT_SED   = ivalpoint(IVAL_SED,   kmx)
   IPNT_WX    = ivalpoint(IVAL_WX ,   kmx)
   IPNT_WY    = ivalpoint(IVAL_WY ,   kmx)
   IPNT_PATM  = ivalpoint(IVAL_PATM,  kmx)
   IPNT_WAVEH = ivalpoint(IVAL_WAVEH, kmx)
   IPNT_WAVER = ivalpoint(IVAL_WAVER, kmx)
   IPNT_ZCS   = ivalpoint(IVAL_ZCS,   kmx)
   IPNT_ZWS   = ivalpoint(IVAL_ZWS,   kmx)
   IPNT_TKIN  = ivalpoint(IVAL_TKIN,  kmx)
   IPNT_TEPS  = ivalpoint(IVAL_TEPS,  kmx)
   IPNT_VICWW = ivalpoint(IVAL_VICWW, kmx)
   IPNT_RICH  = ivalpoint(IVAL_RICH,  kmx)
   IPNT_RHO   = ivalpoint(IVAL_RHO,   kmx)
   IPNT_WS1   = ivalpoint(IVAL_WS1,   kmx)
   IPNT_WSN   = ivalpoint(IVAL_WSN,   kmx)
   IPNT_SEDDIF1 = ivalpoint(IVAL_SEDDIF1,   kmx)
   IPNT_SEDDIFN = ivalpoint(IVAL_SEDDIFN,   kmx)
   
   IPNT_TAIR  = ivalpoint(IVAL_TAIR,  kmx)
   IPNT_WIND  = ivalpoint(IVAL_WIND,  kmx)
   IPNT_RHUM  = ivalpoint(IVAL_RHUM,  kmx)
   IPNT_CLOU  = ivalpoint(IVAL_CLOU,  kmx)
   IPNT_QSUN  = ivalpoint(IVAL_QSUN,  kmx)
   IPNT_QEVA  = ivalpoint(IVAL_QEVA,  kmx)
   IPNT_QCON  = ivalpoint(IVAL_QCON,  kmx)
   IPNT_QLON  = ivalpoint(IVAL_QLON,  kmx)
   IPNT_QFRE  = ivalpoint(IVAL_QFRE,  kmx)
   IPNT_QFRC  = ivalpoint(IVAL_QFRC,  kmx)
   IPNT_QTOT  = ivalpoint(IVAL_QTOT,  kmx)
   IPNT_RAIN  = ivalpoint(IVAL_RAIN,  kmx)
   
   IPNT_NUM   = ivalpoint(0,          kmx)-1
   
   return
   
end subroutine init_valobs_pointers

!> pointer of variable in valobs work array
integer function ivalpoint(ivar, kmx)
   use messageHandling
   
   implicit none
   
   integer, intent(in) :: ivar   !< observation station variable number
   integer, intent(in) :: kmx    !< maximum number of layers
   
   integer             :: i, istart, iend
   
   ivalpoint = 1
   
   istart = 0
   iend   = 0
   
!  2D
   istart = iend+1
   iend   = iend+MAXNUMVALOBS2D
   do i=1,MAXNUMVALOBS2D
      if ( i.eq.ivar ) return
      ivalpoint = ivalpoint + 1
   end do
   
!  3D, layer centers (dim(kmx))
   istart = iend+1
   iend   = iend+MAXNUMVALOBS3D
   do i=istart,iend
      if ( i.eq.ivar ) return
      ivalpoint = ivalpoint + max(kmx,1)
   end do
   
!  3D, layer interfaces (dim(kmx+1))
   istart = iend+1
   iend   = iend+MAXNUMVALOBS3Dw
   do i=istart,iend
      if ( i.eq.ivar ) return
      ivalpoint = ivalpoint + max(kmx,1) + 1
   end do
   
   if ( ivar.ne.0 ) then
      call mess(LEVEL_ERROR, 'ivalpoint: numbering error')
   end if
   
   return
end function ivalpoint

!> Returns the index/position of a named station in the global set arrays of this module.
subroutine getObservationIndex(statname, index)
   character(len=*), intent(in)  :: statname
   integer,          intent(out) :: index !< The position of the (possibly moving) observation station in all set arrays. 0 if not present.

   integer :: i

   index = 0
   do i=1,numobs+nummovobs
      if (trim(namobs(i)) == trim(statname)) then
         index = i
         exit
      end if
   end do
end subroutine getObservationIndex

   
!> Removes the observation point at indicated list position.
subroutine updateObservationXY(pos, xnew, ynew)
    integer, intent(in) :: pos
    double precision, intent(in) :: xnew, ynew

    if (pos <= numobs+nummovobs) then
        xobs(pos) = xnew
        yobs(pos) = ynew
    end if
end subroutine updateObservationXY


!> Adds an observation point to the existing points.
!! New observation point may be a moving one or not.
subroutine addObservation(x, y, name, isMoving)
use m_alloc
    double precision, intent(in) :: x !< x-coordinate
    double precision, intent(in) :: y !< y-coordinate
    character(len=*), optional, intent(in) :: name !< Name of the station, appears in output file.
    logical, optional, intent(in) :: isMoving !< Whether point is a moving station or not. Default: .false.

    logical :: isMoving_
    integer :: i, inew, isize

    character(len=40) :: name_
    name_ = ' '

    if (present(name)) then
        name_ = name
    else
        write(name_, '(a,i2.2)') trim(defaultName_), iUniq_
        iUniq_ = iUniq_ + 1
    end if

    if (present(isMoving)) then
        isMoving_ = isMoving
    else
        isMoving_ = .false.
    end if

    if (allocated(xobs)) then
       isize = size(xobs)
    else
       isize = 0
    end if

    if (isize <= numobs+nummovobs) then
        call realloc(xobs,   numobs+nummovobs+capacity_)
        call realloc(yobs,   numobs+nummovobs+capacity_)
        call realloc(xyobs,  2*(nummovobs+capacity_))
        call realloc(kobs,   numobs+nummovobs+capacity_)
        call realloc(namobs, numobs+nummovobs+capacity_)
        call realloc(smxobs, numobs+nummovobs+capacity_)
        call realloc(cmxobs, numobs+nummovobs+capacity_)
    end if

    ! Before adding new normal observation station:
    ! shift all moving stations (if any) one to the right in arrays.
    if (.not. isMoving_) then
        do i=numobs+nummovobs,numobs+1,-1
            xobs(i+1)   = xobs(i)
            yobs(i+1)   = yobs(i)
            kobs(i+1)   = kobs(i)
            namobs(i+1) = namobs(i)
            smxobs(i+1) = smxobs(i)
            cmxobs(i+1) = cmxobs(i)
        end do
        numobs = numobs+1
        inew   = numobs
    else
        nummovobs = nummovobs + 1
        inew      = numobs+nummovobs
    end if

    ! Add the actual station (moving or static)
    xobs(inew)   = x
    yobs(inew)   = y
    namobs(inew) = name_
    kobs(inew)   = -999   ! Cell number is set elsewhere
    smxobs(inew) = -999d0 ! max waterlevel
    cmxobs(inew) = -999d0 ! max velocity mag.

end subroutine addObservation

!> Adds a moving observation point to the existing points.
subroutine addMovingObservation(x, y, name)
    double precision, intent(in) :: x !< x-coordinate
    double precision, intent(in) :: y !< y-coordinate
    character(len=*), optional, intent(in) :: name

    if (present(name)) then
        call addObservation(x, y, name, isMoving = .true.)
    else
        call addObservation(x, y,       isMoving = .true.)
    end if

end subroutine addMovingObservation

!> Removes the observation point at indicated list position.
subroutine deleteObservation(pos)
    integer, intent(in) :: pos

    if (pos <= numobs+nummovobs) then
        xobs(pos) = dmiss
        yobs(pos) = dmiss
    end if
end subroutine deleteObservation

!> Cleans up 'deleted' observation points. All gaps in the *obs arrays are
!! filled again by shifting the remaining observation points.
subroutine purgeObservations()
    integer :: i, k, kk
    k = 0  ! Counts total nr of remaining obs (both static and moving)
    kk = 0 ! Counts total nr of remaining moving obs
    do i=1,numobs+nummovobs
        if (xobs(i) /= dmiss) then
            k = k+1
            xobs(k)   = xobs(i)
            yobs(k)   = yobs(i)
            kobs(k)   = kobs(i)
            namobs(k) = namobs(i)
            if (i <= numobs) then
                kk = k
            end if
        end if
    end do
    numobs = kk
    nummovobs = k-kk

end subroutine purgeObservations

!> Removes all observation points
subroutine deleteObservations()
    if (allocated(xobs)) then
       deallocate(xobs)
       deallocate(yobs)
       deallocate(xyobs)
       deallocate(kobs)
       deallocate(namobs)
       deallocate(smxobs)
       deallocate(cmxobs)
    end if

    allocate(xobs(capacity_))
    allocate(yobs(capacity_))
    allocate(xyobs(2*capacity_))
    allocate(kobs(capacity_))
    allocate(namobs(capacity_))
    allocate(smxobs(capacity_))
    allocate(cmxobs(capacity_))

    kobs = -999

    numobs = 0
    nummovobs = 0
    tlastupd_valobs = dmiss
    call doclose(mxls)
end subroutine deleteObservations


!> Reads observation points from file.
subroutine loadObservations(filename, jadoorladen)
    use messageHandling
    implicit none
    character(len=*), intent(in) :: filename
    integer,          intent(in) :: jadoorladen !< Append to existing observation points or not

    logical :: jawel
    integer :: mobs, n, L, L2
    double precision :: xp, yp
    character (len=256) :: rec
    character (len=40) :: nam

    inquire(file = filename, exist = jawel)
    if (jawel) then
        call oldfil(mobs,filename)

        if (jadoorladen == 0) then
            call deleteObservations()
        end if

        n=0
 20     read(mobs,'(a)',end =889) rec

        read(rec,*,err=888) xp, yp, nam

        L  = index(rec,'''')
        if (L > 0) then
            L  = L + 1  
            L2 = index(rec(L:),'''') - 2 + L
            nam = rec(L:L2)
        endif

        call addObservation(xp, yp, nam)
        n = n+1
        goto 20

889     call doclose(mobs)
    else
        call mess(LEVEL_WARN, "Observation file '"//trim(filename)//"' not found! Skipping ...")
    endif
    return
888 call readerror('reading x,y,nam but getting ',rec,mobs)
end subroutine loadObservations

subroutine saveObservations(filename)
    use m_sferic, only: jsferic
    implicit none

    character(len=*), intent(in)  :: filename

    integer :: mobs, i
    call newfil(mobs, filename)

    if ( jsferic.ne.1 ) then
       do i=1,numobs
           write(mobs, '(f12.3,f12.3,a,a,a)'), xobs(i), yobs(i), ' ''', trim(namobs(i)), ''''
       end do
    else
       do i=1,numobs
           write(mobs, '(f12.6,f12.6,a,a,a)'), xobs(i), yobs(i), ' ''', trim(namobs(i)), ''''
       end do
    end if
    call doclose(mobs)

end subroutine saveObservations
end module m_observations


!> A cross-section path is defined by a polyline.
!! On the unstructured grid it then results in a set of flow links that
!! cross the polyline (both 1D and 2D).
!! Used for cross sections, and thin dams and dykes.
module m_crspath
implicit none

!> Data type for storing the the polyline path and set of crossed flow
!! links.
type tcrspath
    integer                       :: np            !< Nr of polyline points
    integer                       :: lnx           !< Nr. of flow links that cross the crs path
    integer, allocatable          :: ln(:)         !< Flow links (size=len) (sign defines orientation)
    integer, allocatable          :: indexp(:)     !< Index of segment in xp by which each link is crossed.
                                                   !! (between xp(i) and xp(i+1))
    double precision, allocatable :: wfp(:)        !< Weightfactor of first point in crossed segment
                                                   !! as indicated in indexp (between 0 and 1).
    double precision, allocatable :: xp(:), yp(:), &
                                     zp(:)         !< Polyline points that define the crs (size=np)
    double precision, allocatable :: xk(:,:), yk(:,:) !< For plotting only (size=2,lnx).
                                                   !! for all 'lnx' flow links, store both start
                                                   !! and end point because segments will not be ordered
                                                   !! nor connected.
    integer,          allocatable :: iperm(:)      !! permutation array of crossed flow links in increasing arc length order along cross section polyline
    double precision, allocatable :: sp(:)         !! polygon arclength of flow link, dim()
    double precision, allocatable :: wfk1k2(:)     !! per-flowlink interpolation weight factor between k1 (1) and k2 (0), dim(lnx)
end type tcrspath

contains

!> Allocates the internal data for one crs path.
!! Based on polyline length and flow links upper limit.
subroutine increaseCrossSectionPath(path, maxnp, maxlnx)
use m_alloc
    type(tcrspath), intent(inout) :: path   !< The path structure of a cross section.
    integer,        intent(in)    :: maxnp  !< Max number of polyline points. If 0, nothing is done.
    integer,        intent(in)    :: maxlnx !< Max number of crossed flow links. If 0, nothing is done.

    integer :: m, mcur

    mcur = 0
    if (allocated(path%xp)) then
        mcur = size(path%xp)
    end if

    if (maxnp > 0 .and. maxnp > mcur) then
        m = max(2, int(1.5d0*maxnp))
        call realloc(path%xp, m)
        call realloc(path%yp, m)
        call realloc(path%zp, m)
    end if

    mcur = 0
    if (allocated(path%ln)) then
        mcur = size(path%ln)
    end if

    if (maxlnx > 0 .and. maxlnx > mcur) then
        m = max(5, int(1.5d0*maxlnx))
        call realloc(path%ln,     m)


! GD: memory problems with realloc
     if (allocated(path%xk)) then
        call realloc(path%xk, (/2,m/))
        call realloc(path%yk, (/2,m/))
     else   
        allocate(path%xk(2,m))
        allocate(path%yk(2,m))
     end if

        !if(allocated(path%xk)) deallocate(path%xk)
        !allocate(path%xk(2,m))

        !if(allocated(path%yk)) deallocate(path%yk)
        !allocate(path%yk(2,m))



        call realloc(path%indexp, m)
        call realloc(path%wfp,    m)
        call realloc(path%wfk1k2, m)
        call realloc(path%sp,     m)
        call realloc(path%iperm,  m)
    end if
end subroutine increaseCrossSectionPath


!> Deallocates the internal data for one crs path.
subroutine deallocCrossSectionPath(path)
    type(tcrspath), intent(inout) :: path !< The path structure of a cross section

    if (allocated(path%xp)) then
        deallocate(path%xp)
        deallocate(path%yp)
        deallocate(path%zp)
    end if
    if (allocated(path%ln)) then
        deallocate(path%ln)
        deallocate(path%indexp)
        deallocate(path%wfp)
    end if
    if (allocated(path%xk)) then
        deallocate(path%xk, path%yk)
    end if
    if (allocated(path%sp)) then
        deallocate(path%sp)
    end if
    if (allocated(path%wfk1k2)) then
        deallocate(path%wfk1k2)
    end if
    if (allocated(path%iperm)) then
        deallocate(path%iperm)
    end if
end subroutine deallocCrossSectionPath


!> Sets the cross section definition path to specified polyline coordinates.
subroutine setCrossSectionPathPolyline(path, xp, yp, zp)
    type(tcrspath),   intent(inout) :: path         !< The crs path to be updated.
    double precision, intent(in)    :: xp(:), yp(:) !< Polyline coordinates to define the crs path.
    double precision, optional, intent(in) :: zp(:) !< Optional z-values at xp/yp coordinates.

    integer :: i, n

    n = size(xp)
    if (n <= 0) return

    call increaseCrossSectionPath(path, n, 0)
    do i=1,n
        path%xp(i) = xp(i)
        path%yp(i) = yp(i)
    end do

    if (present(zp)) then
        do i=1,n
            path%zp(i) = zp(i)
        end do
    end if

    path%np = n
end subroutine setCrossSectionPathPolyline


!> Copies a crspath into another, allocating memory for all points and links.
! AvD: TODO: repeated copying will increase the xp and ln arrays (because of grow factor)
subroutine copyCrossSectionPath(pfrom, pto)
    type(tcrspath), intent(in)    :: pfrom
    type(tcrspath), intent(inout) :: pto

    !integer :: maxnp, maxlnx
    !
    !if (allocated(pfrom%xp)) then
    !   maxnp  = size(pfrom%xp)
    !else
    !   maxnp = 0
    !end if
    !
    !if (allocated(pfrom%ln)) then
    !   maxlnx = size(pfrom%ln)
    !else
    !   maxlnx = 0
    !end if
    !
    !call increaseCrossSectionPath(pto, maxnp, maxlnx)

    ! Structures may directly be copied, including their allocatable components (F2003)
    pto = pfrom
end subroutine copyCrossSectionPath


!> Increases the size of an *array* of crspath elements.
!! All existing elements (up to #numcur) are copied.
subroutine increaseCRSPaths(paths, numnew, numcur)
    type(tcrspath), allocatable, intent(inout) :: paths(:)
    integer,                     intent(inout) :: numnew !< Desired new size (may turn out larger).
    integer,                     intent(in)    :: numcur !< Current nr of paths in array
                                                         !! (will be copied, actual array size may be larger)


    type(tcrspath), allocatable :: pathst(:)
    integer :: i, numcurmax

    if (allocated(paths)) then
        numcurmax = size(paths)
        if (numnew < numcurmax) then
            return
        end if
    else
        numcurmax = 0
    end if
    numnew    = max(numnew, int(numcurmax*1.2))

    ! Allocate temp array of cross section paths.
    allocate(pathst(numcur))

    ! Fill temp paths and deallocate each original cross section path.
    do i=1,numcurmax
        if (i <= numcur) then
            call copyCrossSectionPath(paths(i), pathst(i))
        end if
        call deallocCrossSectionPath(paths(i))
    end do
    ! Deallocate original crspath array
    if (allocated(paths)) then
        deallocate(paths)
    end if

    ! Re-allocate original crspath array at bigger size and fill it.
    allocate(paths(numnew))
    do i=1,numcur
        call copyCrossSectionPath(pathst(i), paths(i))
        call deallocCrossSectionPath(pathst(i))
    end do
    deallocate(pathst)
end subroutine increaseCRSPaths


!> Check for crossing of a (flow) link by a crs path.
!! When crossed, the link info (its number and coordinates) are stored
!! in the path structure. Any existing link info is preserved!
!! This routine can be used with 'network geometry' (e.g. for thin dams)
!! and 'flow geometry' (e.g. for cross sections and fixed weirs).
subroutine crspath_on_singlelink(path, linknr, xk3, yk3, xk4, yk4, xza, yza, xzb, yzb)
   
   use geometry_module, only: crossinbox
   use m_sferic, only: jsferic
   use m_missing, only : dmiss
   implicit none
   
   type(tcrspath),   intent(inout) :: path   !< Path that is checked for link crossing, will be updated with link info.
    integer,          intent(in)    :: linknr !< Number of link that is being checked, will be stored in path%ln
    double precision, intent(in)    :: xk3, yk3, xk4, yk4 !< Net node coordinates of this link (or fictious coords for a 1D link)
    double precision, intent(in)    :: xza, yza, xzb, yzb !< cell circum. coordinates of this link.

    integer :: ip, jacros
    double precision :: SL, SM, XCR, YCR, CRP

!   Check whether flow link intersects with a polyline segment of this cross section path.
    do ip=1,path%np-1
        crp = 0d0
        CALL CROSSinbox(path%XP(ip), path%YP(ip), path%XP(ip+1), path%YP(ip+1), xza, yza, xzb, yzb, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
        if (jacros == 1) then
            if (SM == 1d0) then
               if (crp > 0d0) then
                  cycle
               end if
            else if (SM == 0d0) then
               if (crp < 0d0) then
                  cycle
               end if
            end if

            call increaseCrossSectionPath(path, 0, path%lnx+1)
            path%lnx = path%lnx + 1

            path%indexp(path%lnx) =  ip
            path%wfp(path%lnx)    =  1d0-SL ! SL=rel.pos on segment. Weight of left points is 1-SL
            path%wfk1k2(path%lnx) =  1d0-SM ! SM=rel.pos on flow link       of left points is 1-SM

            if (crp < 0d0) then
                path%ln(path%lnx)   =  linknr
                path%xk(1,path%lnx) = xk3
                path%yk(1,path%lnx) = yk3
                path%xk(2,path%lnx) = xk4
                path%yk(2,path%lnx) = yk4
            else
!               Flip flow link orientation, such that its flow direction is rightward through crs path polygon
                path%ln(path%lnx) = -linknr
                path%xk(1,path%lnx) = xk4
                path%yk(1,path%lnx) = yk4
                path%xk(2,path%lnx) = xk3
                path%yk(2,path%lnx) = yk3
            end if


        endif
    enddo
end subroutine crspath_on_singlelink

end module m_crspath


!> Cross sections (crs) are used to monitor summed flow data across a line
!! over time. The definition of crs is by a crspath, which is a polyline
!! with all flow links (1D and 2D, including orientation) that cross it.
!! Given a norhtward crs, the positive  transport direction is eastward.
module m_monitoring_crosssections
use m_crspath
use m_missing
implicit none

type tcrs
    character(len=64)             :: name          !< Name
    integer                       :: nval          !< Nr. of different quantities monitored
    type(tcrspath)                :: path          !< Polyline+crossed flow links that defines this cross section.
    double precision, allocatable :: sumvalcur(:)  !< Values integrated over the crs
    double precision, allocatable :: sumvalcum(:)  !< Values integrated over crs *and* time
    double precision, allocatable :: sumvalavg(:)  !< Values integrated over crs and averaged in time.
                                                   !! Size is nval: nr of monitored quantities.
end type tcrs

! Indices in sumvalcur and other arrays: postfix 'C' means cumulative/sum, 'A' means averaged.
integer                              :: IPNT_Q1C = 1           ! pointers in sumval* arrays
integer                              :: IPNT_AUC = 2           ! pointers in sumval* arrays
integer                              :: IPNT_U1A = 3           ! pointers in sumval* arrays
integer                              :: IPNT_S1A = 4           ! pointers in sumval* arrays
integer                              :: IPNT_HUA = 5           ! pointers in sumval* arrays

type (tcrs), allocatable, target     :: crs(:)
integer                              :: ncrs = 0, maxcrs = 2, maxnval = 5

integer, private                     :: iUniq_ = 1
character(len=*), parameter, private :: defaultName_ = 'Crs'
double precision                     :: tlastupd_sumval        !< Time at which the sumval* arrays were last updated.
double precision, allocatable        :: sumvalcur_tmp(:,:)     !< Store the temporary values for MPI communication of partial sums across cross sections monitoring.
double precision, allocatable        :: sumvalcumQ_mpi(:)      !< Store the time-integrated discharge in each history output interval, only used for parallel run


contains

!> Returns the index/position of a named crosssection in the global set arrays of this module.
subroutine getCrosssectionIndex(crsname, index)
   character(len=*), intent(in)  :: crsname
   integer,          intent(out) :: index !< The position of the (possibly moving) observation station in all set arrays. 0 if not present.

   integer :: i

   index = 0
   do i=1,ncrs
      if (trim(crs(i)%name) == trim(crsname)) then
         index = i
         exit
      end if
   end do
end subroutine getCrosssectionIndex

!> Allocates an array of cross sections, deallocating any existing memory.
subroutine allocCrossSections(cs, n)

use m_transport , only: NUMCONST
implicit none

    type(tcrs), allocatable, intent(inout) :: cs(:)   !< Array of cross sections
    integer,                 intent(in)    :: n       !< Desired nr of cross sections
    
    call deallocCrossSections(cs)
    allocate(cs(n))
    call ReallocCrossSectionSums(cs)                  !< needed only for old interactor
end subroutine allocCrossSections

subroutine ReallocCrosssectionSums(cs)
use m_transport , only: NUMCONST
use m_alloc
use m_sediment, only: jased, stmpar
implicit none
    type(tcrs), allocatable, intent(inout) :: cs(:)   !< Array of cross sections
    integer :: i
    integer                                :: maxnval
   
    maxnval = 5 + NUMCONST
        
    if( jased == 4 .and. stmpar%lsedtot > 0 ) then
       maxnval = maxnval + 1
       if( stmpar%lsedsus > 0 ) then
          maxnval = maxnval + 1
       endif
    endif
    
    
    do i=1,size(cs)
        call realloc(cs(i)%sumvalcur, maxnval, fill=0.0d0, keepExisting=.True.)
        call realloc(cs(i)%sumvalcum, maxnval, fill=0.0d0, keepExisting=.True.)
        call realloc(cs(i)%sumvalavg, maxnval, fill=0.0d0, keepExisting=.True.)
    end do
end subroutine ReallocCrossSectionSums

!> Deallocates an array of crs
subroutine deallocCrossSections(cs)
    type(tcrs), allocatable, intent(inout) :: cs(:)

    integer :: i, n

    if (.not. allocated(cs)) return

    n = size(cs)
    do i=1,n
        call deallocCrossSectionPath(cs(i)%path)
        if (allocated(cs(i)%sumvalcur)) then
            deallocate(cs(i)%sumvalcur)
            deallocate(cs(i)%sumvalcum)
            deallocate(cs(i)%sumvalavg)
        end if
    end do
    deallocate(cs)
end subroutine deallocCrossSections


!> Copies array of crs into another array of crs.
subroutine copyCrossSections(rfrom, rto)
use m_alloc
    type(tcrs), intent(inout) :: rfrom(:)
    type(tcrs), intent(inout) :: rto(:)

    integer :: i, n

    n = size(rfrom)
    if (n > size(rto) .or. n == 0) return

    do i=1,n
        !maxnp  = size(rfrom(i)%path%xp)
        !maxlnx = size(rfrom(i)%path%ln)
        !call increaseCrossSectionPath(rto(i)%path, maxnp, maxlnx)
        rto(i) = rfrom(i)
    end do
end subroutine copyCrossSections


!> Increases memory for crs
subroutine increaseCrossSections(n)
    integer, intent(in) :: n !< Desired number of cross sections.

    type(tcrs), allocatable :: crst(:) ! Temp storage
    integer                 :: jacopy
    
    jacopy = 0

    if (n < maxcrs .and. allocated(crs)) then
        return
    end if

    call allocCrossSections(crst, maxcrs)

    if (n > maxcrs) then
        maxcrs    = max(maxcrs, int(1.2*n))
    end if

    if (allocated(crs)) then
       call copyCrossSections(crs, crst)
    end if
    call allocCrossSections(crs, maxcrs)
    call copyCrossSections(crst, crs)

    call deallocCrossSections(crst)

end subroutine increaseCrossSections


!> Starts a new cross section in the active array of crs, increasing memory when necessary.
subroutine addCrossSection(name, xp, yp)
    character(len=*), intent(in) :: name
    double precision, intent(in) :: xp(:), yp(:)

    integer :: m
    character(len=1) :: cdigits

    call increaseCrossSections(ncrs+1)

    ncrs           = ncrs + 1
    call setCrossSectionPathPolyline(crs(ncrs)%path, xp, yp)
    crs(ncrs)%path%lnx  = 0

    ! Set name (or generate one)
    m = len_trim(name)
    if (m > 0) then
        m = min(len(crs(ncrs)%name), len(name))
        crs(ncrs)%name = ' '
        crs(ncrs)%name(1:m) = name(1:m)
    else ! No name given, generate one.
        write(cdigits, '(i1)') max(2, int(floor(log10(dble(iUniq_))+1)))
        write(crs(ncrs)%name, '(a,i'//cdigits//'.'//cdigits//')'), trim(defaultName_), iUniq_
        iUniq_ = iUniq_ + 1
    end if
end subroutine addCrossSection


!> Deletes all cross sections from crs.
!! Does not free up memory, use deallocCrossSections for that.
subroutine delCrossSections()
    ncrs = 0
    iUniq_ = 1

    if (allocated(sumvalcur_tmp)) then
       deallocate(sumvalcur_tmp)
    end if
    tlastupd_sumval = dmiss

    ! Do not reset crs data, just let it be overwritten later.
end subroutine delCrossSections


!> Converts a set of polylines into cross sections
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_crosssections(xpl, ypl, npl, names)
    use m_missing

    double precision, intent(in) :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in) :: npl            !< Total number of polyline points
    character(len=*), optional, intent(in) :: names(:) ! Optional names for cross sections

    integer :: i, i1, i2, ic, numnam
    character(len=64) :: name

    if (present(names)) then
        numnam = size(names)
    else
        numnam = 0
    end if

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    ic = 0 ! Nr of polylines found so far
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                ! 1: Special name for this CRS or not?
                ic = ic + 1
                if (ic < numnam) then
                    name = names(ic)
                else
                    name = ' '
                end if

                ! 2: add the current polyline as a new crs.
                call addCrossSection(name, xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_crosssections


end module m_monitoring_crosssections

!> Module for maintaining (time-integral) statistics on flow quantities.
!! NOTE: could be the successor of Fourier analysis. Just maintain some first max/avg quantities for now.
module m_integralstats

integer            :: is_numndvals !< Number of variables on flow nodes for which statistics are recorded.
integer, parameter :: IDX_TAUS = 1 !< Index for bed shear stress
integer, parameter :: IDX_UCM  = 2 !< Index for avg cell center velocity magnitude
integer, parameter :: IDX_HS   = 3 !< Index for avg water depth

double precision, allocatable, target :: is_sumvalsnd(:,:) !< [-] Integral values on flow nodes. {"location": "face", "shape": ["is_numndvals", "ndx"]}
double precision, allocatable, target :: is_maxvalsnd(:,:) !< [-] Integral values on flow nodes. {"location": "face", "shape": ["is_numndvals", "ndx"]}

character(len=1024), allocatable, target :: is_valnamesnd(:) !NOBMI [-] Names of the variables for which statistics are maintained {"shape": ["is_numndvals"]}
double precision, target :: is_dtint !< [s] total time interval since last statistics reset.  {"rank": 0}

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_integralstats() instead.
subroutine default_integralstats()

   is_numndvals = 0

   ! Remaining of variables is handled in reset_integralstats()
   call reset_integralstats()
end subroutine default_integralstats

!> Resets only integralstats variables intended for a restart of flow simulation.
!! Upon loading of new model/MDU, call default_integralstats() instead.
subroutine reset_integralstats()
! node related
    is_sumvalsnd(1:is_numndvals,:) = 0d0
    is_maxvalsnd(1:is_numndvals,:) = -huge(1d0)
    is_valnamesnd(:) = ''
    is_valnamesnd(1) = 'taus'
    is_valnamesnd(2) = 'ucm'
    is_valnamesnd(3) = 'hs'
    
    is_dtint = 0d0
end subroutine reset_integralstats


!> Update the (time-)integral statistics for all flow nodes, typically after each time step.
subroutine update_integralstats()
   use m_flowtimes
   use m_flow
   use m_flowgeom

   integer :: k

   if (is_numndvals <= 0) then
      return
   end if

   call gettaus(1)

   do k=1,ndxi
      is_sumvalsnd(IDX_TAUS, k) =     is_sumvalsnd(IDX_TAUS, k) + dts * taus(k)
      is_sumvalsnd(IDX_UCM,  k) =     is_sumvalsnd(IDX_UCM,  k) + dts * sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k))
      is_sumvalsnd(IDX_HS, k)   =     is_sumvalsnd(IDX_HS,   k) + dts * hs(k)

      is_maxvalsnd(IDX_TAUS, k) = max(is_maxvalsnd(IDX_TAUS, k), taus(k))
      is_maxvalsnd(IDX_UCM,  k) = max(is_maxvalsnd(IDX_UCM,  k), sqrt(ucx(k)*ucx(k) + ucy(k)*ucy(k)))
      is_maxvalsnd(IDX_HS, k)   = max(is_maxvalsnd(IDX_HS,   k), hs(k))
   end do

   is_dtint = is_dtint + dts

end subroutine update_integralstats

end module m_integralstats
   

!> Contains the global data for all thin dams.
!! thd is the array of cross section paths.
module m_thindams
    use m_crspath
    implicit none

    type (tcrspath), allocatable    :: thd(:)
    integer                         :: nthd = 0

contains

!> Increases memory for thin dams
subroutine increaseThinDams(n)
    integer, intent(inout) :: n !< Desired number of thin dams

    call increaseCRSPaths(thd, n, nthd)
end subroutine increaseThinDams


!> Converts a set of polylines into thin dams.
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_thindams(xpl, ypl, npl)
    use m_missing

    double precision, intent(in) :: xpl(:), ypl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in) :: npl            !< Total number of polyline points

    integer :: i, i1, i2, maxthd

    nthd = 0

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                maxthd = nthd+1
                call increaseThinDams(maxthd)
                nthd = nthd+1
                call setCrossSectionPathPolyline(thd(nthd), xpl(i1:i2), ypl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_thindams


!> Deletes all thin dams from thd.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
subroutine delThinDams()
    nthd = 0
    ! Do not reset thd data, just let it be overwritten later.
end subroutine delThinDams

end module m_thindams


!> Contains the global data for all fixed weirs.
!! fxw is the array of cross section paths.
module m_fixedweirs
    use m_crspath
    implicit none

    type (tcrspath), allocatable    :: fxw(:)

    integer                         :: nfxw = 0
    integer, allocatable            :: lnfxw(:)              ! Links with fixed weirs (dim=nfxw)
    integer, allocatable            :: nfxwL(:)              ! fixed weirs on links   (dim=Lnx) 
    double precision, allocatable   :: csfxw(:)              ! fixed weir direction 
    double precision, allocatable   :: snfxw(:)              ! fixed weir direction
    double precision, allocatable   :: crestlxw(:)           ! crest length of a weir
    double precision, allocatable   :: shlxw(:)              ! sill height left of a weir
    double precision, allocatable   :: shrxw(:)              ! sill height right of a weir
    double precision, allocatable   :: taludlxw(:)           ! talud left of a weir
    double precision, allocatable   :: taludrxw(:)           ! talud right of a weir
    double precision, allocatable   :: vegxw(:)              ! vegetation code on a weir
    double precision, allocatable   :: weirdte(:)            ! loss coeff

    double precision                :: sillheightmin    = 0.5d0 ! waqua dams with both sillheights > sillheightmin go to fixedweirs.pli
                                                                ! the rest goes to
contains

!> Increases memory for fixed weirs
subroutine increaseFixedWeirs(n)
    integer, intent(inout) :: n !< Desired number of fixed weirs

    call increaseCRSPaths(fxw, n, nfxw)
end subroutine increaseFixedWeirs


!> Converts a set of polylines into fixed weirs.
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
subroutine pol_to_fixedweirs(xpl, ypl, zpl, npl)
    use m_missing

    double precision, intent(in) :: xpl(:), ypl(:), zpl(:) !< Long array with one or more polylines, separated by dmiss
    integer,          intent(in) :: npl            !< Total number of polyline points

    integer :: i, i1, i2, maxfxw

    nfxw = 0

    i1 = 1 ! First possible start index
    i2 = 0 ! No end index found yet.
    do i = 1,npl
        if (xpl(i) == dmiss .or. i == npl) then
            if (i == npl .and. xpl(i) /= dmiss) then
                i2 = i ! Last polyline, no dmiss separator, so also include last point #npl.
            end if
            if (i1 <= i2) then
                maxfxw = nfxw+1
                call increaseFixedWeirs(maxfxw)
                nfxw = nfxw+1
                call setCrossSectionPathPolyline(fxw(nfxw), xpl(i1:i2), ypl(i1:i2), zpl(i1:i2))
            end if
            i1 = i+1
            cycle
        else
            i2 = i ! Advance end point by one.
        end if
    end do
end subroutine pol_to_fixedweirs


!> Deletes all fixed weirs from fxw.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
subroutine delFixedWeirs()
    nfxw = 0
    ! Do not reset fxw data, just let it be overwritten later.
end subroutine delFixedWeirs

end module m_fixedweirs
