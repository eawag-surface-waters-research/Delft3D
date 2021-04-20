!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2021.
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
! $Id$
! $HeadURL$
!> Manholes connect 1D networks with 2D/3D grids or other 1D networks.
!!
!! A manhole has an x,y-location, which determines the unique 2D flow cell
!! that it lies in. A manhole can also be 'pressurized' (closed lid), then
!! no 2D cell is associated, only 1D netlinks can be connected then.
module m_manholes
use properties
use unstruc_messages
implicit none

integer, parameter :: MANHOLE_CLOSED          = 1
integer, parameter :: MANHOLE_RESERVOIR       = 2
integer, parameter :: MANHOLE_OPEN_MOMENTUM   = 3
integer, parameter :: MANHOLE_OPEN_NOMOMENTUM = 4

type manhole_t
    double precision :: x      !< exact x-coordinate
    double precision :: y      !< exact y-coordinate
    character(len=40):: name   !< name of manhole

    integer          :: itype  !< type of manhole (see module parameters)

    ! More attributes...
end type manhole_t

type(manhole_t), allocatable :: manholes(:)  !< Global array of manholes
integer                      :: nummh        !< Current number of manholes in array.

contains
!
!------------------------------------------------------------------------------

subroutine init_manholes()
    nummh = 0
end subroutine init_manholes
!
!------------------------------------------------------------------------------


!> Adds a new manhole at the end of the global manhole list.
subroutine add_manhole(x, y, name, itype)
    double precision, intent(in) :: x, y   !< Location coordinates
    character(len=*), intent(in) :: name   !< Name of manhole
    integer,          intent(in) :: itype  !< Type of manhole (see module parameters)

    integer :: maxmh

    maxmh = size(manholes)
    if (nummh >= maxmh) then
        maxmh = ceiling(1.2*max(1,nummh))
        call realloc_manholes(manholes, maxmh, .true.)
    end if

    nummh = nummh + 1
    manholes(nummh)%x     = x
    manholes(nummh)%y     = y
    manholes(nummh)%name  = name
    manholes(nummh)%itype = itype

end subroutine add_manhole
!
!------------------------------------------------------------------------------


!> Resized an allocatable array of manholes.
subroutine realloc_manholes(mhs, n, keepExisting)
    type(manhole_t), allocatable, intent(inout) :: mhs(:)       !< Array of manholes.
    integer,                      intent(in)    :: n            !< new size
    logical,                      intent(in)    :: keepExisting !< Preserve existing manholes in array or not (set to .false. to save on copy overhead).

    type(manhole_t), allocatable :: mhb(:)
    integer :: nold, ncopy
    logical :: equalSize

    ! 1. Backup if necessary.
    if (allocated(mhs)) then
        nold = ubound(mhs,1)
        equalSize = nold == n
        if (equalSize .and. keepExisting) return ! output=input
        !
        if (keepExisting) then
            ncopy = min(n, nold)
            allocate (mhb(1:ncopy))
            mhb(1:ncopy) = mhs(1:ncopy)
            ! TODO: If manhole_t will contain allocatables/arrays itself, make allocate and copy more sophisticated.
        endif
        if (.not.equalSize) deallocate(mhs)
    endif

    ! Reallocate if necessary.
    if (.not. allocated(mhs)) then
        allocate(mhs(1:n))
    end if

    ! Put back backup if necessary.
    if (allocated(mhb)) then
        mhs(1:ncopy) = mhb(1:ncopy)
        deallocate(mhb)
    endif
end subroutine realloc_manholes
!
!------------------------------------------------------------------------------


!> Deletes all manholes and deallocates global manhole array.
subroutine delete_manholes()
    if (allocated(manholes)) then
        deallocate(manholes)
    end if

    nummh = 0
end subroutine delete_manholes
!
!------------------------------------------------------------------------------


!> Reads manholes from file.
subroutine load_manholes(filename, jadoorladen)
    character(len=*), intent(in) :: filename
    integer,          intent(in) :: jadoorladen !< Append to existing manholespoints or not

    type(tree_data), pointer :: mhs_ptr, mh_ptr
    integer :: istat, i, itype
    logical :: success
    double precision :: x, y

    call tree_create(trim(filename), mhs_ptr)
    call prop_file('ini',trim(filename),mhs_ptr,istat)
    if (istat /= 0) then
        call mess(LEVEL_ERROR, 'Manhole file '''//trim(filename)//''' not read. Code: ', istat)
        return
    else
        call mess(LEVEL_DEBUG, 'Opened manhole file : ', trim(filename) )
    endif


    do i = 1,size(mhs_ptr%child_nodes)
        mh_ptr => mhs_ptr%child_nodes(i)%node_ptr
        if (trim(tree_get_name(mh_ptr)) == 'manhole') then
            call prop_get_double  ( mh_ptr, '*', 'x',      x,     success)
            if (.not. success) then
               cycle
            end if

            call prop_get_double  ( mh_ptr, '*', 'y',      y,     success)
            if (.not. success) then
               cycle
            end if

            call prop_get_integer ( mh_ptr, '*', 'Type',   itype, success)
            if (.not. success) then
               cycle
            end if

        end if
        call add_manhole(x, y, "Manhole", itype)
    end do

end subroutine load_manholes
!
!------------------------------------------------------------------------------


end module m_manholes
