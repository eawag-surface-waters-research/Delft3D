!  Copyright (C)  Stichting Deltares, 2017-2021.
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

module runsum
   use precision
   implicit none

! ------------------------------------------------------------------------------
!   Class:      TRunSum
!   Purpose:    Keeps track of running sum/mean
!   Summary:    This class needs a buffer keeping a given number of values that are given in an update method
!               keeping track of a running mean.
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   private
   public :: TRunSum

   type TRunSum
      integer                     :: ndx = 0
      integer                     :: nvalue
      integer                     :: nstep
      real(kind=hp), allocatable, dimension (:,:) :: buffer
      real(kind=hp), allocatable, dimension (:)   :: state     ! state lives inside the instance
      real(kind=hp), pointer,     dimension (:)   :: dataPtr   ! dataPtr lives in the calling code
   contains
      procedure, pass :: init          => TRunSum_init         ! Init buffer and sum and index
      procedure, pass :: update        => TRunSum_update       ! Add a value to the list, update the sum
      procedure, pass :: update_state  => TRunSum_update_state ! Add a value to the list, update the sum
   end type TRunSum


contains


! ------------------------------------------------------------------------------
!   Method:     init
!   Purpose:    setup new buffer for running sum
!   Arguments:  newvalue
! ------------------------------------------------------------------------------
subroutine TRunSum_init(self, nx, nd, dataptr)
   class(TRunSum), intent(inout)                :: self
   integer, intent(in)                          :: nx
   integer, intent(in)                          :: nd
   real(kind=hp), dimension(:), pointer, optional  :: dataPtr

   if (allocated(self%buffer)) deallocate(self%buffer)
   allocate (self%buffer(nx,0:nd-1))
   if (allocated(self%state)) deallocate(self%state)
   allocate (self%state(nx))
   self%state(1:nx) = 0.d0
   self%ndx = 0
   self%dataPtr => null()
   if (present(dataPtr)) then
      if (associated(dataPtr)) then
         self%dataPtr => dataPtr
      end if
   end if
end subroutine TRunSum_init


! ------------------------------------------------------------------------------
!   Method:     update
!   Purpose:    add a new element to the buffer, remove an old one and update the sum
!   Arguments:  newvalue
! ------------------------------------------------------------------------------
subroutine TRunSum_update(self, newvalue)
   class(TRunSum), intent(inout)                :: self
   real(kind=hp), dimension(:), intent(in), optional, target :: newvalue

   integer :: nx, nd
   real(kind=hp), dimension(:), pointer :: pnew

   nx = size(self%state)
   nd = size(self%buffer,dim=2)
   if (present(newvalue)) then
       pnew => newvalue                  ! update value(s) explicit
   else
       pnew => self%dataPtr              ! update value(s) pointered to
   end if
   call self%update_state(pnew,nx,nd)
   self%ndx = mod(self%ndx+1,nd)
end subroutine TRunSum_update


subroutine TRunSum_update_state(self, pnew, nx, nd)
   class(TRunSum), intent(inout)        :: self
   real(kind=hp), dimension(:), pointer :: pnew
   integer, intent(in)                  :: nx
   integer, intent(in)                  :: nd

   self%state(1:nx) = self%state(1:nx) + pnew(1:nx) - self%buffer(1:nx, self%ndx)
   self%buffer(1:nx, self%ndx) = pnew(1:nx)
end subroutine TRunSum_update_state

end module runsum





module runsum_weighted
   use precision
   use runsum
   implicit none


! ------------------------------------------------------------------------------
!   Class:      TRunSumWeighted
!   Purpose:    Keeps track of running sum/mean with a weight for each time level
!   Summary:    This class needs a buffer keeping a given number of values that are given in an update method
!               keeping track of a running mean.
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   private
   public :: TRunSumWeighted

   type, extends (TRunSum) :: TRunSumWeighted
      real(kind=hp), pointer,     dimension (:)   :: weights   ! dataPtr lives in the calling code
   contains
      procedure, pass :: setweights    => TRunSumWeighted_setweights
      procedure, pass :: update_state  => TRunSumWeighted_update_state
   end type TRunSumWeighted
 

contains

subroutine TRunSumWeighted_setweights(self, pweights)
   class(TRunSumWeighted), intent(inout)    :: self
   real(kind=hp), pointer,  dimension (:)   :: pweights   ! dataPtr lives in the calling code
   self%weights => pweights
end subroutine TRunSumWeighted_setweights

subroutine TRunSumWeighted_update_state(self, pnew, nx, nd)
   class(TRunSumWeighted), intent(inout)   :: self
   real(kind=hp), dimension(:), pointer    :: pnew
   integer, intent(in)                     :: nx
   integer, intent(in)                     :: nd

   integer :: id, idm

   self%buffer(1:nx, self%ndx) = pnew(1:nx)
   self%state(1:nx) = 0.d0
   do id = 0, nd-1
      idm = mod(self%ndx - id + nd, nd)
      if (associated(self%weights)) then
         self%state(1:nx) = self%state(1:nx) + self%weights(id)*self%buffer(1:nx, idm)   ! weighted sum
      else
         self%state(1:nx) = self%state(1:nx) + self%buffer(1:nx, idm)   ! weighted sum
      end if
   end do
   self%state(1:nx) = self%state(1:nx) + pnew(1:nx) - self%buffer(1:nx, self%ndx)
end subroutine TRunSumWeighted_update_state

end module runsum_weighted


