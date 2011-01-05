!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!> Utility routines for memory (re)allocation.
module m_alloc
private 

public realloc

! $Id: malloc.f90 14207 2010-12-21 17:25:13Z mourits $
! TODO: Handle nondefault kinds properly? [AvD]

!> Reallocates memory for an existing array. Arrays of most intrinsic
!! data types up to rank 4 are accepted and they may still be unallocated.
!! realloc is mainly intended for increasing array sizes, but it may also
!! be used for decreasing them.
!!
!! Where the old and new dimensions overlap, the original array data
!! is preserved (i.e., for a larger upperbound, all data is preserved)
!! This can be switched off by passing the optional argument
!! keepExisting=.false.
!!
!! An optional fill value may be specified to set the non-overlapping
!! elements. For example: call realloc(x, newmax, stat=istat, fill=-999d0)
!! The original array elements are NOT overwritten by fill, unless
!! keepExisting=.false.
!!
!! Example usage:\code
!!   integer, allocatable :: iarr(:), itens(:,:,:)
!!   call realloc(iarr, 100)
!!   call realloc(iarr, 1000, fill = -1, keepExisting=.false.)
!!   allocate(itens(10,20,30))
!!   call realloc(itens, (/ 100, 200, 300 /), fill = 0)
!! \endcode
!! 
!! \param[in,out] arr Array (up to rank 4) to be reallocated.
!! \param[in]     uindex Desired new size (upper index) for array, scalar
!!      when arr has rank 1, or rank 1 array with size ra when arr
!!      has rank ra>1.
!! \param[in]     lindex (optional) Lower index for new array, defaults
!!      to lindex(1:ra)==1.
!! \param[out]   stat (optional) Result status of allocate command for the
!!      array.
!! \param[in]     fill (optional) Scalar value to fill any empty spots in
!!      the new array. Empty spots occur when the new size is larger than
!!      the old size, or when keepExisting==.false.
!! \param[in]     keepExisting (optional) Whether to preserve the original
!!      data in arr (defaults to .true.). When set to .false. and the
!!      parameter fill is not present, the resulting data is unspecified.
interface realloc
   module procedure reallocInt
   module procedure reallocInt2
   module procedure reallocInt2x
   module procedure reallocInt3
   module procedure reallocInt4
   module procedure reallocCharacter
   module procedure reallocCharacter2
   module procedure reallocCharacter2x
   module procedure reallocReal
   module procedure reallocReal2
   module procedure reallocReal2x
   module procedure reallocReal3
   module procedure reallocReal3x
   module procedure reallocReal4
   module procedure reallocDouble
   module procedure reallocDouble2
   module procedure reallocDouble2x
   module procedure reallocDouble3
   module procedure reallocDouble4
   module procedure reallocLogical
end interface

contains

subroutine reallocReal(arr, uindex, lindex, stat, fill, keepExisting)
   implicit none
   real, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   logical, intent(in), optional                :: keepExisting

   real, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_
   integer        :: localErr = 0
   logical        :: doalloc, docopy
   logical        :: ident

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr, 1)
      ident = uind == uindex
      lind = lbound(arr, 1)
      ident = ident .and. (lind == lindex_)
      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if (.not.ident) then
         if (docopy) then
            allocate (b(mlind:muind))
            b(mlind:muind) = arr(mlind:muind)
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif
   if (doalloc) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
       arr = fill
   endif

   if (allocated (b)) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocDouble(arr, uindex, lindex, stat, fill, keepExisting)
   implicit none
   double precision, allocatable, intent(inout) :: arr(:)
   integer, intent(in)                          :: uindex
   integer, optional                            :: lindex
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional       :: fill
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_
   integer        :: localErr = 0
   logical        :: doalloc, docopy
   logical        :: ident

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr, 1)
      ident = uind == uindex
      lind = lbound(arr, 1)
      ident = ident .and. (lind == lindex_)
      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if (.not.ident) then
         if (docopy) then
            allocate (b(mlind:muind))
            b(mlind:muind) = arr(mlind:muind)
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif
   if (doalloc) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine


subroutine reallocCharacter(arr, uindex, lindex, stat, fill, keepExisting)
   implicit none
   character(len=*), allocatable, intent(inout) :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   character(len=1), intent(in), optional       :: fill
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr(1))), allocatable      :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_
   integer        :: localErr = 0
   logical        :: doalloc, docopy
   logical        :: ident

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr, 1)
      ident = uind == uindex
      lind = lbound(arr, 1)
      ident = ident .and. (lind == lindex_)
      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if (.not.ident) then
         if (docopy) then
            allocate (b(mlind:muind))
            b(mlind:muind) = arr(mlind:muind)
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocLogical(arr, uindex, lindex, stat, fill, keepExisting)
   implicit none
   logical, allocatable, intent(inout)          :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                :: fill
   logical, intent(in), optional                :: keepExisting

   logical, allocatable                         :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_
   integer        :: localErr = 0
   logical        :: doalloc, docopy
   logical        :: ident

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr, 1)
      ident = uind == uindex
      lind = lbound(arr, 1)
      ident = ident .and. (lind == lindex_)
      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if (.not.ident) then
         if (docopy) then
            allocate (b(mlind:muind))
            b(mlind:muind) = arr(mlind:muind)
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocInt(arr, uindex, lindex, stat, fill, keepExisting)
   implicit none
   integer, allocatable, intent(inout)          :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                :: fill
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                         :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_
   integer        :: localErr = 0
   logical        :: doalloc, docopy
   logical        :: ident

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr, 1)
      ident = uind == uindex
      lind = lbound(arr, 1)
      ident = ident .and. (lind == lindex_)
      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if (.not.ident) then
         if (docopy) then
            allocate (b(mlind:muind))
            b(mlind:muind) = arr(mlind:muind)
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocReal2x(arr, u1, u2, l1, l2, stat, keepExisting)
   real, allocatable, intent(inout)             :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat
   logical, intent(in), optional                :: keepExisting

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocReal2(arr, uindex, lindex, stat = stat)
   else
      call reallocReal2(arr, uindex, stat = stat)
   endif
end subroutine reallocReal2x

subroutine reallocReal2(arr, uindex, lindex, stat, fill, keepExisting)
   real, allocatable, intent(inout) :: arr(:, :)
   integer, intent(in)              :: uindex(2)
   integer, intent(in), optional    :: lindex(2)
   integer, intent(out), optional   :: stat
   real, intent(in), optional       :: fill
   logical, intent(in), optional    :: keepExisting

   real, allocatable                :: b(:,:)

   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   ident = .true.

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2) = arr(i1, i2)
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2) = b(i1, i2)
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocDouble2x(arr, u1, u2, l1, l2, stat)
   double precision, allocatable, intent(inout)             :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocDouble2(arr, uindex, lindex, stat = stat)
   else
      call reallocDouble2(arr, uindex, stat = stat)
   endif
end subroutine reallocDouble2x

subroutine reallocDouble2(arr, uindex, lindex, stat, fill, keepExisting)
   double precision, allocatable, intent(inout) :: arr(:, :)
   integer, intent(in)                    :: uindex(2)
   integer, intent(in), optional          :: lindex(2)
   integer, intent(out), optional         :: stat
   double precision, intent(in), optional :: fill
   logical, intent(in), optional          :: keepExisting

   double precision, allocatable    :: b(:,:)

   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   ident = .true.

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2) = arr(i1, i2)
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
         arr(i1, i2) = b(i1, i2)
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocInt2x(arr, u1, u2, l1, l2, stat)
   integer, allocatable, intent(inout)          :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocInt2(arr, uindex, lindex, stat = stat)
   else
      call reallocInt2(arr, uindex, stat = stat)
   endif
end subroutine reallocInt2x

subroutine reallocInt2(arr, uindex, lindex, stat, fill, keepExisting)
   integer, allocatable, intent(inout)          :: arr(:, :)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                :: fill
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                         :: b(:,:)

   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   ident = .true.

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2) = arr(i1, i2)
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2) = b(i1, i2)
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocCharacter2x(arr, u1, u2, l1, l2, stat)
   character(len=*), allocatable, intent(inout) :: arr(:, :)
   integer                                      :: u1, u2
   integer, optional                            :: l1, l2
   integer                                      :: uindex(2)
   integer                                      :: lindex(2)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2/)
   if (present(l1)) then
      lindex = (/l1, l2/)
      call reallocCharacter2(arr, uindex, lindex, stat = stat)
   else
      call reallocCharacter2(arr, uindex, stat = stat)
   endif
end subroutine reallocCharacter2x

subroutine reallocCharacter2(arr, uindex, lindex, stat, fill, keepExisting)
   character(len=*), allocatable, intent(inout) :: arr(:, :)
   integer, intent(in)                          :: uindex(2)
   integer, intent(in), optional                :: lindex(2)
   integer, intent(out), optional               :: stat
   character(len=1), intent(in), optional       :: fill
   logical, intent(in), optional                :: keepExisting

   character(len=len(arr(1,1))), allocatable    :: b(:,:)

   integer        :: uind(2), lind(2), muind(2), mlind(2), lindex_(2)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1 /)
   endif

   ident = .true.

   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2)))
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2) = arr(i1, i2)
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2) = b(i1, i2)
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocInt3(arr, uindex, lindex, stat, fill, keepExisting)
   integer, allocatable, intent(inout)          :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                :: fill
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                         :: b(:,:,:)

   integer        :: lind(3), uind(3), muind(3), mlind(3), lindex_(3)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2, i3

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   ident = .true.
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3=mlind(3),muind(3)
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2, i3) = arr(i1, i2, i3)
            end do
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2), lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i3=mlind(3),muind(3)
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2, i3) = b(i1, i2, i3)
      end do
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocReal3x(arr, u1, u2, u3, l1, l2, l3, stat)
   real, allocatable, intent(inout)             :: arr(:, :, :)
   integer                                      :: u1, u2, u3
   integer, optional                            :: l1, l2, l3
   integer                                      :: uindex(3)
   integer                                      :: lindex(3)
   integer, intent(out), optional               :: stat

   uindex = (/u1, u2, u3/)
   if (present(l1)) then
      lindex = (/l1, l2, l3/)
      call reallocReal3(arr, uindex, lindex, stat = stat)
   else
      call reallocReal3(arr, uindex, stat = stat)
   endif
end subroutine reallocReal3x

subroutine reallocReal3(arr, uindex, lindex, stat, fill, keepExisting)
   real, allocatable, intent(inout) :: arr(:,:,:)
   integer, intent(in)              :: uindex(3)
   integer, intent(in), optional    :: lindex(3)
   integer, intent(out), optional   :: stat
   real, intent(in), optional       :: fill
   logical, intent(in), optional    :: keepExisting

   real, allocatable                :: b(:,:,:)

   integer        :: lind(3), uind(3), muind(3), mlind(3), lindex_(3)
   integer        :: localErr = 0
   integer        :: i
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2, i3

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   ident = .true.
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3=mlind(3),muind(3)
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2, i3) = arr(i1, i2, i3)
            end do
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2), lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i3=mlind(3),muind(3)
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2, i3) = b(i1, i2, i3)
      end do
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocDouble3(arr, uindex, lindex, stat, fill, keepExisting)
   double precision, allocatable, intent(inout) :: arr(:,:,:)
   integer, intent(in)                          :: uindex(3)
   integer, intent(in), optional                :: lindex(3)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional       :: fill
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                :: b(:,:,:)

   integer        :: lind(3), uind(3), muind(3), mlind(3), lindex_(3)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2, i3

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1 /)
   endif

   ident = .true.
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3)))
            do i3=mlind(3),muind(3)
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2, i3) = arr(i1, i2, i3)
            end do
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2), lindex_(3):uindex(3)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i3=mlind(3),muind(3)
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2, i3) = b(i1, i2, i3)
      end do
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocInt4(arr, uindex, lindex, stat, fill, keepExisting)
   integer, allocatable, intent(inout)          :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                :: fill
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                         :: b(:,:,:,:)

   integer        :: lind(4), uind(4), muind(4), mlind(4), lindex_(4)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2, i3, i4

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   ident = .true.
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4=mlind(4),muind(4)
            do i3=mlind(3),muind(3)
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2, i3, i4) = arr(i1, i2, i3, i4)
            end do
            end do
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2), &
                   lindex_(3):uindex(3), lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i4=mlind(4),muind(4)
      do i3=mlind(3),muind(3)
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2, i3, i4) = b(i1, i2, i3, i4)
      end do
      end do
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocReal4(arr, uindex, lindex, stat, fill, keepExisting)
   real   , allocatable, intent(inout)          :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   real, intent(in), optional                   :: fill
   logical, intent(in), optional                :: keepExisting

   real, allocatable                            :: b(:,:,:,:)
   integer        :: lind(4), uind(4), muind(4), mlind(4), lindex_(4)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2, i3, i4

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   ident = .true.
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4=mlind(4),muind(4)
            do i3=mlind(3),muind(3)
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2, i3, i4) = arr(i1, i2, i3, i4)
            end do
            end do
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2), &
                   lindex_(3):uindex(3), lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i4=mlind(4),muind(4)
      do i3=mlind(3),muind(3)
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2, i3, i4) = b(i1, i2, i3, i4)
      end do
      end do
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

subroutine reallocDouble4(arr, uindex, lindex, stat, fill, keepExisting)
   double precision, allocatable, intent(inout) :: arr(:,:,:,:)
   integer, intent(in)                          :: uindex(4)
   integer, intent(in), optional                :: lindex(4)
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional       :: fill
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                :: b(:,:,:,:)
   integer        :: lind(4), uind(4), muind(4), mlind(4), lindex_(4)
   integer        :: localErr = 0
   logical        :: ident, doalloc, docopy
   integer        :: i1, i2, i3, i4

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = (/ 1, 1, 1, 1 /)
   endif

   ident = .true.
   doalloc = .false.
   docopy  = .true.
   if (present(keepExisting)) then
      docopy = keepExisting
   end if

   if (allocated(arr)) then
      uind = ubound(arr)
      lind = lbound(arr)
      ident = all(uindex == uind) .and. all(lindex_ == lind)

      mlind = max(lind, lindex_)
      muind = min(uind, uindex)
      if ( .not. ident )then
         if (docopy) then
            allocate (b(mlind(1):muind(1),mlind(2):muind(2),mlind(3):muind(3),mlind(4):muind(4)))
            do i4=mlind(4),muind(4)
            do i3=mlind(3),muind(3)
            do i2=mlind(2),muind(2)
            do i1=mlind(1),muind(1)
                b(i1, i2, i3, i4) = arr(i1, i2, i3, i4)
            end do
            end do
            end do
            end do
         end if
         deallocate(arr)
         doalloc = .true.
      endif
   else
      doalloc = .true.
   endif

   if (doalloc) then
      allocate(arr(lindex_(1):uindex(1), lindex_(2):uindex(2), &
                   lindex_(3):uindex(3), lindex_(4):uindex(4)), stat = localErr)
   endif
   if (present(fill) .and. (doalloc .or. .not. docopy)) then
      arr = fill
   endif
   if (allocated (b)) then
      do i4=mlind(4),muind(4)
      do i3=mlind(3),muind(3)
      do i2=mlind(2),muind(2)
      do i1=mlind(1),muind(1)
          arr(i1, i2, i3, i4) = b(i1, i2, i3, i4)
      end do
      end do
      end do
      end do
      deallocate (b)
   endif
   if (present(stat)) stat = localErr
end subroutine

end module m_alloc
