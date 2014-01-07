!!  Copyright(C) Stichting Deltares, 2012-2014.
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

!!  Note: The "part" engine is not yet Open Source, but still under
!!  development. This package serves as a temporary dummy interface for
!!  the references in the "waq" engine to the "part" engine.

module dealloc_mod

use precision
use fileinfo , lun=>lunit

implicit none

interface dealloc
    module procedure dealloc_int_1D
    module procedure dealloc_int_2D
    module procedure dealloc_int_3D
    module procedure dealloc_int_4D
    module procedure dealloc_real_1D
    module procedure dealloc_real_2D
    module procedure dealloc_real_3D
    module procedure dealloc_real_4D
    module procedure dealloc_double_1D
    module procedure dealloc_char_1D
    module procedure dealloc_char_2D
end interface
contains

      subroutine dealloc_int_1D(arr)
      integer, dimension(:), pointer :: arr
      integer                        :: stat
      logical                        :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_1D

      subroutine dealloc_int_2D(arr)
      integer, dimension(:,:), pointer :: arr
      integer                          :: stat
      logical                          :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_2D

      subroutine dealloc_int_3D(arr)
      integer, dimension(:,:,:), pointer :: arr
      integer                            :: stat
      logical                            :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_3D

      subroutine dealloc_int_4D(arr)
      integer, dimension(:,:,:,:), pointer :: arr
      integer                              :: stat
      logical                              :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_int_4D

      subroutine dealloc_real_1D(arr)
      real(sp), dimension(:), pointer :: arr
      integer                        :: stat
      logical                        :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_1D

      subroutine dealloc_double_1D(arr)
      real(dp), dimension(:), pointer :: arr
      integer                         :: stat
      logical                         :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_double_1D

      subroutine dealloc_real_2D(arr)
      real(sp), dimension(:,:), pointer :: arr
      integer                           :: stat
      logical                           :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_2D

      subroutine dealloc_real_3D(arr)
      real(sp), dimension(:,:,:), pointer :: arr
      integer                             :: stat
      logical                             :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_3D

      subroutine dealloc_real_4D(arr)
      real(sp), dimension(:,:,:,:), pointer :: arr
      integer                               :: stat
      logical                               :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_real_4D

      subroutine dealloc_char_1D(arr)
      character (len=*), dimension(:), pointer :: arr
      integer                                  :: stat
      logical                                  :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_char_1D

      subroutine dealloc_char_2D(arr)
      character (len=*), dimension(:,:), pointer :: arr
      integer                                    :: stat
      logical                                    :: dealloc_ok
      if (associated(arr)) then
         deallocate(arr,stat=stat)
         dealloc_ok = (stat == 0)
         if (.not.dealloc_ok) call dealloc_error()
      endif
      return
      end subroutine dealloc_char_2D

      subroutine dealloc_error()
         write(lun(2),'(//a)') ' * Part Memory Error '
         write(lun(2),'(  a)') ' *   Could not deallocate memory'
         write(lun(2),'(a//)') ' *   Please contact Delft3D Support    '
         write(*     ,'(//a)') ' * Part Memory Error '
         write(*     ,'(  a)') ' *   Could not deallocate memory'
         write(*     ,'(a//)') ' *   Please contact Delft3D Support    '
         STOP ' Part aborted ! '
      end subroutine dealloc_error
end module dealloc_mod
