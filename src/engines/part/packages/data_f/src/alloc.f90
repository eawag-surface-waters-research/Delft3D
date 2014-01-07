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

module alloc_mod

      use precision
      implicit none

      integer(ip), private :: lunmem
      integer(ip), private :: lunrep
      integer(ip), private :: accu
      integer(ip), private :: number

   interface alloc
      module procedure alloc_int_1D
      module procedure alloc_int_2D
      module procedure alloc_int_3D
      module procedure alloc_int_4D
      module procedure alloc_real_1D
      module procedure alloc_real_2D
      module procedure alloc_real_3D
      module procedure alloc_real_4D
      module procedure alloc_double_1D
      module procedure alloc_char_1D
      module procedure alloc_char_2D
   end interface
   contains

      subroutine init_alloc ( lun    , lunut  )

      integer(ip), intent(in   ) :: lun
      integer(ip), intent(in   ) :: lunut

      accu   = 0
      number = 0
      lunmem = lun
      lunrep = lunut
      open  ( lunmem, file="part_memory_map.out" )
      write ( lunmem, '(/'' ====> allocated array space in 4-byte words <===='' )' )
      write ( lunmem, '( ''  nr typ       kind array name          array size''/)' )

      return
      end subroutine

      subroutine exit_alloc ( accu2 )
      integer(ip) accu2
      accu2 = accu
      write ( lunmem, '(''                                       ==========='' )' )
      write ( lunmem, 10 )      accu/4                  ,                                 &
                                accu         /1000000000, mod(accu,1000000000)/1000000,   &
                            mod(accu,1000000)/      1000, mod(accu,      1000)
      close ( lunmem )
      return
   10 format ( ' Grand total of all array space:       ',i11,' words or:',      &
                                    i3,'-GB ',i3,'-MB ',i3,'-KB ',i3,'-B.' )
      end subroutine

      subroutine alloc_int_1D(name,arr,n1)

      character(*), intent(in   )    :: name
      integer, dimension(:), pointer :: arr
      integer     , intent(in   )    :: n1
      integer                           knd
      integer                        :: stat
      integer     , pointer          :: wrk (:)
      integer                           increm

      knd    = kind(arr)
      increm = knd * n1
      if ( associated(arr) ) then
         increm = increm - knd*size(arr)
         if ( increm .le. 0 ) return
      endif
      number = number + 1
      allocate ( wrk(n1), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 ) then
         accu   = accu + increm
         if ( associated(arr) ) then
            wrk(1:size(arr)) = arr
            deallocate ( arr )
            write ( lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11,'' <== resized'')' )      &
                                    number,knd,name,increm/4
         else
            write ( lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11)' ) number,knd,name,increm/4
         endif
         arr => wrk
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_int_1D

      subroutine alloc_int_2D(name,arr,n1,n2)

      character(*), intent(in   )     :: name
      integer,                pointer :: arr   (:,:)
      integer     , intent(in   )     :: n1, n2
      integer                            knd
      integer                         :: stat
      integer,                pointer :: wrk   (:,:)
      integer                            first
      integer                            secnd
      integer                            increm

      knd    = kind(arr)
      if ( associated(arr) ) then
         increm = increm - knd*size(arr)
         if ( n1 .le. size(arr,1) .and. n2 .le. size(arr,2) ) return
         first = max(n1,size(arr,1))
         secnd = max(n2,size(arr,2))
         increm = knd*first*secnd - knd*size(arr)
      else
         first  = n1
         secnd  = n2
         increm = knd * n1 * n2
      endif
      number = number + 1
      allocate ( wrk(first,secnd), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 .and. n2 .gt. 0 ) then
         accu   = accu + increm
         if ( associated(arr) ) then
            wrk(1:size(arr,1),1:size(arr,2)) = arr(:,:)
            deallocate ( arr )
            write ( lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11,'' <== resized'')' )      &
                                                                           number,knd,name,increm/4
         else
            write ( lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11)' ) number,knd,name,increm/4
         endif
         arr => wrk
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1, n2
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_int_2D

      subroutine alloc_int_3D(name,arr,n1,n2,n3)

      character(*), intent(in   )    :: name
      integer,               pointer :: arr   (:,:,:)
      integer     , intent(in   )    :: n1, n2, n3
      integer                           knd
      integer                        :: stat

      allocate ( arr(n1,n2,n3), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 .and. n2 .gt. 0 .and. n3 .gt. 0 ) then
         number  = number + 1
         knd     = kind(arr)
         accu    = accu + n1*n2*n3*knd
         write ( lunmem, '(i4,'' integer    ('',i1,'') '',a,t40,i11)' ) number,knd,name,n1*n2*n3*knd/4
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',3i11)' )name, n1, n2, n3
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_int_3D

      subroutine alloc_int_4D(arr,n1,n2,n3,n4)
      integer, dimension(:,:,:,:), pointer :: arr
      integer                              :: stat
      integer                              :: n1 , n2 ,n3,n4
      logical                              :: alloc_ok
      allocate(arr(n1,n2,n3,n4),stat=stat)
      alloc_ok = (stat == 0)
      if (.not.alloc_ok) call alloc_error()
      return
      end subroutine alloc_int_4D

      subroutine alloc_real_1D(name,arr,n1)

      character(*), intent(in   ) :: name
      real        , pointer       :: arr  (:)
      integer     , intent(in   ) :: n1
      integer                        knd
      integer                     :: stat
      real        , pointer       :: wrk  (:)
      integer                        increm

      knd    = kind(arr)
      increm = knd * n1
      if ( associated(arr) ) then
         increm = increm - knd*size(arr)
         if ( increm .le. 0 ) return
      endif
      number = number + 1
      allocate ( wrk(n1), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 ) then
         accu   = accu + increm
         if ( associated(arr) ) then
            wrk(1:size(arr)) = arr
            deallocate ( arr )
            write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11,'' <== resized'')' )      &
                                                                           number,knd,name,increm/4
         else
            write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)' ) number,knd,name,increm/4
         endif
         arr => wrk
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_real_1D

      subroutine alloc_double_1D(name,arr,n1)

      character(*), intent(in   )    :: name
      real(dp),              pointer :: arr  (:)
      integer     , intent(in   )    :: n1
      integer                           knd
      integer                        :: stat

      allocate ( arr(n1), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 ) then
         number  = number + 1
         knd     = kind(arr)
         accu    = accu + n1*knd
         write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)' ) number,knd,name,n1*knd/4
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_double_1D

      subroutine alloc_real_2D(name,arr,n1,n2)

      character( *), intent(in   )    :: name
      real     (sp), pointer          :: arr(:,:)
      integer      , intent(in   )    :: n1, n2
      integer                            knd
      integer                         :: stat
      real     (sp), pointer          :: wrk(:,:)
      integer                            first
      integer                            secnd
      integer                            increm

      knd    = kind(arr)
      if ( associated(arr) ) then
         if ( n1 .le. size(arr,1) .and. n2 .le. size(arr,2) ) return
         first  = max(n1,size(arr,1))
         secnd  = max(n2,size(arr,2))
         increm = knd*first*secnd - knd*size(arr)
      else
         first  = n1
         secnd  = n2
         increm = knd * n1 * n2
      endif
      number = number + 1
      allocate ( wrk(first,secnd), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 .and. n2 .gt. 0 ) then
         accu   = accu + increm
         if ( associated(arr) ) then
            wrk(1:size(arr,1),1:size(arr,2)) = arr(:,:)
            deallocate ( arr )
            write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11,'' <== resized'')' )      &
                                                                           number,knd,name,increm/4
         else
            write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)' ) number,knd,name,increm/4
         endif
         arr => wrk
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1, n2
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_real_2D

      subroutine alloc_real_3D(name,arr,n1,n2,n3)

      character(*), intent(in   )     :: name
      real   ,                pointer :: arr(:,:,:)
      integer     , intent(in   )     :: n1, n2, n3
      integer                            knd
      integer                         :: stat
      real   ,                pointer :: wrk(:,:,:)
      integer                            first
      integer                            secnd
      integer                            third
      integer                            increm

      knd    = kind(arr)
      if ( associated(arr) ) then
         increm = increm - knd*size(arr)
         if ( n1 .le. size(arr,1) .and. n2 .le. size(arr,2) .and. n3 .le. size(arr,3) ) return
         first  = max(n1,size(arr,1))
         secnd  = max(n2,size(arr,2))
         third  = max(n3,size(arr,3))
         increm = knd*first*secnd*third - knd*size(arr)
      else
         first  = n1
         secnd  = n2
         third  = n3
         increm = knd * n1 * n2 * n3
      endif
      number = number + 1
      allocate ( wrk(first,secnd,third), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 .and. n2 .gt. 0 .and. n3 .gt. 0 ) then
         accu   = accu + increm
         if ( associated(arr) ) then
            wrk(1:size(arr,1),1:size(arr,2),1:size(arr,3)) = arr(:,:,:)
            deallocate ( arr )
            write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11,'' <== resized'')' )      &
                                                                           number,knd,name,increm/4
         else
            write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)' ) number,knd,name,increm/4
         endif
         arr => wrk
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1, n2
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_real_3D

      subroutine alloc_real_4D(name,arr,n1,n2,n3,n4)

      character(*), intent(in   )    :: name
      real   ,               pointer :: arr(:,:,:,:)
      integer     , intent(in   )    :: n1, n2, n3,n4
      integer                           knd
      integer                        :: stat

      allocate ( arr(n1,n2,n3,n4), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 .and. n2 .gt. 0 .and. n3 .gt. 0 .and. n4 .gt. 0 ) then
         number  = number + 1
         knd     = kind(arr)
         accu    = accu + n1*n2*n3*n4*knd
         write ( lunmem, '(i4,'' real       ('',i1,'') '',a,t40,i11)' ) number,knd,name,n1*n2*n3*n4*knd/4
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',4i11)' )name, n1, n2, n3, n4
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_real_4D

      subroutine alloc_char_1D(name,arr,n1)

      character(*), intent(in   )    :: name
      character(*),          pointer :: arr  (:)
      integer     , intent(in   )    :: n1
      integer                           knd
      integer                        :: stat

      allocate ( arr(n1), stat=stat )
      if ( stat .eq. 0 .and. n1 .gt. 0 ) then
         number  = number + 1
         knd     = len(arr)
         accu    = accu + n1*knd
         write ( lunmem, '(i4,'' character('',i3,'') '',a,t40,i11)' ) number,knd,name,n1*knd/4
      else
         write ( lunmem, '('' ERROR allocating: '',a,'' requested size: '',i11)' )name, n1*knd
         stop ' Allocation error. Inspect memory_map file.'
      endif
      return

      end subroutine alloc_char_1D

      subroutine alloc_char_2D(arr,n1,n2)
      character (len=*), dimension(:,:), pointer :: arr
      integer                                    :: stat
      integer                                    :: n1 , n2
      logical                                    :: alloc_ok
      allocate(arr(n1,n2),stat=stat)
      alloc_ok = (stat == 0)
      if (.not.alloc_ok) call alloc_error()
      return
      end subroutine alloc_char_2D

      subroutine alloc_error()
         write(lunrep,'(//a)') ' * Part Memory Error '
         write(lunrep,'(  a)') ' *    Could not allocate required memory'
         write(lunrep,'(  a)') ' *    Please contact Delft3D Support        '
         write(lunrep,'(a//)') ' *    Part aborted                          '
         write(*     ,'(//a)') ' * Part Memory Error '
         write(*     ,'(  a)') ' *    Could not allocate required memory'
         write(*     ,'(  a)') ' *    Please contact Delft3D Support    '
         write(*     ,'(a//)') ' *    Part aborted                          '
         STOP ' Part aborted ! '
      end subroutine alloc_error
end module alloc_mod
