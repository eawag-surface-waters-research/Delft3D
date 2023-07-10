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
module m_rdccol
use m_stop_exit


implicit none

contains


      subroutine rdccol ( nmax   , mmax   , lun    , fnam   ,   &
                          lgrid  , xbott  , ybott  , lun2   )

!     READING CURVILINEAR CCO FILE
!          (initially)

!     system administration : m. zeeuw


!     created               : juli 1989, by m.e. sileon



!     modified              : june 1993, by m. zeeuw
!                             - implemented error numbers
!                             nov 1997: uses openfl
!                             dec 1997: read also layt form first record!!
!                                       and 9 times xdummy

!     note                  : standard version july 1991

!     logical unit numbers  : lun

!     subroutines called    : stop_exit

!     functions   called    : none.

      use precision_part      ! single and double precision
      use timers
      use openfl_mod

!     parameters

!     kind           function         name                Descriptipon

      integer  (ip), intent(in   ) :: nmax              !< first dimension of the grid
      integer  (ip), intent(in   ) :: mmax              !< second dimension of the grid
      integer  (ip), intent(in   ) :: lun               !< unit number cco file
      character( *), intent(in   ) :: fnam              !< name of cco file
      integer  (ip), intent(in   ) :: lgrid(nmax,mmax)  !< grid table
      real     (sp), intent(  out) :: xbott(*)          !< x-values in the grid
      real     (sp), intent(  out) :: ybott(*)          !< y-values in the grid
      integer  (ip), intent(in   ) :: lun2              !< unit number log-file

!     local scalars

      integer(ip)   iocond    ! error indicator for file opening
      integer(ip)   nmaxc     ! nmax in file
      integer(ip)   mmaxc     ! mmax in file
      real   (rp)   x0, y0    ! coordinates of the zero
      real   (rp)   alpha     ! unknown
      integer(ip)   npart     ! unknown
      integer(ip)   layt      ! number of layers
      real   (sp)   xdummy    ! help variable
      integer(ip)   i , j     ! loop variables

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "rdccol", ithndl )

!     open cco-file

      write ( lun2, * ) ' Opening the grid coordinates file:', fnam(1:len_trim(fnam))
      call openfl ( lun, fnam, 0 )

!     read requested data
      
      read (lun)
      read (lun) mmaxc, nmaxc, x0, y0, alpha, npart, layt
      if ( mmaxc .ne. mmax .or. nmaxc .ne. nmax ) then
          write (lun2, *)
          write (lun2, *) ' Error 4201. Dimensioning does not match!'
          write (lun2, *) '             nmax,mmax,lgrid-table:   ', nmax , mmax
          write (lun2, *) '             nmax,mmax,cco-file   :   ', nmaxc, mmaxc
          call stop_exit(1)
      endif
      
!     skip header
      
      do i = 1, 2 * npart + 9
        read (lun) xdummy
      enddo
      read (lun, iostat = iocond) ((xbott(lgrid(i, j)), i = 1, nmaxc), j = 1, mmaxc)
      read (lun, iostat = iocond) ((ybott(lgrid(i, j)), i = 1, nmaxc), j = 1, mmaxc)
 !
      close (lun)

      if ( iocond .gt. 0 ) write (lun2, *) ' Error 4202. Reading cco-file:', fnam
      if ( iocond .lt. 0 ) write (lun2, *) ' Error 4203. Unexpected end cco-file:', fnam
      if ( iocond .ne. 0 ) call stop_exit(1)
      write (lun2,'(a,a)') '  Succesful reading of the TELMAC-CCO file : ',fnam(1:len_trim(fnam))

!     end of routine

      if ( timon ) call timstop ( ithndl )
      return

      end subroutine

end module m_rdccol
