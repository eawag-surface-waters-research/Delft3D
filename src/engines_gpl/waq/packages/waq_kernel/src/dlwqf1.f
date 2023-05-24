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

      subroutine dlwqf1 ( noseg  , nobnd  , noq    , noq1   , noq2   ,
     &                    nomat  , ipoint , iwrk   , imat   , rowpnt ,
     &                    fmat   , tmat   )

!     Deltares - Delft Software Department

!     Created   : November 2009 by Leo Postma

!     Function  : modified version of dlwqf1 to create a zero based row pointer and also
!                 create direct mapping from iq to the matrix with fmat and tmat for from and to

!     Modified  :

      use m_srstop
      use timers
      implicit none

      integer(4), intent(in   ) :: noseg                 ! number of volumes
      integer(4), intent(in   ) :: nobnd               ! number of volumes
      integer(4), intent(in   ) :: noq                   ! number of exchanges
      integer(4), intent(in   ) :: noq1                  ! number of exchanges in first direction
      integer(4), intent(in   ) :: noq2                  ! number of exchanges in second direction
      integer(4), intent(in   ) :: nomat                 ! dimension of sparse matrix
      integer(4), intent(in   ) :: ipoint(  4,noq)       ! exchange pointers (dim: 4 x noq)
      integer(4), intent(inout) :: iwrk  (  noseg+nobnd) ! workspace
      integer(4), intent(  out) :: imat  (  nomat)       ! collumn pointers per row of sparse matrix
      integer(4), intent(  out) :: rowpnt(0:noseg+nobnd) ! row pointer, contains row lengths of mat (elsewhere: itrac)
      integer(4), intent(  out) :: fmat  (  noq)         ! location from(iq) in matrix
      integer(4), intent(  out) :: tmat  (  noq)         ! location to  (iq) in matrix

!     Local declarations

      integer(4)                :: i , j                 ! from- and to segments
      integer(4)                :: i2, j2                ! from- and to segments
      integer(4)                :: ip, jp                ! from- and to segment pointers
      integer(4)                :: iseg                  ! current volume
      integer(4)                :: iq                    ! current edge
      integer(4)                :: iadd                  ! help variable

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqf1", ithandl )

      rowpnt = 0
      iwrk   = 0
      imat   = 0
      fmat   = 0
      tmat   = 0

!         compute number of off-diagonals per row for first 2 directions

      do iq = 1, noq1+noq2
         i = ipoint(1,iq)
         j = ipoint(2,iq)
         if ( i .eq. 0 .or. j .eq. 0 ) cycle
         if ( i .gt. 0 ) rowpnt(i) = rowpnt(i) + 1
         if ( j .gt. 0 ) rowpnt(j) = rowpnt(j) + 1
      enddo

!         see if there is a third direction

      iadd = 0
      if ( noq .ne. noq1+noq2 ) iadd = 2  !  in 3D first 2 co diagonals are the vertical

!         accumulate to pointer start of rows

      rowpnt(0) = 0
      do iseg = 1, noseg
         rowpnt(iseg) = rowpnt(iseg) + rowpnt(iseg-1) + iadd
      enddo
      do iseg = noseg+1 , noseg+nobnd
         rowpnt(iseg) = rowpnt(iseg-1)
      enddo
      if ( rowpnt( noseg+nobnd ) .gt. nomat ) then
         write ( * , * ) ' System error in fast solvers matrix.'
         write ( * , * ) ' NOMAT = ',nomat, ', Required = ', rowpnt( noseg+nobnd )
         call srstop(1)
      endif

!         fill the (pointers in) matrix for the first 2 directions

      do iq = 1, noq1+noq2
         i = ipoint(1,iq)
         j = ipoint(2,iq)
         if ( i .eq. 0 .or. j .eq. 0 ) cycle
         i2 = i
         if ( i .lt. 0 ) i2 = noseg-i
         j2 = j
         if ( j .lt. 0 ) j2 = noseg-j
         if ( i .gt. 0 ) then
            ip = iwrk(i) + 1 + iadd
            if ( i .gt. 1 ) ip = ip+rowpnt(i-1)
            imat(ip) = j2
            fmat(iq) = ip
            iwrk(i ) = iwrk(i ) + 1
         endif
         if ( j .gt. 0 ) then
            jp = iwrk(j) + 1 + iadd
            if ( j .gt. 1 ) jp = jp+rowpnt(j-1)
            imat(jp) = i2
            tmat(iq) = jp
            iwrk(j ) = iwrk(j ) + 1
         endif
      enddo

!         fill the matrix for the last direction

      do iq = noq1+noq2+1 , noq
         i = ipoint(1,iq)
         j = ipoint(2,iq)
         if ( i .eq. 0 .or. j .eq. 0 ) cycle
         i2 = i
         if ( i .lt. 0 ) i2 = noseg-i
         j2 = j
         if ( j .lt. 0 ) j2 = noseg-j
         if ( i .gt. 0 ) then
            if ( j .lt. i ) then        ! first  off-diagonal element -> previous layer
               ip = 1
            else                        ! second off-diagonal element -> next layer
               ip = 2
            endif
            if ( i .gt. 1 ) ip = ip+rowpnt(I-1)
            imat(ip) = j2
            fmat(iq) = ip
         endif
         if ( j .gt. 0 ) then
            if ( j .lt. i ) then        ! first  off-diagonal element -> previous layer
               jp = 2
            else                        ! second off-diagonal element -> next layer
               jp = 1
            endif
            if ( j .gt. 1 ) jp = jp+rowpnt(J-1)
            imat(jp) = i2
            tmat(iq) = jp
         endif
      enddo

!         filling of the administration arrays completed

      if ( timon ) call timstop ( ithandl )
      RETURN
      END
