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
module m_part01

implicit none

contains


      subroutine part01 ( lgrid  , lgrid2 , xp     , yp     , dx     ,  &
                          dy     , area   , angle  , nmax   , mmax   )
!
!
!                   Deltares
!
!                        d e l p a r    v1.30
!
!
!     system administration : m. zeeuw
!
!
!     created               : january 1990, by l.postma
!
!
!     function              : computes distances in the grid
!
!
!     note                  : none.
!
!
!     logical unit numbers  : none.
!
!
!     subroutines called    : none.
!
!
!     functions   called    : none.
!
!
      use precision_part       ! single/double precision
      use timers
      implicit none       ! force explicit typing

!     parameters

!     kind         function         name                    Descriptipon

      integer(ip), intent(in   ) :: nmax                  !< first dimension lgrid
      integer(ip), intent(in   ) :: mmax                  !< second dimension lgrid
      integer(ip), intent(in   ) :: lgrid (nmax,mmax)     !< active grid indices matrix
      integer(ip), intent(in   ) :: lgrid2(nmax,mmax)     !< total grid indices matrix
      real   (sp), intent(in   ) :: xp    (nmax*mmax)     !< x of upper right corner grid point
      real   (sp), intent(in   ) :: yp    (nmax*mmax)     !< y of upper right corner grid point
      real   (sp), intent(  out) :: dx    (nmax*mmax)     !< x distance of grid cell
      real   (sp), intent(  out) :: dy    (nmax*mmax)     !< y distance of grid cell
      real   (sp), intent(  out) :: area  (nmax*mmax)     !< horizontal surface area
      real   (sp), intent(  out) :: angle (nmax*mmax)     !< angle of the gridcell

!     local scalars

      real   (sp), parameter        :: default = 999.99
      real    (sp) a1      !  angle
      real    (sp) dx1     !  help var.
      real    (sp) dxa     !  help var.
      real    (sp) dxb     !  help var.
      real    (sp) dy1     !  help var.
      real    (sp) dya     !  help var.
      real    (sp) dyb     !  help var.
      real    (sp) pi
      real    (sp) sqrt
      integer (ip) i       !  index var.
      integer (ip) j       !  index var.
      integer (ip) n0      !  lgrid value
      integer (ip) n1      !  lgrid value
      integer (ip) n2      !  lgrid value
      integer (ip) n3      !  lgrid value

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part01", ithndl )
!
!     loop over the grid
!
      pi  = 4.0*atan(1.0)
      dx  = default
      dy  = default
      do 100 i = 1, nmax
        do 50 j = 1, mmax
!
          n0 = lgrid(i, j)
          if (n0  >  0) then
            n1  = lgrid2(i - 1, j    )
            n2  = lgrid2(i    , j - 1)
            n3  = lgrid2(i - 1, j - 1)
!
            dxa = 0.5*(xp(n0) + xp(n1) - xp(n2) - xp(n3))
            dya = 0.5*(yp(n0) + yp(n1) - yp(n2) - yp(n3))
            dx1 = sqrt(dxa**2 + dya**2)
            dxb = 0.5*(xp(n0) - xp(n1) + xp(n2) - xp(n3))
            dyb = 0.5*(yp(n0) - yp(n1) + yp(n2) - yp(n3))
            dy1 = sqrt(dxb**2 + dyb**2)
!
!            if (dxa  /=  0.0) then
!              a1 = atan(dya / dxa)
!              if (dxa  < 0.0) then
!                a1 = a1 + sign(pi, dya)
!              endif
!            else
!              a1 = 0.5*sign(pi, dya)
!            endif
!
!           supposed unit for angle (see part10) is degrees.
!
            if (dxa  /=  0.0) then
               a1 = atan2(dya,dxa)
            else
               a1 = 0.5*sign(pi, dya)
            endif

            angle(n0) = a1
            area (n0) = dx1 * dy1
            dx   (n0) = dx1
            dy   (n0) = dy1
!
          endif
!
   50   continue
  100 continue
!
!     end of routine
!
      if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine

end module m_part01
