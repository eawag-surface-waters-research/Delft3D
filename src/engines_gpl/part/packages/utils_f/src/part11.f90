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
module m_part11

implicit none

contains


      subroutine part11( lgrid  , xp     , yp     , nmax   , npart  ,        &
                         mpart  , xpart  , ypart  , xa     , ya     ,        &
                         nopart , npwndw , lgrid2 , kpart  , zpart  ,        &
                         za     , locdep , dps    , nolay  , mmax   ,        &
                         tcktot)

!       Deltares Software Centre

!>\file
!>         Calculates real world x,y,z from the particles in-cell position
!>
!>         Interpolates bi-linearly between the corner coordinates of the particle cell.
!>         That is why both active grid (lgrid) and the total (lgrid2) are needed.\n
!>         It seems that in part10 the n,m of non-floating particles is set to a negative
!>         number to exclude them from this computation (needs to be checked).

!     System administration : Antoon Koster

!     Created               : Januari 1990 by Leo Postma

!     Modified              : March   1991 by Arjen Markus : use both lgrid and lgrid2

!     Logical unit numbers  : none

!     Subroutines called    : none

!     Functions   called    : none

      use precision_part               ! single/double precision
      use partmem, only: zmodel, laytop , laybot , zlbot 
      use timers
      use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan 

      implicit none               ! force explicit typing

!     Arguments

!     kind            function         name                      description

      integer  ( ip), intent(in   ) :: nmax                    !< first grid index
      integer  ( ip), intent(in   ) :: mmax                    !< second grid index
      integer  ( ip), intent(in   ) :: nolay                   !< number of layers
      integer  ( ip), intent(in   ) :: npwndw                  !< start nr of active particle
      integer  ( ip), intent(in   ) :: nopart                  !< total number of active particles
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid matrix
      real     ( rp), intent(in   ) :: xp    (nmax*mmax)       !< x of the grid cell corner
      real     ( rp), intent(in   ) :: yp    (nmax*mmax)       !< y of the grid cell corner
      real     ( rp), intent(in   ) :: locdep(nmax*mmax,nolay) !< local depth of a gridcell
      real     ( rp), intent(in   ) :: tcktot(nolay )          !< relative thickness of a layer
      real     ( rp), intent(in   ) :: dps   (nmax*mmax)       !< depth of the reference plain
      
!      integer  ( ip), intent(in   ) :: laytop(nmax,mmax)       !< highest active layer in z-layer model
!      integer  ( ip), intent(in   ) :: laybot(nmax,mmax)       !< deepest active layer in z-layer model
!      real     ( rp), intent(in   ) :: zlbot (nolay)           !< z-layer layer bottom level
!      real     ( rp), intent(in   ) :: zltop (nolay)           !< z-layer layer top level

      integer  ( ip), intent(in   ) :: npart (nopart)          !< first grid cell index particles
      integer  ( ip), intent(in   ) :: mpart (nopart)          !< second grid cell index particles
      integer  ( ip), intent(in   ) :: kpart (nopart)          !< third grid cell index particles
      real     ( rp), intent(in   ) :: xpart (nopart)          !< x-in the grid of particles
      real     ( rp), intent(in   ) :: ypart (nopart)          !< y-in the grid of particles
      real     ( rp), intent(in   ) :: zpart (nopart)          !< z-in the grid of particles
      real     ( rp), intent(  out) :: xa    (nopart)          !< absolute x of particles
      real     ( rp), intent(  out) :: ya    (nopart)          !< absolute y of particles
      real     ( rp), intent(  out) :: za    (nopart)          !< absolute z of particles

!     Locals:

      integer( ip) ipart   ! loop varible particle loop
      integer( ip) n       ! first grid index of this particle
      integer( ip) m       ! second grid index of this particle
      integer( ip) ilay    ! third grid index of this particle
      integer( ip) n0      ! linear 2D grid cell number of the particle
      integer( ip) n1      ! first index minus 1 grid number
      integer( ip) n2      ! second index minus 1 grid number
      integer( ip) n3      ! both indices minus 1 grid number
      real   ( rp) xt      ! in-cell x value of this particle
      real   ( rp) yt      ! in-cell y value of this particle
      real   ( rp) totdep  ! depth of the bed at the location of the particle
      real   ( rp) dlay    ! thickness of the layer at the location of the particle
      real   ( rp) dist    ! depth of the particle relative to the surface

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part11", ithndl )

!     loop over the number of particles

!$OMP PARALLEL DO PRIVATE  ( n, m, n0, n1, n2, n3, xt, yt, totdep,     &
!$OMP                        ilay, dlay, dist ),                       &
!$OMP             SCHEDULE ( DYNAMIC, max((nopart-npwndw)/100,1)  )
      do 100 ipart = npwndw, nopart
         n  = npart(ipart)
         m  = mpart(ipart)

!        only perform transformation particles with positive (n,m) indices
!        works as a mask to exclude certain particles from transformation.
!        in later delpar version used to exclude non-floating particles
!        (see end of routine part10)

         if ( n .ge. 0 .and. m .ge. 0 ) then
            n0 = lgrid(n, m)
            if ( n0 .gt. 0 ) then
               n1        = lgrid2(n - 1, m    )
               n2        = lgrid2(n    , m - 1)
               n3        = lgrid2(n - 1, m - 1)
               xt        = xpart(ipart)
               yt        = ypart(ipart)

!              horizontal coordinates (x and y)

               xa(ipart) = xp(n3) +                                          &
                       xt * (xp(n1) - xp(n3)) + yt * (xp(n2) - xp(n3)) +     &
                       xt * yt * (xp(n0) - xp(n1) - xp(n2) + xp(n3))
               ya(ipart) = yp(n3) +                                          &
                       xt * (yp(n1) - yp(n3)) + yt * (yp(n2) - yp(n3)) +     &
                       xt * yt * (yp(n0) - yp(n1) - yp(n2) + yp(n3))

!              vertical coordinate (z)
               if (zmodel) then
                  ilay     = kpart(ipart)
                  if (ilay == laytop(n, m)) then
                     dlay = locdep(n0,ilay)
                     za(ipart)= zlbot(ilay) + (1.0-zpart(ipart))*dlay
                  else if (ilay .le. laybot(n, m)) then
                     dlay = locdep(n0,ilay) - locdep(n0,ilay-1)
                     za(ipart)= zlbot(ilay-1) - zpart(ipart)*dlay
                  else
                     ilay = laybot(n, m)
                     dlay = locdep(n0,ilay) - locdep(n0,ilay-1)
                     za(ipart)= zlbot(ilay-1) - dlay
                  endif
               else
                  totdep   = locdep(n0,nolay)
                  ilay     = kpart(ipart)
                  if ( ilay .le. nolay ) then
                     dlay     = tcktot(ilay)*totdep
                     dist     = locdep(n0,ilay) - (1.0-zpart(ipart))*dlay
                     za(ipart)= totdep - dps(n0) - dist
                  else
                     za(ipart)= -dps(n0)
                  endif
               endif
            else

!              particles outside model area

               xa(ipart) = -999
               ya(ipart) = -999
               za(ipart) = -999
            endif
         endif
  100 continue
!$OMP END PARALLEL DO

!     end of subroutine

      if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine part11


end module m_part11
