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
module m_part06

implicit none

contains


      subroutine part06 ( lun    , lgrid  , lgrid2 , nmax   , mmax   ,      &
                          xb     , yb     , nodye  , nocont , xwaste ,      &
                          ywaste , nwaste , mwaste )

!       Deltares Software Centre

!>\file
!>            Determines the grid cells and relative coordinates of waste locations
!>
!>            The wastelocations are given by the user in global x,y coordinates.\n
!>            This routine determines the n,m grid indices and the local x,y coordinates.\n
!>            The local x,y coordinates are 0< .. <1 and are store in the old x,y locations

!     System administration : Antoon Koster

!     Created               : February 1990, by Leo Postma

!     Modified              : August   2011, by Leo Postma, only warning if outside grid

!     logical unit numbers  : lun    - output log file

!     subroutines called    : part07 - searches for cell and coordinates

      use precision_part
      use timers
      use grid_search_mod

      implicit none

!     Arguments

!     kind            function         name                   description

      integer  ( ip), intent(in   ) :: lun                  !< unit number output log file
      integer  ( ip), intent(in   ) :: nmax                 !< first index hydrodynamic grid
      integer  ( ip), intent(in   ) :: mmax                 !< second index hydrodynamic grid
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)    !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)    !< total grid matrix
      real     ( rp), intent(in   ) :: xb    (nmax*mmax)    !< x of grid corner points
      real     ( rp), intent(in   ) :: yb    (nmax*mmax)    !< y of grid corner points
      integer  ( ip), intent(in   ) :: nodye                !< number of dye releases
      integer  ( ip), intent(in   ) :: nocont               !< number of continuous release
      real     ( rp), intent(inout) :: xwaste(nodye+nocont) !< x of wasteload location
      real     ( rp), intent(inout) :: ywaste(nodye+nocont) !< y of wasteload location
      integer  ( ip), intent(  out) :: nwaste(nodye+nocont) !< first grid index wasteloads
      integer  ( ip), intent(  out) :: mwaste(nodye+nocont) !< second grid index wasteloads

!     Locals

      integer  ( ip) ::  id      ! loop counter wasteloads
      integer  ( ip) ::  ierror  ! error variable of part07
      real     ( rp) ::  xnloc   ! input x coordinate for the grid search
      real     ( rp) ::  ynloc   ! input y coordinate for the grid search
      real     ( rp) ::  xmloc   ! output x coordinate for the grid search
      real     ( rp) ::  ymloc   ! output y coordinate for the grid search
      integer  ( ip) ::  nmloc   ! output n index of the wasteload point
      integer  ( ip) ::  mmloc   ! output m index of the wasteload point
      integer  ( ip) ::  noerr   ! local error accumulator

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part06", ithndl )

!     loop over the number of wasteloads

      noerr = 0
      do id = 1, nodye + nocont
         xnloc = xwaste(id)
         ynloc = ywaste(id)
         call part07 ( lgrid   , lgrid2  , nmax    , mmax    , xb      ,    &
                       yb      , xnloc   , ynloc   , nmloc   , mmloc   ,    &
                       xmloc   , ymloc   , ierror  )
         if ( ierror .eq. 0 ) then
            nwaste(id) = nmloc
            mwaste(id) = mmloc
            xwaste(id) = xmloc
            ywaste(id) = ymloc
         else                          ! location invalid
            if ( id .gt. nodye ) then
               write ( lun, 1010 ) id-nodye, xnloc, ynloc
            else
               write ( lun, 1000 ) id      , xnloc, ynloc
            endif
            noerr = noerr + 1
         endif
      enddo

      if ( noerr .ne. 0 ) write ( lun, 1020 ) noerr

!     end of routine

      if ( timon ) call timstop ( ithndl )
      return

 1000 format( '  Warning 4901. Dye        release', i3,' at (x,y): (',   &
                 f9.2,',',f9.2,') not on active grid cell.' )
 1010 format( '  Warning 4902. Continuous release', i3,' at (x,y): (',   &
                 f9.2,',',f9.2,') not on active grid cell.' )
 1020 format( '  Total number of waste load warnings = ', i3 )

      end subroutine

end module m_part06
