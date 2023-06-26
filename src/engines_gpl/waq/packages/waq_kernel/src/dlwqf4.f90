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
module m_dlwqf4

implicit none

contains


      subroutine dlwqf4 ( noseg  , nobnd  , nosys  , notot  , isys   ,         &
     &                    idt    , conc   , deriv  , volold , bound  ,         &
     &                                      rhs    , diag   , sol    )

!     Deltares - Delft Software Department

!     Created   : Nov. 1996 by Kian Tan

!     Function  : define right hand side of the matrix equation

!     Modified  : July 2008, Leo Postma  : WAQ performance timers
!                 July 2009, Leo Postma  : double precission version

      use timers                         ! WAQ performance timers

      implicit none

!     Arguments           :

!     Kind        Function         Name                  Description

      integer(4), intent(in   ) :: noseg               ! Number of computational volumes
      integer(4), intent(in   ) :: nobnd               ! Number of open boundaries
      integer(4), intent(in   ) :: nosys               ! Number of transported substances
      integer(4), intent(in   ) :: notot               ! Total number of substances
      integer(4), intent(in   ) :: isys                ! This substance
      integer(4), intent(in   ) :: idt                 ! timestep in scu's
      real   (4), intent(in   ) :: conc  (notot,noseg) ! all concentrations
      real   (4), intent(in   ) :: deriv (notot,noseg) ! all derivatives (loads, processes)
      real   (4), intent(in   ) :: volold(noseg)       ! volumes at beginning of time step
      real   (4), intent(in   ) :: bound (nosys,nobnd) ! open boundary concentrations
      real   (8), intent(  out) :: rhs   (noseg+nobnd) ! right hand side of the equation
      real   (8), intent(in   ) :: diag  (noseg+nobnd) ! value of the diagonal
      real   (8), intent(  out) :: sol   (noseg+nobnd) ! initial guess

!     Local declarations

      real   (8) ddt                                   ! 1.0 / time step in double
      integer(4) iseg                                  ! Loop variable

!     The WAQ-timer

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqf4", ithandl )

!         set the right hand side, normal part

      ddt = 1.0d00 / idt

      do iseg = 1 , noseg
         rhs(iseg) = deriv(isys,iseg) + volold(iseg)*conc(isys,iseg)*ddt
      enddo

!         set the right hand side, open boundary part

      do iseg = 1, nobnd
         rhs(noseg+iseg) = bound(isys,iseg)
      enddo

!        row scaling

      do iseg = 1, noseg + nobnd
         rhs(iseg) = rhs(iseg) / diag(iseg)
      enddo

!        zero initial guess, try previous concentration for water volumes
!        ( alternatively take zero vector ). Zero initial guess for boundaries.

      sol = 0.0
      do iseg = 1, noseg
         sol(iseg) = conc(isys,iseg) + 0.01
      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

end module m_dlwqf4
