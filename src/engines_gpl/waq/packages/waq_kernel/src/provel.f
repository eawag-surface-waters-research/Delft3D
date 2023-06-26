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
      module m_provel

      implicit none

      contains


      subroutine provel (velonw, nveln , ivpnew, velo  , novelo,
     +                   ivpnt , velx  , nvelx , vsto  , nosys ,
     +                   noq   , velndt, istep )
!
!     function            : makes velonw array from velo and velx array
!
!     created:            : december 1994 by jan van beek
!
!     modified            : october  2010, jvb,  only update new velocities if needed

      use timers
      implicit none

      ! declaration of arguments

      integer, intent(in)       :: nveln                           ! number of new velocities
      integer, intent(in)       :: novelo                          ! number of velocities from input
      integer, intent(in)       :: nvelx                           ! number of velocities from processes
      integer, intent(in)       :: nosys                           ! number of active substances
      integer, intent(in)       :: noq                             ! number of exchanges
      real   , intent(inout)    :: velonw(nveln,noq)               ! new velocity array
      integer, intent(in)       :: ivpnew(nosys)                   ! pointer to new velo array (actually only input)
      real   , intent(in)       :: velo(novelo,noq)                ! velocities from input
      integer, intent(in)       :: ivpnt(nosys)                    ! pointer to original velo
      real   , intent(in)       :: velx(nvelx,noq)                 ! velocities from processes
      real   , intent(in)       :: vsto(nosys,nvelx)               ! factor for velocities
      integer, intent(in)       :: velndt(nvelx)                   ! time step size of the velocities
      integer, intent(in)       :: istep                           ! time step nr.

      ! local declarations

      integer                   :: isys                            ! index substances
      integer                   :: isys2                           ! index substances
      integer                   :: ivnw                            ! index new velocities
      integer                   :: ivx                             ! index velocities from process
      integer                   :: ivp                             ! index velocities from input
      integer                   :: iq                              ! index exchange
      integer                   :: ivpnew_loc(nosys)               ! local copy of ivpnew
      logical                   :: lfirst                          ! first velocity in combination of velocities
      logical                   :: update                          ! update of the combined velocity needed
      real                      :: factor                          ! factor for susbtance velocity combination
      integer(4), save          :: ithandl = 0                     ! handle in timer routines

      ! activate time routines

      if ( timon ) call timstrt ( "provel", ithandl )

      ! local copy of ivpnew

      ivpnew_loc = ivpnew

      ! we construeren nu de velonw

      do isys = 1 , nosys

         do ivnw = 1 , nveln

            if ( ivpnew_loc(isys) .eq. ivnw ) then

               ! check if update is needed, always from input, fractional step from processes

               update = .false.
               if ( ivpnt(isys) .ne. 0 ) update = .true.
               do ivx = 1  , nvelx
                  factor = vsto(isys,ivx)
                  if ( abs(factor) .gt. 1.e-20 ) then
                     if ( mod( istep-1, velndt(ivx) ) .eq. 0 ) update = .true.
                  endif
               enddo

               if ( update ) then

                  ! look in original velo

                  lfirst = .true.
                  if ( ivpnt(isys) .ne. 0 ) then
                     lfirst = .false.
                     ivp = ivpnt(isys)
                     do iq = 1 , noq
                        velonw(ivnw,iq) = velo(ivp,iq)
                     enddo
                  endif

                  ! add the contribution of the calculated velocities.

                  do ivx = 1  , nvelx
                     factor = vsto(isys,ivx)
                     if ( abs(factor) .gt. 1.e-20 ) then
                        if ( lfirst ) then
                           lfirst = .false.
                           if ( abs(factor-1.0) .lt. 1.e-10 ) then
                              do iq = 1 , noq
                                 velonw(ivnw,iq) = velx(ivx,iq)
                              enddo
                           else
                              do iq = 1 , noq
                                 velonw(ivnw,iq) = factor*velx(ivx,iq)
                              enddo
                           endif
                        else
                           if ( abs(factor-1.0) .lt. 1.e-10 ) then
                              do iq = 1 , noq
                                    velonw(ivnw,iq) = velonw(ivnw,iq) +
     +                                                velx(ivx,iq)
                              enddo
                           else
                              do iq = 1 , noq
                                    velonw(ivnw,iq) = velonw(ivnw,iq) +
     +                                                factor*velx(ivx,iq)
                              enddo
                           endif
                        endif
                     endif
                  enddo

               endif

               ! trick the other substances also pointing to this array by setting pointer negative

               do isys2 = isys + 1 , nosys
                  if ( ivpnew_loc(isys2) .eq. ivnw ) then
                     ivpnew_loc(isys2) = -ivpnew_loc(isys2)
                  endif
               enddo

               ! there can be no other new velocity for this substance so exit nveln loop

               exit

            endif

         enddo

      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_provel
