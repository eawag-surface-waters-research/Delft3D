!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

      subroutine wq_processes_integrate_velocities ( nosys  , notot  , noseg  , noq    , novelo , &
                                                     velo   , area   , volume , ipoint , iknmrk , &
                                                     ivpnt  , conc   , idt    , deriv  )

!     Deltares Software Centre

!>\file
!>         Makes explicit upwind derivatives for the aditonal velocities from the proces library
!>
!>         This routine makes for the nosys transported substaces the contribution of the advection and
!>         the diffusion to the DERIV(notot,noseg) array. Notot is the total number of substances,
!>         noseg is the number of computational volumes.\n

!     Function            : Makes explicit derivatives according to additional flow

!     Routines            : none

      use timers

      implicit none

!     Parameters          :

!     kind           function         name                   description

      integer  ( 4), intent(in   ) :: nosys                !< number of transported substances
      integer  ( 4), intent(in   ) :: notot                !< total number of substances
      integer  ( 4), intent(in   ) :: noseg                !< number of computational volumes
      integer  ( 4), intent(in   ) :: noq                  !< total number of interfaces
      integer  ( 4), intent(in   ) :: novelo               !< number additional velocities
      real     ( 4), intent(in   ) :: velo  (novelo,noq)   !< array with additional velocities
      real     ( 4), intent(in   ) :: area  (noq)          !< exchange areas in m2
      real     ( 4), intent(in   ) :: volume(noseg)        !< volumes in m3
      integer  ( 4), intent(in   ) :: ipoint(  4   ,noq)   !< from, to, from-1, to+1 volume numbers
      integer  ( 4), intent(in   ) :: iknmrk(noseg)        !< feature array
      integer  ( 4), intent(in   ) :: ivpnt (nosys)        !< additional velocity number per substance
      real     ( 4), intent(in   ) :: conc  (notot,noseg)  !< concentrations at previous time level
      integer  ( 4), intent(in   ) :: idt                  !< time step in seconds
      real     ( 4), intent(inout) :: deriv (noseg,notot)  !< explicit derivative in mass/m3/s

!     Local variables     :

      integer  ( 4) iq          ! loop counter exchanges
      integer  ( 4) isys        ! loop counter substance
      integer  ( 4) ifrom, ito  ! from and to volume numbers
      real     ( 4) a           ! this area
      real     ( 4) vfrom       ! from volume
      real     ( 4) vto         ! to volume
      real     ( 4) q           ! flow for this exchange
      real     ( 4) dq          ! total flux from and to

      integer(4), save :: ithndl = 0
      if (timon) call timstrt( "wq_processes_integrate_velocities", ithndl )

      !     loop accross the number of exchanges

      do iq = 1 , noq
          ifrom = ipoint(1,iq)
          ito   = ipoint(2,iq)
          if ( ifrom .le. 0 .or. ito .le. 0 ) cycle
          a = area(iq)
          if(ifrom.gt.0) then
              vfrom = volume(ifrom)
          endif
          if(ito.gt.0) then
              vto = volume(ito)
          endif
          do isys = 1, nosys
              if ( ivpnt(isys) .gt. 0 ) then
                  q = velo  ( ivpnt(isys), iq ) * a
                  if ( q .gt. 0.0 ) then
                      dq = q*conc(isys,ifrom)
                  else
                      dq = q*conc(isys,ito  )
                  endif
                  if(ifrom.gt.0.and.vfrom.gt.0.0) then
                      deriv(ifrom,isys) = deriv(ifrom,isys) - dq/vfrom
                  endif
                  if(ito.gt.0.and.vto.gt.0.0) then
                      deriv(ito,isys) = deriv(ito,isys) + dq/vto
                  endif
              endif
          enddo
      enddo
      if (timon) call timstop( ithndl )
      return
      end
