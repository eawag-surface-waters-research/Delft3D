!!  Copyright (C)  Stichting Deltares, 2012-2015.
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

      subroutine get_effi( temper, radiat, ext   , depthw, daylen, nspe  , effi )
!>\file
!>       calculate and store efficiency for all species

      implicit none
      
!     BLOOM commons

      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'arran.inc'

!     arguments

      real     temper     ! input , temperature
      real     radiat     ! input , radiation
      real     ext        ! input , total extinction
      real     depthw     ! input , depth of the layer
      real     daylen     ! input , daylength in hours
      integer  nspe       ! output, number of bloom algea species
      real     effi(30)   ! output, calculated efficiencies per species

!     local decalarations

      real*8   temp       ! temperature
      real*8   csol       ! radiation
      real*8   dsol       ! radiation
      real*8   dep        ! depth
      real*8   exttot     ! total extinction
      real*8   day        ! daylength in hours
      real*8   tcorr      ! tcorr
      real*8   surf_typ   ! scaled, converted and corrected radiation for a type
      integer  ntyp       ! number of bloom algea types
      integer  itype      ! index number of bloom algae type
      integer  igroup     ! index number of bloom algae group
      real*8   pmax20(mt),sdmixn(mt)

      real*8   phi_s      ! x value tabulated function at surface
      real*8   fun_s      ! function at surface
      real*8   der_s      ! derivative at sutface
      real*8   phi_d      ! x value tabulated function at dep
      real*8   fun_d      ! function at surface at dep
      real*8   der_d      ! derivative at sutface at dep

      dep    = depthw
      exttot = ext
      temp   = temper
      day    = daylen
      nspe   = nuecog
      ntyp   = maxval(it2)
      effi = 0.0d0
      
      call maxprd ( tefcur )
      do itype = 1,ntyp
         pmax20(itype) = pmax(itype)
         if (sdmix(itype) .lt. 0.0) then
            sdmixn(itype) = 1.0d0 + sdmix(itype)
         else
            sdmixn(itype) = 0.0d0
         endif
      enddo
      call maxprd ( temp  )

      dsol=1428.57d0 * solaco * radiat
      do igroup = 1 , nuecog
         do itype = it2(igroup,1),it2(igroup,2)
            tcorr      = pmax20(itype)/pmax(itype)
            surf_typ   = tcorr * dsol * dexp (- exttot * sdmixn(itype) * dep)
            surf_typ   = surf_typ/day
            if ( surf_typ .gt. 1.0 .and. exttot*dep .gt. 1.0d-10) then
               phi_s = - dlog(surf_typ)
               call ebcalc(phi_s,fun_s,der_s,igroup)
               phi_d = exttot*dep - dlog(surf_typ)
               call ebcalc(phi_d,fun_d,der_d,igroup)
               effi(igroup) = max(effi(igroup), (fun_d-fun_s)/exttot/dep)
            else
               effi(igroup) = 0.0
            endif
      
         enddo
      enddo

      return
      end subroutine get_effi

      subroutine get_nspe( nspe )
      integer  nspe       ! input , number of bloom algea types
      include 'blmdim.inc'
      include 'size.inc'
      include 'phyt2.inc'

      nspe   = nuecog

      return
      end subroutine get_nspe