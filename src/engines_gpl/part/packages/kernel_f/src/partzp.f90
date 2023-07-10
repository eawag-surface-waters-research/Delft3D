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
module m_partzp

implicit none

contains


      subroutine partzp (lunpr, nopart, nmax, mmax, mnmax2, nolay, mpart, npart, kpart, zpart, &
                         lgrid, laytopp, laytop, locdepp, locdep, itime, itstrtp)

      use precision_part    ! single/double precision
      use timers
      implicit none    ! explicit typing

!     Arguments

!     kind                       name                     description
      integer(ip), intent(in   ) :: lunpr                 !< unit number for log files
      integer(ip), intent(in   ) :: nopart                !< total number of particles
      integer(ip), intent(in   ) :: nmax                  !< first dimension of the grid
      integer(ip), intent(in   ) :: mmax                  !< second dimension of the grid
      integer(ip), intent(in   ) :: mnmax2                !< nmax*mmax
      integer(ip), intent(in   ) :: nolay                 !< number of layers == layt
      integer(ip), intent(in   ) :: npart(nopart)         !< first  grid index of the particles
      integer(ip), intent(in   ) :: mpart(nopart)         !< second grid index of the particles
      integer(ip), intent(inout) :: kpart(nopart)         !< third grid index of the particles
      real   (sp), intent(inout) :: zpart(nopart)         !< z-value (0.0-1.0) third  direction within grid cell
      integer(ip), intent(in   ) :: lgrid(nmax,mmax)      !< grid with active grid numbers, negatives for open boundaries
      integer(ip), intent(in   ) :: laytopp(nmax,mmax)    !< highest active layer in z-layer model of previous time step
      integer(ip), intent(in   ) :: laytop(nmax,mmax)     !< highest active layer in z-layer model
      real   (sp), intent(inout) :: locdepp(mnmax2,nolay) !< depth per layer of previous time step
      real   (sp), intent(in   ) :: locdep(mnmax2,nolay)  !< depth per layer
      integer(ip), intent(in   ) :: itime                 !< current time
      integer(ip), intent(in   ) :: itstrtp               !< start time

!     local variables

!     kind                       name                  description
      real(sp), parameter     :: zsurf = 0.001       ! particles in top layer are close to surface when zp < zsurf
      integer(ip)             :: mp                  ! m of the particle
      integer(ip)             :: np                  ! n of this particle
      integer(ip)             :: kp                  ! particle layernr
      integer(ip)             :: ktopold             ! previous time or location k top
      integer(ip)             :: ktopnew             ! current k top
      real(sp)                :: zp                  ! relative location in layer
      integer(ip)             :: n0                  ! segment number 2d
      
      real(sp)                :: partdep             ! absolute location of particle from the top of the water column
      integer(ip)             :: ipart               ! particle loop counter
      integer(ip)             :: ilay                ! layer loop counter

      
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "partzp", ithndl )

!     only update after first time step
      if (itime > itstrtp) then
!$NOOMP PARALLEL
!$NOOMP DO PRIVATE(mp, np, kp, n0, ktopold, ktopnew, zp, partdep, ilay) SCHEDULE(DYNAMIC)
         do ipart = 1, nopart
            mp = mpart(ipart)
            np = npart(ipart)
            kp = kpart(ipart)
            n0 = lgrid(np, mp)
            ktopold = laytopp(np, mp)
            ktopnew = laytop(np, mp)
            zp = zpart(ipart)
            if (ktopold == ktopnew) cycle ! nothing to do when the toplayer doesn't change
            if (kp > ktopold .and. kp > ktopnew) cycle ! nothing to do when the particle was and is not in the top layer
   
            if (kp < ktopold) then
               ! the particle was and is (floating) above the top layer, this should not happen...
               write ( * , 1000) ipart, kp, ktopold
               write ( lunpr , 1000) ipart, kp, ktopold
          1000 format (/'  WARNING: Particle number',i12,' found in layer ',i4,' which was less than ktop (=',i4,')'/)
               kp = ktopold
            end if
   
            if (kp == ktopold .and. zp <= zsurf) then
               ! do not update zp when close to the surface (floating), just move to the new top layer and keep zp
               kp = ktopnew
            elseif (ktopold < ktopnew) then
               ! column has now less active layers
               ! calculate absolute position from the top of the water column in the old layers
               if (kp == 1) then
                  partdep = zp * locdepp(n0, kp)
               else
                  partdep = locdepp(n0, kp - 1) + zp * (locdepp(n0, kp) - locdepp(n0, kp - 1))
               endif
               ! calculate the relative location of the particle over all old layers that merge into the new toplayer
               zp = partdep / locdepp(n0, ktopnew)
               kp = ktopnew
            else
               ! column has now more active layers
               ! calculate absolute position from the top of the water column in the new layers
               partdep = zp * locdep(n0, ktopold)
               ! search in which layer we are
               do ilay = ktopnew, ktopold
                  if (partdep <= locdep(n0, ilay)) then
                     ! the particle is now in this layer
                     kp = ilay
                     exit              
                  endif   
               enddo
               if (ilay == 1) then
                  ! in the top layer the relative lacation is the particle depth divided by the 
                  zp = partdep / locdep(n0, ilay)
               else
                  zp = (partdep - locdep(n0, ilay - 1)) / (locdep(n0, ilay) - locdep(n0, ilay - 1))
               endif
            endif      
            kpart(ipart) = kp
            zpart(ipart) = zp
         enddo
!$NOOMP END PARALLEL DO
      endif

!     copy locdep to locdepp
      do ilay = 1, nolay
         do n0 = 1, mnmax2
            locdepp(n0, ilay) = locdep(n0, ilay)
         enddo
      enddo

!     end of subroutine

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine

end module m_partzp
