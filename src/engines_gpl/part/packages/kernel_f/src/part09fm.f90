!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

module part09fm_mod
!
contains
      subroutine part09fm ( lun2   , itime  , nodye  , nwaste , mwaste ,  &
                            xwaste , ywaste , iwtime , amassd , aconc  ,  &
                            npart  , mpart  , xpart  , ypart  , zpart  ,  &
                            wpart  , iptime , nopart , radius , nrowswaste, &
                            xpolwaste       , ypolwaste       , ndprt  ,  &
                            nosubs , layt   , tcktot , zmodel ,  &
                            laytop , laybot , nplay  , laywaste,nolay  ,  &
                            modtyp , zwaste , track  , nmdyer , substi ,  &
                            rhopart )

!       Deltares Software Centre

!>\file
!>         Adds mass for dye releases
!>
!>         The routine has been adapted from part09 to accommodate flexible mesh.

      use precision_part          ! single/double precision
      use timers
      use grid_search_mod
      use spec_feat_par
      use m_particles, only: xrpart, yrpart, zrpart
      use m_sferic, only: jsferic
      use m_sferic_part, only: ptref
      use geometry_module, only: Cart3Dtospher, sphertocart3D
      use mathconsts, only: raddeg_hp, pi
      use physicalconsts, only: earth_radius
      use random_generator
      use m_part_modeltypes

      implicit none

!     Arguments

!     kind            function         name                    description

      integer  ( ip), intent(in   ) :: nodye                 !< nr of dye release points
      integer  ( ip), intent(in   ) :: nosubs                !< nr of substances
      integer  ( ip), intent(in   ) :: layt                  !< number of hydr. layer
      integer  ( ip), intent(in   ) :: itime                 !< actual time
      integer  ( ip), intent(inout) :: iwtime (nodye)        !< array of wasteload times
      integer  ( ip), intent(in   ) :: nwaste (nodye)        !< n-values of waste locations
      integer  ( ip), intent(in   ) :: mwaste (nodye)        !< m-values of waste locations
      real     ( rp), intent(in   ) :: xwaste (nodye)        !< x-values of waste locations
      real     ( rp), intent(in   ) :: ywaste (nodye)        !< y-values of waste locations
      real     ( rp), intent(in   ) :: zwaste (nodye)        !< z-values of waste locations
      real     ( rp), intent(in   ) :: amassd (nosubs,nodye) !< total masses per dye release
      real     ( rp), pointer       :: aconc  (:,:)          !< mass per particle
      integer  ( ip), intent(  out) :: npart  (*)            !< n-values particles
      integer  ( ip), intent(in   ) :: ndprt  (nodye)        !< no. particles per waste entry
      integer  ( ip), intent(  out) :: mpart  (*)            !< m-values particles
      real     ( dp), intent(  out) :: xpart  (*)            !< x-in-cell of particles
      real     ( dp), intent(  out) :: ypart  (*)            !< y-in-cell of particles
      real     ( dp), intent(  out) :: zpart  (*)            !< z-in-cell of particles
      real     ( rp), intent(  out) :: wpart  (nosubs,*)     !< weight of the particles
      integer  ( ip), intent(  out) :: iptime (*)            !< particle age
      integer  ( ip), intent(inout) :: nopart                !< number of active particles
      real     ( rp), intent(in   ) :: radius (nodye)        !< help var. radius (speed)
      real     ( sp), pointer       :: xpolwaste(:,:)        !< x-coordinates of waste polygon
      real     ( sp), pointer       :: ypolwaste(:,:)        !< y-coordinates of waste polygon
      integer  ( ip), pointer       :: nrowswaste(:)         !< length of waste polygon
      integer  ( ip), intent(in   ) :: modtyp                !< for model type 2 temperature
      integer  ( ip), intent(in   ) :: lun2                  !< output report unit number
      real     ( rp), intent(in   ) :: tcktot (layt)         !< thickness hydrod.layer
      logical       , intent(in   ) :: zmodel
      integer  ( ip), intent(in   ) :: laytop(:,:)            !< highest active layer in z-layer model
      integer  ( ip), intent(in   ) :: laybot(:,:)            !< highest active layer in z-layer model
      integer  ( ip)                :: nplay  (layt)         !< work array that could as well remain inside
      integer  ( ip), intent(inout) :: laywaste (nodye)      !< layer for the dye points
      integer  ( ip), intent(in   ) :: nolay                 !< number of comp. layer
      real     ( rp), intent(inout) :: track  (8,*)          !< track array for all particles
      character( 20), intent(in   ) :: nmdyer (nodye)        !< names of the dye loads
      character( 20), intent(in   ) :: substi (nosubs)       !< names of the substances
      real     ( rp), intent(inout) :: rhopart  (nosubs,*)   !< density of the particles

      save

!     Locals

      logical        lcircl            ! determines whether load is spread over a circle
      integer(ip) :: id                ! loop variable dye loads
      integer(ip) :: iwt               ! help variable wasteload time
      integer(ip) :: ilay  , isub      ! loop variables layers and substances
      integer(ip) :: nwasth, mwasth    ! help variables for n and m of wastelocation
      integer(ip) :: laypart           !< initial layer for the particles
      real   (rp) :: xwasth, ywasth    ! help variables for x and y of wastelocation within (n,m)
      real   (rp) :: zwasth            ! help variables for z within the layer
      real   (rp) :: radiuh            ! help variable for the radius
      double precision  :: rseed = 0.5d0 ! seed for random number generation
      double precision  :: dpangle, dxp, dyp, dradius, xx, yy
      integer(ip) :: ntot              ! help variables for particles
      integer(ip) :: nulay             ! help variables for the actual layer in a particle loop
      integer(ip) :: i, ipart          ! loop/help variables for particles

      integer(4) ithndl                ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part09", ithndl )

!     loop over the number of dye releases

      write ( lun2, '(/)' )
      do id = 1, nodye
         iwt = iwtime(id)
         if ( iwt   .eq. -999 ) cycle     ! this release already happened
         if ( itime .lt. iwt  ) cycle     ! this release is for the future

!     dye release, to be activated, found

         write ( lun2, '(6x,a,a)' ) 'Instantaneous release ',nmdyer(id)
         write ( lun2, * ) ndprt(id), iwt/86400,                       &
                              mod(iwt, 86400)/3600, mod(iwt, 3600)/60,    &
                              mod(iwt, 60)
         do isub = 1, nosubs
            write ( lun2, 1010 ) substi(isub), amassd(isub,id), ' kg.'
         enddo
         iwtime(id) = -999
         if ( nwaste(id) .eq. 0 ) then
            write ( lun2, 1020 )
            cycle
         endif

!     insert the particles
         nwasth = nwaste(id)
         mwasth = mwaste(id)
         xwasth = xwaste(id)
         ywasth = ywaste(id)
         zwasth = zwaste(id)
         radiuh = radius(id)
         laypart = laywaste(id)
!     distribution in a circle ?

         lcircl = .false.
         if ( laywaste(id) .lt. 0 ) then
            lcircl = .true.
            laywaste(id) = -laywaste(id)
         endif

!     layer distribution

         if ( laywaste(id) .eq. 0 ) then          !.. uniform
            ntot = 0
            do ilay = 1, layt
               nplay(ilay) = nint(ndprt(id)*tcktot(ilay))
               ntot = ntot + nplay(ilay)
            enddo                               !.. round off in layer 1
            nplay(1) =  nplay(1) + ndprt(id) - ntot
            if ( nplay(1) .lt. 0 ) then
               write (*,*) ' Neg. dye release in top layer '
               write( lun2,*) ' Neg. dye release in top layer '
               call stop_exit(1)
            endif
         else                                   !.. for one layer only
            nplay = 0
            nplay(laywaste(id)) = ndprt(id)
         endif

!     horizontal distribution (spreaded in a circle if required

         nulay = 1
         ipart = 0
         do i = 1, ndprt(id)
            npart(nopart+i)   = 1
!            laypart(nopart+i) = 1 !2D for the moment!
            xpart(nopart+i)   = xwasth
            ypart(nopart+i)   = ywasth
            zpart(nopart+i)   = zwasth
            mpart(nopart+i)   = mwasth
!            radiuh            = 0.0

            if (radiuh.ne.-999.0) then
!              spread the particles over a circle
!               radiusr = radiuh * sqrt(rnd(rseed))
               dpangle    = 2.0D0 * pi * rnd(rseed)
!               xpart(nopart+i) = xwasth + radiuh * sin(angle)
!               ypart(nopart+i) = ywasth + radiuh * cos(angle)
! this is the code to deal with sferical models (if needed) te get the distances correct
               dradius = sqrt(rnd(rseed)) * radius(id) !noteradius is in m. 
               dxp = cos(dpangle) * dradius
               dyp = sin(dpangle) * dradius
               xpart(nopart+i) = xwasth + dxp !radius(iload)/2. * rnd(rseed)
               ypart(nopart+i) = ywasth + dyp !radius(iload)/2. * rnd(rseed)
              ! trpart(ipart) = iwtime(iload)
               if (jsferic == 1) then
                  dradius = atan2(dradius,earth_radius)*raddeg_hp !in degrees
                  dxp = cos(dpangle) * dradius ! distance in degrees
                  dyp = sin(dpangle) * dradius
! the distance is expressed in degrees (to make a circle for spherical models,
                  call Cart3Dtospher(dble(xwasth),dble(ywasth),dble(zwasth),xx,yy,ptref)
                  xx = xx + dxp
                  yy = yy + dyp
                  call sphertocart3D(xx,yy,xpart(nopart+i),ypart(nopart+i),zpart(nopart+i))
               endif
            else
               radiuh = 0
!              spread the particles over a polygon
!               !call findpoly   (nmax, mmax, lgrid, lgrid2, xp, yp, nrowswaste(id), &
!               !                 xpolwaste(1:nrowswaste(id), id), ypolwaste(1:nrowswaste(id), id), &
!               !                 xpart(i), ypart(i), npart(i), mpart(i))
            end if
!
         enddo

         !call calculate_position_in_grid(ndprt(id), xadd, yadd, nopart, xpart, ypart, zpart, mpart)

!        correct the position for particules that ended up in a dry cell or outside the grid

         !call correct_position( ndprt(i), xpart(nopart+1:), ypart(nopart+1:), npart(nopart+1:)

!     distribute the particles for this waste over the vertical

         do i = 1,ndprt(id)
            !do
            !   ipart = ipart + 1
            !   if ( ipart .gt. nplay(nulay) ) then
            !      ipart = 0
            !      nulay = nulay + 1
            !      if ( nulay .gt. nolay ) then
            !         nulay = nolay
            !         exit
            !      endif
            !endif
            !if ( nulay .gt. nolay ) then
            !   write (*,*) ' Nulay > nolay in part09 '
            !   write( lun2,*) ' Nulay > nolay in part09 '
            !   call stop_exit(1)
            !endif
            !if (zmodel) then
            !   laypart(i) = min(laybot(npart(i), mpart(i)), max(nulay,laytop(npart(i), mpart(i))))
            !else
            !   laypart(i) = nulay
            !endif
         
            !laypart(nopart + i) = nulay
!    for one layer models (2dh), the release will be in the user-defined location
            if (jsferic .ne. 1) then ! in a sferic model, the zwaste is needed for conversion of sferic to cartesian, so setting the zpart cannot .
               if ( modtyp .eq. model_oil .and. laywaste(id) .eq. 1 ) then
                  zpart(nopart+i) = zwasth
               elseif ( nolay .eq. 1 ) then
                  zpart(nopart+i) = zwasth/100.0
               else
        
!        for 3d models, the release will be distributed uniformly over the layer depth
!                                       This always gives zero due to integer division !
!                                       In part14 a random number generator is used ! (LP 2011)
        
                  !zpart(nopart+i) = (ipart-0.5)/nplay(nulay)
               endif
            endif
            
            do isub = 1, nosubs
               wpart( isub, nopart+i ) = aconc( id, isub )
               if (modtyp .eq. model_prob_dens_settling) then
                  rhopart(isub, nopart+i) = pldensity(isub)
               endif
            enddo
            iptime(nopart+i) = 0

!     store information required for Nefis files ofparticle tracks

            track(1,nopart+i) = mpart(nopart+i)
            track(2,nopart+i) = npart(nopart+i)
            track(3,nopart+i) = laywaste(id)
            track(4,nopart+i) = xpart(nopart+i)
            track(5,nopart+i) = ypart(nopart+i)
            track(6,nopart+i) = zpart(nopart+i)
            track(7,nopart+i) = itime
            track(8,nopart+i) = id

!     end of loop across the particles of this release

         enddo
         nopart = nopart + ndprt(id)

!     end of loop across dye releases

      enddo

!     end of routine

      if ( timon ) call timstop ( ithndl )
      return

!     formats

 1000 format( 10x,'No of added (released) particles: ',i8,            &
             /10x,'release time   : ',i3,'d-',i2.2,'h-',i2.2,'m-',    &
              i2.2,'s.')
 1010 format(12x,a,es15.7,a)
 1020 format( 10x,'Warning: this release is outside active area !' )
      end subroutine
end module
