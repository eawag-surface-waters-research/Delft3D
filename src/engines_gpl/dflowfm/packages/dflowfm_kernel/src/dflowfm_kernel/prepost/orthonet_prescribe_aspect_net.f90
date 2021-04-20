!>  prescribe link-based aspect ratios in nets for mesh refinement (obsolete)
subroutine orthonet_prescribe_aspect_net(smp_mu, idir, aspect)
   use m_netw
   use m_sferic
   use m_missing
   use geometry_module, only: dbdistance, dcosphi

   IMPLICIT NONE

   double precision, dimension(numk)  :: smp_mu    !< mesh attractor

   integer                            :: idir      !< mesh adaptation direction

   double precision, dimension(numL)  :: aspect    !< aspect ratio at the links

   double precision, dimension(2)     :: orient    ! prescribed orientation


!   integer, parameter                 :: IMISS = -999999

   double precision                   :: x1,y1, x2,y2, x3,y3
   double precision                   :: R01, cosphi, cos2phi, sin2phi
   double precision                   :: A, A2, fA2, mu

   integer                            :: L, k1, k2, imin, jmin, mc, nc

   do L=1,numL
!     compute the angle of link L with the prescribed orientation
      k1 = kn(1,L)
      k2 = kn(2,L)

      if ( kc(k1).ne.1 .or. kc(k2).ne.1 ) cycle

      mu = 0.5d0*(smp_mu(k1) + smp_mu(k2))

      A   = mu
      aspect(L) = aspect(L) * A

   end do

end subroutine orthonet_prescribe_aspect_net
