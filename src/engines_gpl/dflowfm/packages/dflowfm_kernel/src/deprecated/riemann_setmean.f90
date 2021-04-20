!> compute riemann boundary reference state
   subroutine riemann_setmean()
      use m_flow, only: s1, u1
      use m_flowgeom, only: bl
      use m_flowexternalforcings
      use m_flowtimes, only: dts
      use m_physcoef, only: ag
      implicit none

      double precision :: dfac, dfac1
      double precision :: h

      integer          :: n, kb, k2, L, itpbn

      double precision :: Tref

      if ( nbndz.gt.0 ) then
         do n=1,nbndz
            itpbn   = kbndz(4,n)
            Tref    = dble(kbndz(6,n))  ! integer, but can get away with it, nobody uses fractional seconds..
            dfac  = max(min(dts/Tref, 1d0), 0d0)
            dfac1 = 1d0 - dfac

            if ( itpbn.eq.5 ) then
               kb      = kbndz(1,n)
               k2      = kbndz(2,n)
               L       = kbndz(3,n)

               k2 = kbndz(2,n)

               h = s1(k2) - bl(k2)
               zbndz0(n) = dfac1*zbndz0(n) + dfac*(s1(k2) - sqrt(h/ag)*u1(L))
            end if
         end do
      end if

      return
   end subroutine riemann_setmean
