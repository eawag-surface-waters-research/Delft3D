   subroutine wave_uorbrlabda()
   use m_waves, only: uorb, wlenwav, uorbwav, twav, hwav, hwavcom, gammax, rlabda, jauorb, jauorbfromswan
   use m_flow, only: hs
   use m_flowgeom, only: ndx
   use m_physcoef, only: ag
   use m_sferic, only: pi
   use m_flowtimes, only: time1

   implicit none

   integer                            :: k
   integer                            :: uorbwav_from_SWAN=0
   integer                            :: wlenwav_from_SWAN=0

   double precision                   :: hss, per, omeg, k0, k0h, rk, uorb1

   do k = 1,ndx
      hss = max(0.01, hs(k))
      per = max(0.01, twav(k))                   ! wave period

      hwav(k) = min(hwavcom(k), gammax*hs(k))       ! Prevent unrealistic Hrms in shallow water. Use original comfile value again every time, as hs changes per dts
      omeg       = 2.0*pi/per
      k0         = omeg*omeg/ag
      k0h        = k0*hss
      if (k0h>pi) then                ! if deep water
         rk = k0
      elseif (k0h<0.005) then         ! if very shallow water
         rk = omeg/sqrt(ag*hss)
      else
         call getwavenr(hss,per,rk)
      endif
      if (wlenwav_from_SWAN.eq.1) then
         rlabda(k) = wlenwav(k)
      else
         rlabda(k) = 2.0*pi/rk
      endif
      if (rk*hss<80d0) then            ! if not very deep water
         if (jauorbfromswan.eq.1) then
            Uorb(k)    = uorbwav(k)
         else
            Uorb(k)      = 0.5d0*hwav(k)*omeg/sinh(rk*hss)
            !Uorb(k)    = uorb1*sqrt(pi)/2d0                            ! See note Dano on orbital velocities in D3D, SWAN and XBeach
            if (jauorb==0) then       ! old d3d convention
               uorb(k) = uorb(k)*sqrt(pi)/2d0    ! only on hrms derived value, not on SWAN read uorb
            end if
         endif
      else
         Uorb(k) = 0d0
      endif
   enddo

   end subroutine wave_uorbrlabda
