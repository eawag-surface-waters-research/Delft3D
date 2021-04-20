   subroutine fm_mor_maxtimestep()
   use m_flowtimes
   use m_flow
   use m_flowgeom
   use m_sediment
   use m_partitioninfo
   use m_fm_erosed, only: sxtot, sytot, cdryb, morfac, lsedtot

   implicit none

   integer           :: k, k1, k2, kk, L, ised, ac1, ac2
   double precision  :: dum, sx, sy, sL, dt, dtmaxmor, dhmax

   dtmaxmor = huge(0d0)

   do k = 1, ndx
      dum = 0.d0
      if (kcsmor(k)==0) then
         cycle
      endif

      do ised = 1, lsedtot
         !
         do kk = 1, nd(k)%lnx
            L = iabs(nd(k)%ln(kk))
            k1 = ln(1,L)
            k2 = ln(2,L)
            ac1 = acl(L)
            ac2 = 1d0-ac1

            sx = (ac1*sxtot(k1,ised) + ac2*sxtot(k2,ised))/cdryb(ised)*max(morfac,1d0)
            sy = (ac1*sytot(k1,ised) + ac2*sytot(k2,ised))/cdryb(ised)*max(morfac,1d0)
            sL = csu(L)*sx + snu(L)*sy

            if (k2 .eq. k) sL = -sL

            if (sL .ge. 0.) then        ! outgoing transport fluxes only
               dum = dum + sL*wu(L)
            end if
         end do
      end do
      if (dum > tiny(0d0)) then
         dt = dzbdtmax*ba(k) / max(dum,eps10)   ! safety
         if ( dt.lt.dtmaxmor ) then
            dtmaxmor = dt
         end if
      end if
   end do

   if ( jampi.eq.1 ) then
      call reduce_double_min(dtmaxmor)
   end if

   if (dtmaxmor > dts) dtmaxmor = dts
   dtmaxmor = dts/ceiling(dts/dtmaxmor)
   !
   dts = dtmaxmor
   dti = 1d0/dts

   end subroutine fm_mor_maxtimestep
