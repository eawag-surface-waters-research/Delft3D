   subroutine duneaval(error)
   use m_fm_erosed
   use m_sediment
   use m_flowgeom
   use m_flow
   use message_module

   implicit none

   logical,                                      intent(out)   :: error

   integer                    :: ierr
   integer                    :: k1, k2, L, lsd, ac1, ac2
   double precision           :: slp, slpmax, avflux, maxflux

   error = .true.
   avalflux = 0d0

   do lsd = 1, lsedtot
      if (sedtyp(lsd) == SEDTYP_COHESIVE) cycle
      do L = 1, lnx
         if (wu_mor(L)==0d0) cycle
         k1 = ln(1,L); k2 = ln(2,L)
         ac1 = acL(L); ac2=1d0-ac1
         if (hs(k1)>hswitch .or. hs(k2)> hswitch) then
            slpmax = wetslope
         else
            slpmax = dryslope
         end if
         !
         slp = sqrt(e_dzdn(L)*e_dzdn(L)+e_dzdt(L)*e_dzdt(L))
         if (slp>slpmax) then
            avflux = ba(k1)*ba(k2)/(ba(k1)+ba(k2)) * (bl(k2)-bl(k1) + slpmax*e_dzdn(L)/slp*Dx(L)) * (ac1*frac(k1,lsd) + ac2*frac(k2,lsd)) / avaltime / max(morfac, 1d0)

            maxflux= ba(k1)*ba(k2)/(ba(k1)+ba(k2)) * dzmaxdune / max(morfac,1d0)

            if (abs(maxflux) < abs(avflux)) then
               if (avflux > 0 ) then
                  avflux = min(avflux , maxflux)
               else
                  avflux = max(avflux,-maxflux)
               end if
            endif

            avalflux(L, lsd) = avalflux(L,lsd) - avflux*rhosol(lsd)/wu_mor(L)
         end if
      end do
   end do
   !
   error = .false.
1234 continue
   end subroutine duneaval
