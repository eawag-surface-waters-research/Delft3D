!> determine if the horizontal fluxes have to be updated (1) or not (0) from cell-based mask
subroutine get_jaupdatehorflux(nsubsteps, limtyp, jaupdate,jaupdatehorflux)
   use m_flowgeom,  only: Ndx, Lnx, ln, klnup
   use timers

   implicit none

   integer,                  intent(in)  :: nsubsteps       !< number of substeps
   integer,                  intent(in)  :: limtyp          !< limited higher-order upwind (>0) or first-order upwind (0)
   integer, dimension(Ndx),  intent(in)  :: jaupdate        !< cell updated (1) or not (0)
   integer, dimension(Lnx),  intent(out) :: jaupdatehorflux !< update horizontal flux (1) or not (0)

   integer                               :: kk, k1, k2, LL
   integer                               :: kk1L, kk2L
   integer                               :: kk1R, kk2R

   integer(4) ithndl /0/
   if (timon) call timstrt ( "get_jaupdatehorflux", ithndl )

   if ( nsubsteps.eq.1 ) then
      jaupdatehorflux = 1
   else
      jaupdatehorflux = 0
      if ( limtyp.eq.0 ) then
         do LL=1,Lnx
            k1 = ln(1,LL)
            k2 = ln(2,LL)
            if ( jaupdate(k1).eq.1 .or. jaupdate(k2).eq.1 ) then
               jaupdatehorflux(LL) = 1 ! also for diffusion
            end if
         end do
      else
         do LL=1,Lnx
            k1 = ln(1,LL)
            k2 = ln(2,LL)
            if ( jaupdate(k1).eq.1 .or. jaupdate(k2).eq.1 ) then
               jaupdatehorflux(LL) = 1 ! also for diffusion
               cycle
            end if

            kk1L = klnup(1,LL)
            if ( kk1L.ne.0 ) then
               if ( jaupdate(iabs(kk1L)).eq.1 ) then
                  jaupdatehorflux(LL) = 1
                  cycle
               end if

               if ( kk1L.gt.0 ) then
                  kk2L = klnup(2,LL)
                  if ( jaupdate(iabs(kk2L)).eq.1 ) then
                     jaupdatehorflux(LL) = 1
                     cycle
                  end if
               end if
            end if

            kk1R = klnup(4,LL)
            if ( kk1R.ne.0 ) then
               if ( jaupdate(iabs(kk1R)).eq.1 ) then
                  jaupdatehorflux(LL) = 1
                  cycle
               end if

               if ( kk1R.gt.0 ) then
                  kk2R = klnup(5,LL)
                  if ( jaupdate(iabs(kk2R)).eq.1 ) then
                     jaupdatehorflux(LL) = 1
                  end if
               end if
            end if
         end do
      end if
   end if

   if (timon) call timstop( ithndl )
   return
end subroutine get_jaupdatehorflux
