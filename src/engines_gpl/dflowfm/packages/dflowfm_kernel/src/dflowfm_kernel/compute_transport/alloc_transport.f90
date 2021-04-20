!> allocate transport arrays
subroutine alloc_transport(Keepexisting)
   use m_flowgeom, only: Ndx, Lnx
   use m_flow, only: Lnkx, Ndkx, kmx, sigdifi, wsf
   use m_fm_wq_processes
   use m_transport
   use m_alloc
   use m_meteo, only: numtracers, numfracs
   use m_flowexternalforcings, only: numsrc, qcsrc, vcsrc, wstracers
   use m_sediment, only: stm_included
   implicit none

   logical, intent(in) :: KeepExisting    !< keep existing data (true) or not (false)

!  allocate and initialize fluxes

   call realloc(fluxhor, (/ NUMCONST, Lnkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(fluxver, (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)

   call realloc(fluxhortot, (/ NUMCONST, Lnkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(sinksetot,  (/ NUMCONST, Ndx /),  keepExisting=KeepExisting, fill=0d0)
   call realloc(sinkftot,   (/ NUMCONST, Ndx /),  keepExisting=KeepExisting, fill=0d0)

   call realloc(difsedu, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(difsedw, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(sigdifi, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(wsf, NUMCONST, keepExisting=.true., fill=0d0)

   call realloc(constituents, (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)

   call realloc(const_sour  , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(const_sink  , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)

   call realloc(dsedx       , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)
   call realloc(dsedy       , (/ NUMCONST, Ndkx /), keepExisting=KeepExisting, fill=0d0)

   call realloc(thetavert, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   !call realloc(wstracers, NUMCONST, keepExisting=KeepExisting, fill=0d0)
   call realloc(wstracers, NUMCONST, keepExisting=.true., fill=0d0)

   call realloc(const_names, NUMCONST, keepExisting=KeepExisting, fill='')
   call realloc(const_units, NUMCONST, keepExisting=KeepExisting, fill='')

   call realloc(id_const, (/ 2, NUMCONST /), keepExisting=KeepExisting, fill = 0)

   call realloc(sumhorflux, (/ NUMCONST, Ndkx /), keepExisting=.false., fill=0d0)
   call realloc(ndeltasteps, Ndx, keepExisting=.false., fill=1)
   call realloc(jaupdate,    Ndx, keepExisting=.false., fill=1)
   call realloc(jaupdatehorflux, Lnx, keepExisting=.false., fill=1)
   call realloc(dtmax,       Ndx, keepExisting=.false., fill=0d0)

   if (stm_included) then
      call realloc(u1sed, Lnkx, keepExisting=.false., fill=0d0)
      call realloc(q1sed, Lnkx, keepExisting=.false., fill=0d0)
   endif

   if ( jalimitdtdiff.eq.1 ) then
      call realloc(sumdifflim, Ndkx, keepExisting=.false., fill = 0d0)
   end if
   call realloc(dxiAu, Lnkx, keepExisting=.false., fill = 0d0)

   call realloc(jaupdateconst, NUMCONST, keepExisting=.false., fill=1)
   if ( stm_included ) then
      call realloc(noupdateconst, NUMCONST, keepExisting=.false., fill=0)
   end if

!  work arrays
   if ( allocated(rhs) ) deallocate(rhs)
   allocate(rhs(NUMCONST,Ndkx))

   if ( kmx.gt.0 ) then ! 3D
      if ( allocated(a) ) deallocate(a,b,c,d,e,sol)
      allocate(a(kmx,NUMCONST),b(kmx,NUMCONST),c(kmx,NUMCONST),d(kmx,NUMCONST),e(kmx),sol(kmx))
      a = 0d0
      b = 0d0
      c = 0d0
      d = 0d0
      e = 0d0
   end if

!  tracer boundary condition
   call realloc(itrac2const, numtracers, keepExisting=KeepExisting, fill=0)
   call realloc(ifrac2const, numfracs, keepExisting=KeepExisting, fill=0)

   call realloc(qcsrc, (/   NUMCONST, numsrc /), keepExisting=.false., fill=0d0)
   call realloc(vcsrc, (/ 2*NUMCONST, numsrc /), keepExisting=.false., fill=0d0)

   if ( jawaqproc > 0 ) then
!     WAQ
      call realloc(isys2const,  notot, keepExisting=.true., fill=0)
   end if
   call realloc(iconst2sys,  NUMCONST, keepExisting=.true., fill=0)
   return
end subroutine alloc_transport
