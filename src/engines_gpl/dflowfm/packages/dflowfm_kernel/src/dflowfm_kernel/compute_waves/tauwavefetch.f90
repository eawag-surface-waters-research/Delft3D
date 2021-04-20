subroutine tauwavefetch(tim)               ! fetchlength and fetchdepth based significant wave height and period
 use m_sediment                             ! based on Hurdle, Stive formulae
 use m_netw                                 ! tauwave based on Swart
 use m_flowgeom                             ! taus = taubmx = taucur + tauwave, as in Delwaq
 use m_flow
 use m_waves, only: fetch, nwf, fetdp, uorb, twav, hwav
 use m_flowtimes
 use m_partitioninfo
 use unstruc_display, only: jaGUI
 USE M_OBSERVATIONS
 use geometry_module, only: getdx, getdy, dbdistance, cross, normalout, normalin
 use m_missing, only: dmiss
 use m_sferic
 use m_plotdots

 implicit none

 double precision :: tim

 double precision :: U10, fetchL, fetchd, hsig, tsig, tlim, rl, rl0, sqrt2, rk, ust, xkk1, ykk1, xkk2, ykk2
 double precision :: dir, uwin, vwin, prin, cs, sn, fetc, fetd, xn, yn, sumw, www , dsk2
 double precision :: SL,SM,XCR,YCR,CRP, alfa1, alfa2, wdep,  xzk, yzk, dist, distmin, celsiz
 double precision :: sind, cosd, ustx1, ustx2, usty1, usty2
 integer          :: k, L, kk, kkk, k1, k2, kup, n, ndone, ierr, nup, nupf, jacros, nw1, nw2, nodenum, LL, knw = 5
 INTEGER          :: NDIR, NWND, NSTAT, MOUT, ndoneprevcycle, kkmin, ndoner, k12, ks, ke, ki, msam = 0, jaopen

 integer :: ndraw
 COMMON /DRAWTHIS/ ndraw(50)

 if ( .not. allocated (fetch) .or. size (fetch,2) .ne. ndx) then
      nwf = 13

      if (  allocated (fetch) )  deallocate (fetch)
      allocate ( fetch(nwf, ndx) , stat = ierr)
      call aerr('fetch(nwf, ndx)', ierr ,  ndx*nwf)
      if (  allocated (fetdp) )  deallocate (fetdp)
      allocate ( fetdp(nwf, ndx) , stat = ierr)
      call aerr('fetdp(nwf, ndx)', ierr ,  ndx*nwf)

      ndx2dr = 0
      if (jampi == 1) then
         allocate ( fett(2, ndx) , stat = ierr)
         call aerr('fett(2, ndx)', ierr ,  ndx*2)

         do k = 1,ndxi
            if (idomain(k) .ne. my_rank) cycle
            if (kcs(k) == 2) ndx2dr = ndx2dr + 1
         end do
         call reduce_int_sum(ndx2dr,ndoner)
         ndx2dr = ndoner
      else
         do k = 1,ndxi
             if (kcs(k) == 2) ndx2dr = ndx2dr + 1
         end do
      endif

 endif

 if (tim >= time_fetch) then

      ! call mpi_barrier(DFM_COMM_DFMWORLD,ierr)

      time_fetch = max(tim, time_fetch + tifetch )
      if (tifetch == 0d0) time_fetch = 1d30

      fetch = dmiss ; fetdp = dmiss
mainloop:do n  = 1, nwf
         if (jagui > 0) then
            call cls1()
            call setcol(221)
            ! numdots = 0
         endif
         dir   = twopi *real (n-1) / real(nwf-1)
         uwin  = cos(dir) ; vwin = sin(dir)
         ndone = 0

         do k = 1,ndxi
            if (kcs(k) .ne. 2) cycle
            kkmin = 0 ; distmin = 1d10; celsiz = 0d0
            if (jampi == 1) then
               if (idomain(k) .ne. my_rank) cycle
            endif
            do kk = 1,netcell(k)%n
               L  = netcell(k)%lin(kk)
               k1 = netcell(k)%nod(kk)
               if (kk == netcell(k)%n) then
                  k2 = netcell(k)%nod(1)
               else
                  k2 = netcell(k)%nod(kk+1)
               endif
               celsiz = max(celsiz, dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss) )
            enddo
            if (jsferic == 1) celsiz=celsiz*rd2dg/ra

            jaopen = 0
            do kk = 1,nd(k)%lnx
               L  = iabs( nd(k)%ln(kk) )
               if (ln(1,L) > ndxi) then
                  jaopen = 1
               endif
            enddo

            do kk = 1,netcell(k)%n
               L  = netcell(k)%lin(kk)
               k1 = netcell(k)%nod(kk)
               if (kk == netcell(k)%n) then
                  k2 = netcell(k)%nod(1)
               else
                  k2 = netcell(k)%nod(kk+1)
               endif

               wdep   = s1(k) - min(zk(k1),zk(k2))
               if (lnn(L) == 1 .or.  wdep < 0.5d0 .or. kn(3,L) == 0 .or. jaopen == 1) then    ! link shallow or closed => start fetch here
                  call normalout(xk(k1), yk(k1), xk(k2), yk(k2), xn, yn, jsferic, jasfer3D, dmiss, dxymis)
                  prin = uwin*xn + vwin*yn
                  if ( prin < 0d0 ) then                   ! if upwind
                     crp  = xn ; xn  = -yn ; yn = crp
                     crp  = 0d0
                     xkk1 = xk(k1) - 2*celsiz*xn
                     ykk1 = yk(k1) - 2*celsiz*yn
                     xkk2 = xk(k2) + 2*celsiz*xn
                     ykk2 = yk(k2) + 2*celsiz*yn
                     CALL CROSS(Xkk1,Ykk1,Xkk2,Ykk2,Xzw(k),Yzw(k),Xzw(k)-1d4*uwin,Yzw(k)-1d4*vwin, &
                                JACROS,SL,SM,XCR,YCR,CRP,jsferic, dmiss)
                     if (jacros == 1) then
                        dist = dbdistance(xz(k), yz(k), xcr, ycr, jsferic, jasfer3D, dmiss)
                        if (dist < distmin) then
                           distmin = dist ; kkmin = kk        ! closest crossed upwind edge
                        endif
                     endif
                  endif
               endif
            enddo
            if (kkmin > 0) then
                if (jaopen == 1) then
                   fetch(n,k) = 1d5
                else
                   fetch(n,k) = min(distmin, celsiz)
                endif
                fetdp(n,k) = max( s1(k) - bl(k), .1d0)
                if (jagui > 0) then
                   !CALL rCIRc(Xz(k),Yz(k) ) !, fetch(n,k))
                   !call adddot(Xz(k),Yz(k),1d0)
                endif
                ndone      = ndone + 1
            endif

         enddo

         if (jampi == 1) then
             call reducefett(n)
             call reduce_int_sum(ndone,ndoner)
             ndone = ndoner
         endif

         if (jagui > 0) call setcol(31)
         do while ( ndone < ndx2dr )

            ndoneprevcycle = ndone
            ndone          = 0

555         continue
            do k = 1,ndxi
               if (kcs(k) .ne. 2) cycle
               if (jampi == 1) then
                  if (idomain(k) .ne. my_rank) cycle
               endif

               if (fetch(n,k) .eq. dmiss) then
                  kup = 0 ; fetc = 0; fetd = 0; sumw = 0; nup = 0; nupf = 0
                  do kk = 1,nd(k)%lnx
                     L  = iabs( nd(k)%ln(kk) )
                     k2 = ln(1,L) ; if (k2 == k) k2 = ln(2,L)
                     if ( kcs(k2) == 2 ) then  ! internal
                        !prin = uwin*getdx(xz(k2),yz(k2),xz(k),yz(k), jsferic) + vwin*getdy( xz(k2),yz(k2),xz(k),yz(k), jsferic)
                        !dsk2 = dbdistance(xz(k2),yz(k2),xz(k),yz(k), jsferic, jasfer3D, dmiss)
                        !cs   = min(max(prin/dsk2,-1d0),1d0)

                        cs   = uwin*csu(L) + vwin*snu(L)
                        if (L .ne. nd(k)%ln(kk) ) cs = -1d0*cs
                        dsk2 = dx(L)
                        prin = dsk2*cs

                        if (cs > 0) then ! internal upwind points
                           nup = nup + 1
                           if (fetch(n,k2) .ne. dmiss) then ! do not look at open boundaries
                               nupf = nupf + 1
                               sn   = sqrt( 1d0 - cs*cs)
                               ! www  = (1d0-sn)/dsk2               ! first attempt
                               www  = (cs   + 0.05d0*sn)*wu(L)/dsk2 ! some diffusion
                               fetc = fetc  + www*(fetch(n,k2) + prin)
                               fetd = fetd  + www*(fetch(n,k2) + prin)*max(.1d0, 0.8d0*fetdp(n,k2) + 0.2d0*(s1(k)-bl(k)) )
                               sumw = sumw  + www
                           endif
                        endif
                     endif
                  enddo
                  if ( nup == nupf .and. sumw > 0d0) then
                     fetch(n,k) = fetc/sumw
                     fetdp(n,k) = fetd/ ( sumw*fetch(n,k) )
                     ndone      = ndone + 1
                     if (jagui > 0) then
                        !CALL rCIRc(Xz(k),Yz(k) )
                        !call adddot(Xz(k),Yz(k),2d0)
                         call KCIR(Xz(k),Yz(k),1d0)
                     end if
                  endif
               else
                  ndone = ndone + 1
               endif
            enddo ! k

            if (jampi == 1) then
               call reducefett(n)
               call reduce_int_sum(ndone,ndoner)
               ndone = ndoner
            endif

            if ( ndone.eq.ndoneprevcycle ) then
               call QNERROR('connectivity issue in fetch', ' ', ' ')
               exit mainloop
            end if

         enddo

    enddo mainloop

 endif

 sqrt2 = sqrt(2d0)

 do k = 1,ndx2d
    Hwav(k)   = 0d0
    Twav(k)   = 0d0
    Uorb(k)   = 0d0
    rlabda(k) = 0d0
    ustk(k)   = 0d0

    if ( hs(k) > 0.01d0 ) then

       call getfetch(k,U10,FetchL,FetchD)
       if (FetchL > 0) then

          if (jawave == 1) then

             call hurdlestive (U10, fetchL, fetchD, Hsig, Tsig)

          else if (jawave == 2) then

             call ian_young_pt(U10, fetchL, fetchD, Hsig, Tsig)

          endif

          Hwav(k) = Hsig / sqrt2          ! Hwav === hrms
          Twav(k) = Tsig
          call tauwavehk(Hwav(k), Twav(k), hs(k), Uorb(k), rlabda(k), ustk(k))      ! basically now just a dispersion function with 2DH stokes drift
       endif
    endif

    if (NDRAW(28) == 35) then
       plotlin(k) = fetchL
    else if (NDRAW(28) == 36) then
       plotlin(k) = fetchD
    else if (NDRAW(28) == 37) then
       plotlin(k) = Hsig
    else if (NDRAW(28) == 38) then
       plotlin(k) = Tsig
    else if (NDRAW(28) == 39) then
       !  plotlin(k) = Taucur
    else if (NDRAW(28) == 40) then
       plotlin(k) = uorb(k)
    endif

 enddo

 ! need something for 2D ustokes
 do LL = 1, lnx
   if (hu(LL)>epswav) then
       k1 = ln(1,LL); k2 = ln(2,LL)
       dir   = atan2(wy(LL), wx(LL))
       sind  = sin(dir); cosd = cos(dir)
       ustx1 = ustk(k1)*cosd
       ustx2 = ustk(k2)*cosd
       usty1 = ustk(k1)*sind
       usty2 = ustk(k2)*sind
       ustokes(LL) =      acL(LL) *( csu(LL)*ustx1 + snu(LL)*usty1) + &
                     (1d0-acL(LL))*( csu(LL)*ustx2 + snu(LL)*usty2)
       vstokes(LL) =      acL(LL) *(-snu(LL)*ustx1 + csu(LL)*usty1) + &
                     (1d0-acL(LL))*(-snu(LL)*ustx2 + csu(LL)*usty2)
    else
       ustokes(LL) = 0d0
       vstokes(LL) = 0d0
    endif
 end do

end subroutine tauwavefetch
