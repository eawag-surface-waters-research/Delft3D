   subroutine setucxqucyq_mor (u1, ucxq, ucyq)
   use m_fm_erosed, only: ucxq_mor, ucyq_mor, hs_mor, link1, link1sign
   use m_flowgeom, only: ndx, lnx, lnxi, ln, nd, wcx1, wcx2, wcy1, wcy2, csu, snu, bl, ndxi, lnx1D, kcs
   use m_flow, only: hs, hu, zws, kmx, kmxL, au, q1, ucx_mor, ucy_mor
   use m_flowparameters ,only: jacstbnd, epshs, eps10
   use m_sediment, only: stmpar
   use m_turbulence, only:ln0
   use m_CrossSections, only: GetCSParsFlow
   use unstruc_channel_flow, only: network

   implicit none
   double precision, dimension(lnx), intent(in ) :: u1
   double precision, dimension(ndx), intent(in ) :: ucxq
   double precision, dimension(ndx), intent(in ) :: ucyq
   integer          :: L, LL, k, k1, k2, Lt, Lb, kk, kb, kt
   double precision :: wcxu, wcyu, cs, sn, uin, huL
   logical, pointer :: maximumwaterdepth
   double precision, dimension(:), allocatable :: area
   integer                                     :: qsign
   double precision :: area_L
   double precision :: width_L
   double precision :: perim_L
   integer          :: nstruc

   maximumwaterdepth => stmpar%morpar%mornum%maximumwaterdepth

   allocate(area(ndx))

   do k = 1,ndx
       hs_mor(k) = hs(k)
       if (kcs(k) == 1) then
           ucxq_mor(k) = 0d0
           ucyq_mor(k) = 0d0
           area(k)= 0d0
       else
           ucxq_mor(k) = ucxq(k)
           ucyq_mor(k) = ucyq(k)
       endif
   enddo

   ! we define the node as the begin/end point of the first link connected to it
   if (stmpar%morpar%mornum%pure1d) then
   do L = 1,lnx1D
       k1 = ln(1,L)
       k2 = ln(2,L)
       nstruc = network%adm%lin2str(L)
       if (kcs(k1) == 1) then ! link pointing away from the node
           if (link1(k1) == L) then
               qsign = 1
           else
               qsign = link1sign(k1)
           endif
           ! call getprof_1D(L, hu(L), au(L), widu, japerim, calcConv, perim)
           ucxq_mor(k1) = ucxq_mor(k1) + qsign * q1(L)
           if (.true.) then !nstruc > 0) then
              ! link has structure
              ! see getprof_1D: if this is not a boundary link, we can use L to index line2cross.
              call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, hs(k1), area_L, perim_L, width_L)
           else
              ! no structure on link
              area_L = au(L)
           endif
           area(k1)     = area(k1) + area_L
       endif
       if (kcs(k2) == 1) then ! link pointing towards the node
           if (link1(k2) == L) then
               qsign = 1
           else
               qsign = -link1sign(k2)
           endif
           ucxq_mor(k2) = ucxq_mor(k2) + qsign * q1(L)
           if (.true.) then !nstruc > 0) then ! link has structure?
              ! link has structure
              ! see getprof_1D: if this is not a boundary link, we can use L to index line2cross.
              call GetCSParsFlow(network%adm%line2cross(L, 2), network%crs%cross, hs(k2), area_L, perim_L, width_L)
           else
              ! no structure on link
              area_L = au(L)
           endif
           area(k2)     = area(k2) + area_L
       endif
   enddo

   do k = 1,ndx
       if (kcs(k) == 1) then
           ucxq_mor(k) = ucxq_mor(k)/area(k)
           ucyq_mor(k) = 0d0
       else
           ucxq_mor(k) = ucxq(k)
           ucyq_mor(k) = ucyq(k)
       endif
   enddo

   deallocate(area)

   else
   ucxq_mor = 0d0 ; ucyq_mor = 0d0; hs_mor = 0d0

   if( .not. maximumwaterdepth ) then
      if (kmx<1) then
         do k = 1,ndx
            ucxq_mor(k) = ucxq(k)
            ucyq_mor(k) = ucyq(k)
            hs_mor(k)   = hs(k)
         enddo
      else
         do k=1,ndx
            ucxq_mor(k) = ucxq(k)   ! depth-averaged values
            ucyq_mor(k) = ucyq(k)
            hs_mor(k)    = hs(k)
            call getkbotktop(k,kb,kt)
            do kk=kb,kt
               ucxq_mor(kk) = ucxq(kk)
               ucyq_mor(kk) = ucyq(kk)
            enddo
         enddo
      endif
      return
   endif

   if (kmx<1) then
      do L = 1,lnx
         if (u1(L) == 0d0) cycle
         k1 = ln(1,L) ; k2 = ln(2,L)
         wcxu = wcx1(L)*u1(L)
         ucxq_mor (k1) = ucxq_mor(k1) + wcxu*hu(L)
         wcyu = wcy1(L)*u1(L)
         ucyq_mor (k1) = ucyq_mor(k1) + wcyu*hu(L)
         wcxu = wcx2(L)*u1(L)
         ucxq_mor (k2) = ucxq_mor(k2) + wcxu*hu(L)
         wcyu = wcy2(L)*u1(L)
         ucyq_mor (k2) = ucyq_mor(k2) + wcyu*hu(L)
      enddo

      do L = lnxi+1,lnx
         k1 = ln(1,L) ; k2 = ln(2,L)
         cs = csu(L) ; sn = snu(L)
         if ( jacstbnd == 0 ) then
            uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
            ucxq_mor(k1) = uin * cs
            ucyq_mor(k1) = uin * sn
            bl(k2) = bl(k1)
         else
            ucxq_mor(k1) = ucxq_mor(k2)
            ucyq_mor(k1) = ucyq_mor(k2)
         end if
      enddo

      do k = 1,ndx
         hs_mor(k) = hs(k)
         do L = 1,nd(k)%lnx
            LL = abs( nd(k)%ln(L) )
            hs_mor(k) = max( hs_mor(k), hu(LL) )
         enddo
      enddo

      do k = 1,ndx
         if( hs_mor(k) > epshs) then
            ucxq_mor(k) = ucxq_mor(k) / hs_mor(k)
            ucyq_mor(k) = ucyq_mor(k) / hs_mor(k)
         else
            ucxq_mor(k) = 0d0
            ucyq_mor(k) = 0d0
         endif
      enddo
   else
      do LL = 1,lnx
         if (u1(LL)<eps10) cycle
         call getLbotLtop(LL,lb,Lt)
         !Lb = Lbot(LL) ; Lt = Lb - 1 + kmxL(LL)
         do L = Lb, Lt
            k1 = ln0(1,L); k2 = ln0(2,L)
            huL = hu(L)
            if (L>Lb) then
               huL   = huL - hu(L-1)
            endif
            ucxq_mor(k1) = ucxq_mor(k1) + wcx1(LL)*u1(L)*huL
            ucyq_mor(k1) = ucyq_mor(k1) + wcy1(LL)*u1(L)*huL
            ucxq_mor(k2) = ucxq_mor(k2) + wcx2(LL)*u1(L)*huL
            ucyq_mor(k2) = ucyq_mor(k2) + wcy2(LL)*u1(L)*huL
         enddo
      enddo

      do L = lnxi+1,lnx
         cs = csu(L) ; sn = snu(L)
         call getLbotLtop(L,Lb,Lt)
         do LL=Lb,Lt
            k1 = ln0(1,LL) ; k2 = ln0(2,LL)
            if ( jacstbnd == 0 ) then
               uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
               ucxq_mor(k1) = uin * cs
               ucyq_mor(k1) = uin * sn
            else
               ucxq_mor(k1) = ucxq_mor(k2)
               ucyq_mor(k1) = ucyq_mor(k2)
            end if
         enddo
      enddo
      !
      do k = 1,ndx
         hs_mor(k) = hs(k)
         do L = 1,nd(k)%lnx
            LL = abs( nd(k)%ln(L) )
            hs_mor(k) = max( hs_mor(k), hu(LL) )
         enddo
         !
         call getkbotktop(k,kb,kt)
         do kk=kb,kt
            hs_mor(kk) = zws(kk)-zws(kk-1)
            do L = 1,nd(kk)%lnx
               LL = abs( nd(kk)%ln(L) )
               hs_mor(kk) = max( hs_mor(kk), hu(LL)-hu(LL-1) )
            enddo
         enddo
      enddo
      !
      do k = 1,ndxi
        if (hs_mor(k) > eps10)  then
           call getkbotktop(k,kb,kt)
           ucxq_mor(k) = sum(ucxq_mor(kb:kt)) / hs_mor(k)
           ucyq_mor(k) = sum(ucyq_mor(kb:kt)) / hs_mor(k)
           do kk = kb,kt
              if (hs_mor(kk) > eps10) then
                 ucxq_mor(kk) = ucxq_mor(k)/hs_mor(kk)
                 ucyq_mor(kk) = ucyq_mor(k)/hs_mor(kk)
              endif
         enddo
        endif
      enddo
   endif
   endif
   end subroutine setucxqucyq_mor
