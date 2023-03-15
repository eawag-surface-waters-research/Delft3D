   module m_set_HO_advec_velocity
   contains
   
   elemental subroutine setvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   use m_sferic
   use m_flowtimes
   use unstruc_messages
   use m_nod2lin
   use m_flowgeom
   use m_limiters, only: dslim, dslimvec
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb
   double precision, intent(inout) :: ucxu, ucyu
   integer, intent(in)             :: k, kd, is, n12, ib, kku, ku2, ku
   double precision, intent(in)    :: half, sl1, sl2, sl3

   double precision           :: cf, ucxku, ucyku,  ds1x, ds1y, ds2x, ds2y, dsx, dsy

   if (limtypmom == 6) then
      ds1x = (ducxdx(k)*csu(LL) + ducxdy(k)*snu(LL)) * is * Dx(LL)
      ds1y = (ducydx(k)*csu(LL) + ducydy(k)*snu(LL)) * is * Dx(LL)
   else
      if (kku < 0) then
         ucxku = ucx(ku)
         ucyku = ucy(ku)
      else
         ucxku = ucx(ku)*sl1 + ucx(ku2)*sl2
         ucyku = ucy(ku)*sl1 + ucy(ku2)*sl2
      endif
      ds1x = (ucx(k)  - ucxku)*sl3
      ds1y = (ucy(k)  - ucyku)*sl3
   endif

   cf   =  dts*abs(u1(L))*dxi(LL)
   cf  =  half*max( 0d0,1d0-cf )

   ds2x =  ucx(kd) - ucx(k)
   ds2y =  ucy(kd) - ucy(k)

   call dslimvec(ds1x, ds1y, ds2x, ds2y, csu(L), snu(L), limtypmom, dsx, dsy)
   ucxu = ucxu + cf*dsx
   ucyu = ucyu + cf*dsy

   end subroutine

   elemental subroutine setvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   use m_sferic
   use m_flowtimes
   use unstruc_messages
   use m_nod2lin
   use m_flowgeom
   use m_limiters, only: dslim, dslimvec
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb
   double precision, intent(inout) :: ucxu, ucyu
   integer, intent(in)             :: k, kd, is, n12, ib, kku, ku2, ku
   double precision, intent(in)    :: half, sl1, sl2, sl3

   double precision           :: cf, ucxku, ucyku,  ds1x, ds1y, ds2x, ds2y, dsx, dsy

   if (limtypmom == 6) then
      ds1x = (nod2linx(LL,n12,ducxdx(k),ducxdy(k))*csu(LL) + nod2liny(LL,n12,ducxdx(k),ducxdy(k))*snu(LL)) * is * Dx(LL)
      ds1y = (nod2linx(LL,n12,ducydx(k),ducydy(k))*csu(LL) + nod2liny(LL,n12,ducydx(k),ducydy(k))*snu(LL)) * is * Dx(LL)
   else
      if (kku < 0) then
         ucxku = nodup2linx(LL,1+ib,ucx(ku),ucy(ku))
         ucyku = nodup2liny(LL,1+ib,ucx(ku),ucy(ku))
      else
         ucxku = nodup2linx(LL,1+ib,ucx(ku) ,ucy(ku ))*sl1 + &
            nodup2linx(LL,2+ib,ucx(ku2),ucy(ku2))*sl2
         ucyku = nodup2liny(LL,1+ib,ucx(ku) ,ucy(ku ))*sl1 + &
            nodup2liny(LL,2+ib,ucx(ku2),ucy(ku2))*sl2
      endif
      ds1x = (nod2linx(LL,n12,ucx(k),ucy(k))  - ucxku)*sl3
      ds1y = (nod2liny(LL,n12,ucx(k),ucy(k))  - ucyku)*sl3
   endif

   cf   =  dts*abs(u1(L))*dxi(LL)
   cf  =  half*max( 0d0,1d0-cf )

   ds2x =  nod2linx(LL,3-n12,ucx(kd),ucy(kd)) - nod2linx(LL,n12,ucx(k),ucy(k))
   ds2y =  nod2liny(LL,3-n12,ucx(kd),ucy(kd)) - nod2liny(LL,n12,ucx(k),ucy(k))

   call dslimvec(ds1x, ds1y, ds2x, ds2y, csu(L), snu(L), limtypmom, dsx, dsy)
   ucxu = ucxu + cf*dsx
   ucyu = ucyu + cf*dsy

   end subroutine
   
   elemental subroutine setvelocity_neg(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   !use m_sferic, only: jasfer3D
   implicit none

   integer, intent(in)             :: L, LL, Lb
   double precision, intent(inout) :: ucxu, ucyu

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)
   !      use klnup to check for disabled higher-order correction
   if (limtypmom == 6 .and. klnup(1,LL).eq.0) return
   !   <-      ds2   ds1
   k = k2 ; kd = k1 ; is = -1 ; half = 1d0-acl(LL)   ;          !   <-   kd     k     ku
   n12 = 2
   ib  = 2

   sl1  =       slnup(4,LL)
   sl2  =       slnup(5,LL)
   sl3  =       slnup(6,LL)

   kku  =     klnup(4,LL)
   ku2  = abs(klnup(5,LL))
   ku   = abs(kku)

   if (kmx /= 0) then !different ku for 3D
      kkub = ku2
      ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) ;
      if (ku < kbot(kkub) .or. ku > ktop(kkub) ) return
   endif

   if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd .or. kku == 0 .or. (kku > 0 .and. ku2 == 0)) return

   call setvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine
   
   elemental subroutine setvelocity_pos(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   !use m_sferic, only: Jasfer3D
   implicit none

   integer, intent(in)             :: L, LL, Lb
   double precision, intent(inout) :: ucxu, ucyu

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)

   !      use klnup to check for disabled higher-order correction
   if (limtypmom == 6 .and. klnup(1,LL).eq.0) return
   !   ->      ds1   ds2
   k = k1 ; kd = k2 ; is =  1 ; half = acl(LL)       ;          !   ->   ku     k     kd
   n12 = 1
   ib  = 0

   sl1  =       slnup(1,LL)
   sl2  =       slnup(2,LL)
   sl3  =       slnup(3,LL)

   kku  =     klnup(1,LL)
   ku2  = abs(klnup(2,LL))
   ku   = abs(kku)

   if (kmx /= 0) then !different ku for 3D
      kkub = ku2
      ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) ;
      if (ku < kbot(kkub) .or. ku > ktop(kkub) ) return
   endif

   if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd .or. kku == 0 .or. (kku >= 0 .and. ku2 == 0)) return

   call setvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)
   
   !if(jasfer3D == 0) then
   !   call setvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)
   !else
   !   call setvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)
   !endif

   end subroutine

   elemental subroutine setvelocity_neg_jasfer3D(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb
   double precision, intent(inout) :: ucxu, ucyu

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)

   !      use klnup to check for disabled higher-order correction
   if (limtypmom == 6 .and. klnup(1,LL).eq.0) return
                                                                !   <-      ds2   ds1
   k = k2 ; kd = k1 ; is = -1 ; half = 1d0-acl(LL)   ;          !   <-   kd     k     ku
   n12 = 2
   ib  = 2
   sl1  =       slnup(4,LL)
   sl2  =       slnup(5,LL)
   sl3  =       slnup(6,LL)
   kku  =     klnup(4,LL)
   ku2  = abs(klnup(5,LL))
   ku   = abs(kku)

   if (kmx /= 0) then !different ku for 3D
      kkub = ku2
      ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) ;
      if (ku < kbot(kkub) .or. ku > ktop(kkub) ) return
   endif
   if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd .or. kku == 0 .or. (kku > 0 .and. ku2 == 0)) return

   call setvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine

   elemental subroutine setvelocity_pos_jasfer3D(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1

   implicit none

   integer, intent(in)             :: L, LL, Lb
   double precision, intent(inout) :: ucxu, ucyu

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)

   !      use klnup to check for disabled higher-order correction
   if (limtypmom == 6 .and. klnup(1,LL).eq.0) return
                                                                !   <-      ds2   ds1
   k = k1 ; kd = k2 ; is =  1 ; half = acl(LL)       ;          !   ->   ku     k     kd
   n12 = 1
   ib  = 0

   sl1  =       slnup(1,LL)
   sl2  =       slnup(2,LL)
   sl3  =       slnup(3,LL)

   kku  =     klnup(1,LL)
   ku2  = abs(klnup(2,LL))
   ku   = abs(kku)

   if (kmx /= 0) then !different ku for 3D
      kkub = ku2
      ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) ;
      if (ku < kbot(kkub) .or. ku > ktop(kkub) ) return
   endif

   if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd .or. kku == 0 .or. (kku > 0 .and. ku2 == 0)) return

   call setvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine
   
   !elemental subroutine setvelocity_2(ucxu, ucyu, L, LL, Lb)
   !
   !use m_flowgeom
   !use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   !use m_sferic, only: Jasfer3D
   !implicit none
   !
   !integer, intent(in)             :: L, LL, Lb
   !double precision, intent(inout) :: ucxu, ucyu
   !
   !integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkub
   !double precision           :: half, sl1, sl2, sl3
   !
   !if (qa(L) == 0) return
   !
   !k1  = ln(1,L) ; k2 = ln(2,L)
   !
   !!      use klnup to check for disabled higher-order correction
   !if (limtypmom == 6 .and. klnup(1,LL).eq.0) return
   !
   !if (qa(L) > 0) then
   !                                                             !   ->      ds1   ds2
   !k = k1 ; kd = k2 ; is =  1 ; half = acl(LL)       ;          !   ->   ku     k     kd
   !n12 = 1
   !ib  = 0
   !
   !sl1  =       slnup(1,LL)
   !sl2  =       slnup(2,LL)
   !sl3  =       slnup(3,LL)
   !
   !kku  =     klnup(1,LL)
   !ku2  = abs(klnup(2,LL))
   !ku   = abs(kku)
   !else if (qa(L) < 0) then
   !                                                             !   <-      ds2   ds1
   !k = k2 ; kd = k1 ; is = -1 ; half = 1d0-acl(LL)   ;          !   <-   kd     k     ku
   !n12 = 2
   !ib  = 2
   !sl1  =       slnup(4,LL)
   !sl2  =       slnup(5,LL)
   !sl3  =       slnup(6,LL)
   !kku  =     klnup(4,LL)
   !ku2  = abs(klnup(5,LL))
   !ku   = abs(kku)
   !endif
   !
   !if (kmx /= 0) then !different ku for 3D
   !   kkub = ku2
   !   ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) ;
   !   if (ku < kbot(kkub) .or. ku > ktop(kkub) ) return
   !endif
   !
   !if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd .or. kku == 0 .or. (kku >= 0 .and. ku2 == 0)) return
   !if(jasfer3D == 0) then
   !   call setvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)
   !else
   !   call setvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)
   !endif
   !
   !end subroutine

   
   end module