   module m_set_HO_advec_velocity
   contains
   
   !> Actual calculation of HOA velocity for Jasfer3D = 0
   elemental subroutine setHOAvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   use m_sferic
   use m_flowtimes
   use unstruc_messages
   use m_nod2lin
   use m_flowgeom
   use m_limiters, only: dslim, dslimvec
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb                         !> Flowlink numbers, main link, 3D link and bottom link respectively
   double precision, intent(inout) :: ucxu, ucyu                        !> HOA velocities that need to be calculated
   integer, intent(in)             :: k, kd, is, n12, ib, kku, ku2, ku  !> indices that depend on flow direction
   double precision, intent(in)    :: half, sl1, sl2, sl3               !> indices that depend on flow direction

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

   call dslimvec(ds1x, ds1y, ds2x, ds2y, csu(LL), snu(LL), limtypmom, dsx, dsy)
   ucxu = ucxu + cf*dsx
   ucyu = ucyu + cf*dsy

   end subroutine
   
   !> Actual calculation of HOA velocity for Jasfer3D = 1
   elemental subroutine setHOAvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   use m_sferic
   use m_flowtimes
   use unstruc_messages
   use m_nod2lin
   use m_flowgeom
   use m_limiters, only: dslim, dslimvec
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb                         !> Flowlink numbers, main link, 3D link and bottom link respectively
   double precision, intent(inout) :: ucxu, ucyu                        !> HOA velocities that need to be calculated
   integer, intent(in)             :: k, kd, is, n12, ib, kku, ku2, ku  !> indices that depend on flow direction
   double precision, intent(in)    :: half, sl1, sl2, sl3               !> indices that depend on flow direction

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

   call dslimvec(ds1x, ds1y, ds2x, ds2y, csu(LL), snu(LL), limtypmom, dsx, dsy)
   ucxu = ucxu + cf*dsx
   ucyu = ucyu + cf*dsy

   end subroutine
   
   !> calculates parameters when QA(L) < 0, JAsfer3D = 0
   elemental subroutine setHOAvelocity_neg(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb    !> Flowlink numbers, 3D link, main link, and bottom link respectively
   double precision, intent(inout) :: ucxu, ucyu   !> HOA velocities that need to be calculated

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkua, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)
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
      kkua = ku
      ku  = kbot(kkua) + kmxn(kkua) - ( Lb + kmxL(LL) - L)
      if (ku < kbot(kkua) .or. ku > ktop(kkua)) return
      
      if (kku > 0) then
         kkub = ku2 
         ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) 
         if (ku2 < kbot(kkub) .or. ku2 > ktop(kkub)) return
      endif
   endif
   
   if (kku == 0 .or. (kku > 0 .and. ku2 == 0)) return

   call setHOAvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine
   
   !> calculates parameters when QA(L) > 0, JAsfer3D = 0
   elemental subroutine setHOAvelocity_pos(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb    !> Flowlink numbers, main link, 3D link and bottom link respectively
   double precision, intent(inout) :: ucxu, ucyu   !> HOA velocities that need to be calculated

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkub, kkua
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)
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
      kkua = ku
      ku  = kbot(kkua) + kmxn(kkua) - ( Lb + kmxL(LL) - L)
      if (ku < kbot(kkua) .or. ku > ktop(kkua)) return
      
      if (kku > 0) then
         kkub = ku2 
         ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) 
         if (ku2 < kbot(kkub) .or. ku2 > ktop(kkub)) return
      endif
   endif
   
   if (kku == 0 .or. (kku > 0 .and. ku2 == 0)) return
   
   call setHOAvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine

   !> calculates parameters when QA(L) < 0, JAsfer3D = 1
   elemental subroutine setHOAvelocity_neg_jasfer3D(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   implicit none

   integer, intent(in)             :: L, LL, Lb    !> Flowlink numbers, main link, 3D link and bottom link respectively
   double precision, intent(inout) :: ucxu, ucyu   !> HOA velocities that need to be calculated

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkua, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)
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
      kkua = ku
      ku  = kbot(kkua) + kmxn(kkua) - ( Lb + kmxL(LL) - L)
      if (ku < kbot(kkua) .or. ku > ktop(kkua)) return
      
      if (kku > 0) then
         kkub = ku2 
         ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) 
         if (ku2 < kbot(kkub) .or. ku2 > ktop(kkub)) return
      endif
   endif
   
   if (kku == 0 .or. (kku > 0 .and. ku2 == 0)) return
   
   call setHOAvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine

   !> calculates parameters when QA(L) > 0, JAsfer3D = 1
   elemental subroutine setHOAvelocity_pos_jasfer3D(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1

   implicit none

   integer, intent(in)             :: L, LL, Lb    !> Flowlink numbers, main link, 3D link and bottom link respectively
   double precision, intent(inout) :: ucxu, ucyu   !> HOA velocities that need to be calculated

   integer                    :: k1, k2, k, kd, is, n12, ib, kku, ku2, ku, kkua, kkub
   double precision           :: half, sl1, sl2, sl3

   k1  = ln(1,L) ; k2 = ln(2,L)
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
      kkua = ku
      ku  = kbot(kkua) + kmxn(kkua) - ( Lb + kmxL(LL) - L)
      if (ku < kbot(kkua) .or. ku > ktop(kkua)) return
      
      if (kku > 0) then
         kkub = ku2 
         ku2 = kbot(kkub) + kmxn(kkub) - ( Lb + kmxL(LL) - L) 
         if (ku2 < kbot(kkub) .or. ku2 > ktop(kkub)) return
      endif
   endif
   
   if (kku == 0 .or. (kku > 0 .and. ku2 == 0)) return

   call setHOAvelocity_jasfer3D(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine

   end module