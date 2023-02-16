   !----- AGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2017-2023.
   !
   !  This file is part of Delft3D (D-Flow Flexible Mesh component).
   !
   !  Delft3D is free software: you can redistribute it and/or modify
   !  it under the terms of the GNU Affero General Public License as
   !  published by the Free Software Foundation version 3.
   !
   !  Delft3D  is distributed in the hope that it will be useful,
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   !  GNU Affero General Public License for more details.
   !
   !  You should have received a copy of the GNU Affero General Public License
   !  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
   !
   !  contact: delft3d.support@deltares.nl
   !  Stichting Deltares
   !  P.O. Box 177
   !  2600 MH Delft, The Netherlands
   !
   !  All indications and logos of, and references to, "Delft3D",
   !  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
   !  Deltares, and remain the property of Stichting Deltares. All rights reserved.
   !
   !-------------------------------------------------------------------------------

   ! $Id$
   ! $HeadURL$

   Subroutine sethigherorderadvectionvelocities()
   use m_flowgeom
   use m_flow
   use m_sferic
   use m_flowtimes
   use unstruc_messages
   use m_nod2lin
   use m_limiters, only: dslim

   implicit none

   integer  L, LL , Lb, Lt
   integer, dimension (:), allocatable       :: negIdx, posIdx ! pointer to 1-dim real array
   integer, dimension (:), allocatable, save :: L2 ! pointer to 1-dim real array
   logical             :: countpos, countneg

   if (.not. allocated(L2)) then
      allocate(L2(lnx))
      do L = 1, lnx
         L2(L) = L
      enddo
   endif

   if (limtypmom < 1 ) return

   if (kmx == 0) then
      negIdx = pack(L2,qa(L2)<0)
      posIdx = pack(L2,qa(L2)>0)
      countpos = count(qa(l2)>0) > 0
      countneg = count(qa(l2)>0) < 0

      if (jasfer3D == 0) then
         if(countpos) call setvelocity_pos(ucxu(posIdx), ucyu(posIdx), posIdx, posIdx, posIdx)
         if(countneg) call setvelocity_neg(ucxu(negIdx), ucyu(negIdx), negIdx, negIdx, negIdx)
      else
         if(countpos) call setvelocity_pos_Jasfer3D(ucxu(posIdx), ucyu(posIdx), posIdx, posIdx, posIdx)
         if(countneg) call setvelocity_neg_Jasfer3D(ucxu(negIdx), ucyu(negIdx), negIdx, negIdx, negIdx)
      endif

   else
      if (jasfer3D == 0) then
         !$OMP PARALLEL DO PRIVATE(LL, Lb, Lt)
         do L  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)
            if (qa(L) .ne. 0d0) then
               call getLbotLtop(L,Lb,Lt)
               do LL = Lb,Lt
                  if (qa (LL) > 0) then
                     call setvelocity_pos(ucxu(LL), ucyu(LL), L, LL, Lb)
                  else
                     call setvelocity_neg(ucxu(LL), ucyu(LL), L, LL, Lb)
                  endif
               enddo
            endif
         enddo  ! horizontal
         !$OMP END PARALLEL DO
      else
         !$OMP PARALLEL DO PRIVATE(LL, Lb, Lt)
         do L  = 1,lnx                                                    ! upwind (supq) + limited high order (dsq)
            if (qa(L) .ne. 0d0) then
               call getLbotLtop(L,Lb,Lt)
               do LL = Lb,Lt
                  if (qa (LL) > 0) then
                     call setvelocity_pos_Jasfer3D(ucxu(LL), ucyu(LL), L, LL, Lb)
                  else
                     call setvelocity_neg_Jasfer3D(ucxu(LL), ucyu(LL), L, LL, Lb)
                  endif
               enddo
            endif
         enddo  ! horizontal
         !$OMP END PARALLEL DO
      endif
   endif

   end subroutine sethigherorderadvectionvelocities

   module m_setvelocity
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
   end module

   elemental subroutine setvelocity_neg(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   use m_setvelocity
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
   use m_setvelocity
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

   if (hs(ln(1,LL)) < Chkadvd .or. hs(ln(2,LL)) < Chkadvd .or. kku == 0 .or. (kku > 0 .and. ku2 == 0)) return

   call setvelocity(ucxu, ucyu, L, LL, Lb, k, kd, is, n12, ib, half, sl1, sl2, sl3, kku, ku2, ku)

   end subroutine

   elemental subroutine setvelocity_neg_jasfer3D(ucxu, ucyu, L, LL, Lb)

   use m_flowgeom
   use m_flow, only: limtypmom, qa, kmx, kbot, kmxn, kmxl, ktop, hs, chkadvd, ducxdx, ducxdy, ducydx, ducydy, u1, ucx, ucy, u1
   use m_setvelocity
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
   use m_setvelocity
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
