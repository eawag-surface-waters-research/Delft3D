   subroutine settaubxu_nowave()
      use m_flowgeom
      use m_flow
      use m_physcoef
      use m_waves, only: taubxu
      implicit none

      integer            :: L , Lb, Lt
      integer            :: k1, k2
      double precision   :: cz, z00, cwall, rz, umod

   do L = 1, lnx
      call getLbotLtop(L,Lb,Lt)
      if (Lt<Lb) cycle
      if (hu(L)>epshu) then
         if (frcu(L)>0d0) then
            call getczz0(hu(L), frcu(L), ifrcutp(L), cz, z00)
         else
            call getczz0(hu(L), frcuni, ifrctypuni, cz, z00)
         end if
         umod = sqrt(u1(Lb)*u1(Lb) + v(Lb)*v(Lb))
         z0urou(L) = hu(L)*exp(-1d0 - vonkar*cz/sag)
         if (kmx>0) then
            rz = 1d0 + hu(Lb)/2d0/z0urou(L)                  ! cell centre first bottom layer
         else
            rz = 1d0 + hu(L)/(ee*z0urou(L))
         endif
         cz            = log(rz)/vonkar
         cwall         = 1./(cz**2)
         taubxu(L)    = rhomean*cwall*umod*umod
      else
         taubxu(L)    = eps10
      endif
   enddo
   !
   ! for output purposes
   if (jamaptaucurrent>0) then
      tausmax   = 0d0
      do L=1,lnx
         k1=ln(1,L)
         k2=ln(2,L)
         if (hu(L) > epshu) then
            tausmax(k1) = tausmax(k1) + taubxu(L)*wcL(1,L)
            tausmax(k2) = tausmax(k2) + taubxu(L)*wcL(2,L)
         end if
      enddo
   endif

   end subroutine
