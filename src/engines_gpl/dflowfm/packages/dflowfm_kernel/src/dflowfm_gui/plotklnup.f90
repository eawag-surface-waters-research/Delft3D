!     plot stencil for higher-order corrections to screen
      subroutine plotklnup(L)
         use m_flowgeom
         implicit none

         integer, intent(in)   :: L  !< flowlink number

         double precision      :: sln1, sln2, sln3
         integer               :: i, ip, k1, k2, kdum

         integer, dimension(3) :: icolor = (/ 31, 221 , 31 /)

         i = 0
         do ip=0,3,3
            i = i+1
            k1   = klnup(1+ip,L)
            sln1 = slnup(1+ip,L)
            k2   = iabs(klnup(2+ip,L))
            sln2 = slnup(2+ip,L)
            sln3 = slnup(3+ip,L)
            if ( k1.ne.0 ) then
               kdum = iabs(k1)
               call cirr(xz(kdum),yz(kdum),icolor(i))
               CALL dHTEXT(sln1,xz(kdum),yz(kdum),0d0)
            else
               call cirr(xu(L),yu(L),icolor(3))
            end if
            if ( k1.gt.0 ) then
               if ( k2.gt.0 ) then
                  call cirr(xz(k2),yz(k2),icolor(i))
                  CALL dHTEXT(sln2,xz(k2),yz(k2),0d0)
                  CALL dHTEXT(sln3,xu(L),yu(L),0d0)
               end if
            end if
         end do

         return
      end subroutine plotklnup
