! =================================================================================================
! =================================================================================================
   subroutine setuc1D ()
      use m_flow
      use m_flowgeom
      implicit none
      integer :: L, LL, La, n, nx, ip, i12, k2, ja1D

      if (kmx == 0 .and. lnx1D > 0) then ! setuc
         uc1D  = 0d0
         do n  = ndx2D+1,ndxi
            nx = nd(n)%lnx
            if (nx == 2) then
               ja1D = 1
               do LL = 1,nx
                  L   = nd(n)%ln(LL)
                  La  = iabs(L)
                  if (iabs(kcu(La)) /= 1) ja1D = 0
               enddo
               if (ja1D == 1) then
                  do LL = 1,nx
                     L   = nd(n)%ln(LL)
                     La  = iabs(L)
                     i12 = 2 ; if (L < 0) i12 = 1
                     if (LL == 1) then
                        if (L  > 0) then
                           ip =  1
                        else
                           ip = -1
                        endif
                     else
                        if (ip*L > 0) then
                           ip = -ip
                        endif
                     endif
                     uc1D(n) = uc1D(n) + wcL(i12,La)*u1(La)*ip
                  enddo
               endif
            endif
         enddo

         do LL = lnxi+1,lnx          ! bnd
            if (kcu(LL) == -1) then  ! 1D type link
               n = Ln(1,LL) ; k2 = Ln(2,LL)
               if (uc1D(k2) .ne. 0) then
                  uc1D(n) = uc1D(k2)
               endif
            endif
         enddo
      endif

   end subroutine setuc1D
