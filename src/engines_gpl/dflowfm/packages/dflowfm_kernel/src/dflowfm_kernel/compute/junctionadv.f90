   ! =================================================================================================
   ! =================================================================================================
   subroutine junctionadv()
   use m_flowgeom , only: lnx1d, ln, nd
   use m_flow     , only: q1
   use m_fm_erosed, only: q_zeta
   implicit none
   integer          :: i, L, Li, Lf, La, k
   double precision :: s_l, s_m

   q_zeta = 0d0

   do L = 1,lnx1d                                       ! loop over flow links
      !if (kfu(m)==1) then                                      !.and. kcu(m)==1
      do i = 1,2
         k = ln(i,L)
         do Li = 1,nd(k)%lnx                              ! loop over all flow links for each zeta point
            Lf = nd(k)%ln(Li)
            La = iabs(Lf)
            if (La /= L) then                               ! if (m1 /= current flow link)
               s_l = sign(1d0,Lf+0d0)
               q_zeta(i,L) = q_zeta(i,L) + q1(La) * s_l
            else
               s_m = sign(1d0,Lf+0d0)
            endif
         enddo
         if (nd(k)%lnx == 1) then                           ! if boundary or end node
            q_zeta(i,L)  = q1(L)
         else
            q_zeta(i,L) = ( - s_m * q_zeta(i,L) + ( nd(k)%lnx - 1 ) * q1(L) ) / nd(k)%lnx
         endif
      enddo
   enddo

   end subroutine junctionadv
