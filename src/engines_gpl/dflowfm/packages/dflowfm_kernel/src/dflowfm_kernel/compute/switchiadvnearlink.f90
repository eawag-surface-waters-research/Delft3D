subroutine switchiadvnearlink(L)
use m_flowgeom
use m_flow
implicit none
integer :: L, k1, k2, kk,LL, iadv1, iadv2

k1 = ln(1,L) ; k2 = ln(2,L)

if (iadvec==0) then
   iadv1 = 0
   iadv2 = 0
elseif (u0(L) > 0) then
   iadv1 = 8  ! piaczek incoming upwind
   iadv2 = 0  ! noadv downstream
else if (u0(L) < 0) then
   iadv1 = 0
   iadv2 = 8
else ! == (now safe for grid direction)
   iadv1 = 8
   iadv2 = 8
end if

do kk  = 1,nd(k1)%lnx
   LL = iabs( nd(k1)%ln(kk) )
   if ( iadv(LL) .ne. 22 .and. (kcu(LL) == 1 .or. kcu(LL) == 2)) then ! Only for regular 1D or 2D.
        iadv(LL) = iadv1
   endif
enddo
do kk  = 1,nd(k2)%lnx
   LL = iabs( nd(k2)%ln(kk) )
   if ( iadv(LL) .ne. 22 .and. (kcu(LL) == 1 .or. kcu(LL) == 2)) then ! Only for regular 1D or 2D.
        iadv(LL) = iadv2
   endif
enddo

end subroutine switchiadvnearlink
