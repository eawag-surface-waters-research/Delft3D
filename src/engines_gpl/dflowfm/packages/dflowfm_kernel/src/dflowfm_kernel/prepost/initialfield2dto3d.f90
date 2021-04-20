 subroutine initialfield2Dto3D( v2D, v3D, tr13, tr14 )
 use m_flowgeom
 use m_flow
 use m_missing
 use timespace

 implicit none

 double precision, intent(inout) :: v2D(*) , v3D(*)
 double precision, intent(in   ) :: tr13, tr14
 double precision                :: zb, zt, zz
 integer                         :: n, k, kb, kt
 !character(len=1), intent(in)    :: operand !< Operand type, valid values: 'O', 'A', '+', '*', 'X', 'N'.

 zb = -1d9 ; if (tr13 .ne. dmiss) zb = tr13
 zt =  1d9 ; if (tr14 .ne. dmiss) zt = tr14
 do n = 1,ndx
    if ( v2D(n) .ne. dmiss ) then
       if (kmx == 0) then
          call operate(v3D(n), v2D(n), operand)
       else
          kb = kbot(n) ; kt = ktop(n)
          do k = kb, kt
             zz = 0.5d0*( zws(k) + zws(k-1) )
             if (zz > zb .and. zz < zt ) then
                call operate(v3D(k), v2D(n), operand)
             endif
          enddo
       endif
    endif
 enddo
 end subroutine initialfield2Dto3D
