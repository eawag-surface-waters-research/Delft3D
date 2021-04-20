 subroutine iadvecini()
 use m_flowgeom
 use m_flow
 use unstruc_messages
 implicit none
 integer :: L, jado

 jado = 0
 if (jado == 1) then
 if (cflmx > 0.9d0 )  then
    if (iadvec == 3) then
        iadvec = 5
    else if (iadvec == 4) then
        iadvec = 6
    else
        iadvec = 5
    endif
    call mess(LEVEL_INFO, 'CFLMax > 0.9, Advectype switched to semi implicit Piaczek&Williams ')
 else if (cflmx < 0.71d0) then
    if (iadvec == 5) then
        iadvec = 3
        call mess(LEVEL_INFO, 'CFLMax < 0.71 Advectype switched to explicit ')
    else if (iadvec == 6) then
        iadvec = 4
        call mess(LEVEL_INFO, 'CFLMax < 0.71 Advectype switched to explicit ')
    endif
 endif
 endif


 if (kmx > 0 .or. iadvec == 0) iadvec1D = iadvec                  ! for now, same if 3D
 do L = 1,lnx
    if (iadv(L) .ne. -1) then
       iadv(L) = iadvec
       if (L <= Lnx1D) then
          if ( iadvec .ne. 0) iadv(L) = iadvec1D ! voorlopig altijd piacz impl 4 voor 1D
       endif
    endif
 enddo

 end subroutine iadvecini
