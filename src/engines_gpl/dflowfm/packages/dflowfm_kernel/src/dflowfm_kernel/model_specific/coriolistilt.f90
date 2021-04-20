    subroutine coriolistilt(tim)
    use m_netw
    use m_flowgeom
    use m_flow
    use m_sferic
    use unstruc_display
    implicit none

    integer          :: k, L, k1, k2
    double precision :: s1k, xx, yy, samp, ux, uy, dif, alf, tim

    ux = 0.1d0; uy = 0d0;  samp = ux*fcorio/ag
    if (tim == 0d0) then

       do k = 1,numk
          alf   = (yk(k)-ykmin)/(ykmax-ykmin)
          zk(k) = -600d0 + 500d0*cos(pi*alf)
       enddo

       call setbobs()

       do L = 1,lnx
          u1(L) = csu(L)*ux + snu(L)*uy
       enddo
     endif

    call statisticsnewstep()

    do k   = 1,ndx
       yy  = yz(k)
       s1k =  -samp*yy

       if (tim == 0d0) then
          s1(k)  = max( bl(k), s1k) ; s0(k) = s1(k)
       endif

       dif = abs(s1(k) - s1k)
       call statisticsonemorepoint(dif)
    enddo

    call statisticsfinalise()
    end subroutine coriolistilt
