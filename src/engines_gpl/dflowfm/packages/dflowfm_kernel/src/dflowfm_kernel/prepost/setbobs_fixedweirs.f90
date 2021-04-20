!> Sets the bob values on the flow links that are overridden by a fixed weir.
!! This is based on the interpolated pliz values from the fixed weir definition.
subroutine setbobs_fixedweirs()
use m_flowgeom
use m_fixedweirs
implicit none

integer                       :: i, ip, iL, Lf
double precision              :: alpha, zc


if ( nfxw == 0 ) return

do i = 1,nfxw
    do iL=1,fxw(i)%lnx
        Lf = abs(fxw(i)%ln(iL))
        ip = fxw(i)%indexp(iL)
        alpha = fxw(i)%wfp(iL)
        zc = alpha * fxw(i)%zp(ip) + (1d0-alpha)*fxw(i)%zp(ip+1)
        bob(1,Lf) = max( zc,bob(1,Lf) ) ; bob(2,Lf) = max( zc,bob(2,Lf) )
    end do
end do
end subroutine setbobs_fixedweirs
