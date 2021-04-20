subroutine diffusionimplicit2D()
use m_transport
use m_flowgeom
use m_flow
use m_flowtimes
use m_reduce
use timers

implicit none

double precision :: ddx, difcoeff, diuspL, diag
integer i, k1, k2, L, LL, n

integer(4) ithndl /0/
if (timon) call timstrt ( "diffusionimplicit2D", ithndl )

do i=1,numconst

   bbr = 0d0 ; ccr = 0d0
   do L=1,lnx
      if (dxiau(L) > 0d0) then
         k1 = ln(1,L) ; k2 = ln(2,L)
         if (jadiusp == 1) then
             diuspL = diusp(L)
         else
             diuspL = dicouv
         endif
         difcoeff    = sigdifi(i)*viu(L) + difsedu(i) + diuspL
         ddx         = dxiau(L)*max(0d0, difcoeff)  ! safety first...
         bbr(k1)     = bbr(k1)     + ddx
         bbr(k2)     = bbr(k2)     + ddx
         ccr(lv2(L)) = ccr(lv2(L)) - ddx
      endif
   enddo
   do n = 1,ndx
      if (bbr(n) > 0d0) then
         diag    = 0.5d0*( vol0(n) + vol1(n) )*dti  ! safety first...,  flooding : vol1 > 0, ebbing : vol0 > 0
         bbr(n)  = bbr(n) + diag
         ddr(n)  = diag*constituents(i,n)
      else
         bbr(n)  = 1d0
         ddr(n)  = constituents(i,n)
      endif
      workx(n) = constituents(i,n)
   enddo
   call solve_matrix(workx,ndx,itsol)
   do n = 1,ndxi
      constituents(i,n) = workx(n)
   enddo

enddo

if (timon) call timstop( ithndl )
end subroutine diffusionimplicit2D
