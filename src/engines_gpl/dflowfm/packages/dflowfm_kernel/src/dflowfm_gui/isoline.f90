      subroutine isoline(xa,ya,za,xb,yb,zb)
      use unstruc_display
      implicit none
      double precision :: xa,ya,za,xb,yb,zb,dx,s,c,d,xh(4),yh(4),zh(4)
      dx = 0.2d0*rcir
      call sincosdis(xa,ya,xb,yb,s,c,d)
      xh(1) = xa + dx*s
      yh(1) = ya - dx*c
      xh(2) = xb + dx*s
      yh(2) = yb - dx*c
      xh(3) = xb - dx*s
      yh(3) = yb + dx*c
      xh(4) = xa - dx*s
      yh(4) = ya + dx*c
      zh(1) = za
      zh(2) = zb
      zh(3) = zb
      zh(4) = za
      CALL ISOFIL(Xh,Yh,Zh,4,0)
      end subroutine isoline
