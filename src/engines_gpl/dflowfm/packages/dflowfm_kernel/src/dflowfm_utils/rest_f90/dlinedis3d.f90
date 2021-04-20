      subroutine dlinedis3D(xx3,yy3,zz3,xx1,yy1,zz1,xx2,yy2,zz2,JA,DIS,xxn,yyn,zzn,rl)
      implicit none
      integer          :: ja
      DOUBLE PRECISION :: DIS,XN,YN,ZN, d2
      DOUBLE PRECISION :: R2,RL,X21,Y21,Z21,X31,Y31,Z31
      DOUBLE PRECISION :: xx1,xx2,xx3,yy1,yy2,yy3,zz1,zz2,zz3,xxn,yyn,zzn

      !     korste afstand tot lijnelement

      JA  = 0

      x21 = xx2-xx1
      y21 = yy2-yy1
      z21 = zz2-zz1
      x31 = xx3-xx1
      y31 = yy3-yy1
      z31 = zz3-zz1

      r2  = x21*x21 + y21*y21 + z21*z21
      if (r2 .ne. 0d0) then
         RL = (X31*X21 + Y31*Y21 + Z31*Z21) / R2
         IF (0d0 .LE. RL .AND. RL .LE. 1d0) then
            JA = 1
         endif
         XXN  = xx1 + RL*x21
         YYN  = yy1 + RL*y21
         ZZN  = zz1 + RL*z21
         x31 = xxn-xx3
         y31 = yyn-yy3
         z31 = zzn-zz3
         DIS = sqrt(x31*x31 + y31*y31 + z31*z31)
      else
         DIS = 0d0
      endif

      RETURN

      end subroutine dlinedis3D
