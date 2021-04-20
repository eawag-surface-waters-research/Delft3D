      SUBROUTINE ARROWSXYzfac(X0,Y0,UX,UY,VFAC,JW,zfac)
      implicit none
      integer :: i
      integer :: jw
      double precision :: X0,Y0,UX,UY,VFAC,zfac

      IF (UX .EQ. 0 .AND. UY .EQ. 0) RETURN

      uy = uy

      CALL MOVABS(X0,Y0)
      CALL LNABS(x0+ux*vfac,y0+uy*vfac*zfac)
      RETURN
      END
