      SUBROUTINE ARROWS(X0,Y0,UR,VR,PSI0,VFAC)
      implicit none
      double precision :: alfa
      double precision :: csa
      integer :: i
      double precision :: psi0
      double precision :: sna
      double precision :: ur
      double precision :: vfac
      double precision :: vr
      double precision :: x0
      double precision :: xlen
      double precision :: y0
      double precision :: X(3), Y(3), XR(3), YR(3)
      DATA X(1)  /0.8d0/, X(2) /1d0/, X(3) /0.8d0/,  &
           Y(1) /-0.1d0/, Y(2) /0d0/, Y(3) /0.1d0/

      IF (UR .EQ. 0 .AND. VR .EQ. 0) RETURN
      ALFA = ATAN2(VR,UR) + PSI0
      CSA  = COS(ALFA)
      SNA  = SIN(ALFA)
      XLEN = SQRT(UR*UR+VR*VR)

      DO 10 I = 1,3
         XR(I) = X0 + VFAC*XLEN*(X(I)*CSA - Y(I)*SNA)
         YR(I) = Y0 + VFAC*XLEN*(Y(I)*CSA + X(I)*SNA)
   10 CONTINUE

      CALL MOVABS(X0,Y0)
      CALL LNABS(XR(2),YR(2))
      CALL LNABS(XR(1),YR(1))

      CALL MOVABS(XR(2),YR(2))
      CALL LNABS(XR(3),YR(3))
      RETURN
      END
