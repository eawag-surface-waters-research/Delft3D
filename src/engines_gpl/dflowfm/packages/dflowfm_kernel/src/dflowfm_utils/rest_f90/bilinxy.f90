      SUBROUTINE BILINXY(X, Y, XZ, YZ, XP, YP, XP2, YP2, INI)
      implicit none
      double precision :: c
      integer :: i
      integer :: ini
      integer :: japarallel
      double precision, SAVE    :: A(4,4), BX(4), BY(4)
      double precision :: X(4), Y(4), XZ(4), YZ(4), XP, YP, XP2, YP2
      INTEGER, SAVE :: INX(4)
      ! (Zi = AXi + BYi + CXiYi + Di ,i=1,4)
      ! Coefficienten in A, rechterlid in B, opl met LU-decompositie
      IF (INI .EQ. 1) THEN
         DO I = 1,4
            A(I,1) =  X(I)-X(1)
            A(I,2) =  Y(I)-Y(1)
            A(I,3) = (Y(I)-Y(1))*(X(I)-X(1))
            A(I,4) =  1
            BX(I)  =  XZ(I)
            BY(I)  =  YZ(I)
         ENDDO
         CALL LUDCMP(A,4,4,INX,C,JAPARALLEL)
         IF (JAPARALLEL .EQ. 1) THEN
            CALL qnerror('Problem in Ludcmp',' ',' ')
            INI = -1
            RETURN
         ENDIF
         CALL LUBKSB(A,4,4,INX,BX)
         CALL LUBKSB(A,4,4,INX,BY)
      ENDIF
      XP2 = (XP-X(1))*BX(1) + (YP-Y(1))*BX(2) + (XP-X(1))*(YP-Y(1))*BX(3) + BX(4)
      YP2 = (XP-X(1))*BY(1) + (YP-Y(1))*BY(2) + (XP-X(1))*(YP-Y(1))*BY(3) + BY(4)
      RETURN
      END
