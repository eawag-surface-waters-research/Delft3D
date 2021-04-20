      SUBROUTINE DISPF2cir(X,Y,N,Rcx,Rcy,NCOL)
      implicit none
      integer :: i
      integer :: n
      integer :: ncol
      integer :: nmax
!     LAAT EENDIMENSIONALE FUNCTIE ZIEN met cirkels
      double precision :: X(N), Y(N), rcx, rcy
      CALL SETCOL(NCOL)
      CALL MOVABS(X(1),Y(1))
      DO I = 2,N
         CALL LNABS(X(I),Y(I))
      ENDDO
      CALL MOVABS(X(1),Y(1))
      if (rcx > 0) CALL fbox( x(1)-rcx,y(1)-rcy,x(1)+rcx,y(1)+rcy )     ! CIR(RCIR)
      DO I = 2,N
         if (rcx > 0) CALL fbox( x(i)-rcx,y(i)-rcy,x(i)+rcx,y(i)+rcy )  ! CIR(RCIR)
      ENDDO

      RETURN
      END
