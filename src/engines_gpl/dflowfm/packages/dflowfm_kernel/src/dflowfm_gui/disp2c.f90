!
      SUBROUTINE DISP2C(X,Y,N,RCIR,NCOL)
      use m_missing
 !     use gridoperations
      implicit none
      integer          :: n, ncol
      double precision :: X(N), Y(N), rcir
      logical          :: inview

      integer          :: i, istart, key, in
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS

      IF (N .LE. 0) RETURN
      CALL SETCOL(NCOL)

      CALL JGRLINE8(x,y,N)

      if (rcir == 0) return

      IF ( NCOL.NE.0 ) THEN

         in = 0
         DO I = 1,N
            if ( INVIEW(X(i),Y(i)) ) then
               CALL MOVABS(X(I),Y(I))
               CALL CIR(RCIR)
               in = in + 1
               if (in > 5000) exit
            endif
         enddo

         CALL SETCOL(31)
         ISTART = 0
         DO I = 1,N
            IF (X(I) .NE. dmiss) THEN
               IF (ISTART .EQ. 1) THEN
               ELSE
                  CALL MOVABS(X(I),Y(I))
                  CALL CIR(RCIR)
                  ISTART = 1
               ENDIF
            ELSE
               ISTART = 0
            ENDIF
         END DO

      END IF

      RETURN
      END
