   SUBROUTINE deleteSelectedSplines()
      USE M_SPLINES
      use M_POLYGON
      USE M_MISSING
      use geometry_module, only: pinpok
      implicit none

      integer :: i, j
      integer :: inhul
      integer :: ja
      integer :: k
      integer :: key
      integer :: nsol
      double precision :: rd
      double precision :: xi
      double precision :: yi
      logical :: jaAllPoints

      IF (Npl .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO DELETE all Splines? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
         call delSplines()
         RETURN
      ENDIF

      I = 1
      DO
        if (I > mcs) exit
        jaAllPoints = .true.
        DO j=1,lensp(I)
            CALL PINPOK(xsp(i,j), ysp(i,j), Npl, Xpl, Ypl, INHUL, jins, dmiss)
            jaAllPoints = jaAllPoints .and. (INHUL==1)
        enddo
        if (jaAllPoints) then
            call delSpline(I)
            ! splines are shifted to the left, so don't increment I.
        else
            I = I+1
        end if
      enddo

      RETURN

   END SUBROUTINE deleteSelectedSplines
