   SUBROUTINE deleteSelectedCrossSections()
      USE m_monitoring_crosssections
      use M_POLYGON
      USE M_MISSING
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

      !IF (Npl .LE. 2) THEN
         CALL CONFRM('NO POLYGON will be used, SO DELETE all cross sections? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
         call delCrossSections()
         RETURN
!      ENDIF

      I = 1
      DO
        if (I > ncrs) exit
        jaAllPoints = .true.
        DO j=1,crs(I)%path%np
            CALL PINPOK(crs(i)%path%xp(j), crs(i)%path%yp(j), Npl, Xpl, Ypl, INHUL, jins, dmiss)
            jaAllPoints = jaAllPoints .and. (INHUL==1)
        enddo
        if (jaAllPoints) then
 !AvD: Disabled           call delCrossSection(I)
            ! cross sections are shifted to the left, so don't increment I.
        else
            I = I+1
        end if
      end do

      RETURN

   END SUBROUTINE deleteSelectedCrossSections
