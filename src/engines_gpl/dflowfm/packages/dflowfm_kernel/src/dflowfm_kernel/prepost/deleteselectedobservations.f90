   SUBROUTINE deleteSelectedObservations()
   use m_observations
      USE M_SAMPLES
      use M_POLYGON
      use m_missing
      use geometry_module, only: pinpok
      implicit none

      integer :: i
      integer :: inhul
      integer :: ja
      integer :: k
      integer :: key
      integer :: nsol
      double precision :: rd
      double precision :: xi
      double precision :: yi

      IF (Npl .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO DELETE all Observation Points? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
         call deleteObservations()
         RETURN
      ENDIF

      DO 10 I = 1,numobs
            CALL PINPOK(xobs(I), yobs(I), Npl, Xpl, Ypl, INHUL, jins, dmiss)
            IF (INHUL .EQ. 1) then
                call deleteObservation(I)
            end if
   10 CONTINUE
    call purgeObservations()

      RETURN

   END SUBROUTINE deleteSelectedObservations
