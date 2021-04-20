!
      SUBROUTINE SETELLIPS(IELL)
      implicit none
      integer :: iell
      COMMON /ELLIPS/ A,E
      double precision :: A,E

      A = 6378137d0
      E = 0.081819d0

      IF (IELL .EQ. 1) THEN      ! Hayford
         A = 6378388d0
         E = 0.081992d0
      ELSEIF (IELL .EQ. 2) THEN  ! Bessel
         A = 6377397d0
         E = 0.081690d0
      ELSEIF (IELL .EQ. 3) THEN  ! WGS 84
         A = 6378137d0
         E = 0.081819d0
      ELSEIF (IELL .EQ. 4) THEN  ! Clarke 1880
         A = 6378249d0
         E = 0.082478d0
      ELSEIF (IELL .EQ. 5) THEN  ! India 1830
         A = 6377276.345d0
         E = 0.081473d0
      ENDIF
      RETURN
      END SUBROUTINE SETELLIPS
