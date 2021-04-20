      SUBROUTINE MINMXI(IS,MMAX,MINI,MAXI)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: maxi
      integer :: mini
      integer :: mmax
      INTEGER IS(MMAX)

      MAXI = -999999
      MINI =  999999
      DO 10 I = 1,MMAX
         IF (IS(I) .NE. dmiss) THEN
            MAXI = MAX(MAXI,IS(I))
            MINI = MIN(MINI,IS(I))
         ENDIF
   10 CONTINUE
      RETURN
      END
