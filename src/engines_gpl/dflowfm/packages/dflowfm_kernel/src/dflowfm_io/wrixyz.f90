      SUBROUTINE WRIXYZ(FILNAM,XS,YS,ZS,NS)
      implicit none
      CHARACTER  (LEN=*):: FILNAM
      INTEGER           :: NS
      DOUBLE PRECISION  :: XS(NS), YS(NS), ZS(NS)
      INTEGER           :: I, MOUT

      CALL NEWFIL(MOUT , FILNAM)
      DO I = 1,NS
         WRITE (MOUT,'(3(F16.7))') XS(I), YS(I), ZS(I)
      ENDDO
      CALL DOCLOSE(MOUT)
      END SUBROUTINE WRIXYZ
