      SUBROUTINE ISOCOL2(VALC,NCOL)
      implicit none
      integer :: i, ncol
      double precision :: valc

      integer :: NCOLS,NV,NIS,NIE,JAAUTO
      double precision :: VMAX,VMIN,DV,VAL
      COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      DO 10 I = NV,1,-1
         IF (VALC .GE. VAL(I)) THEN
            NCOL = I + 1
            CALL SETCOL(NCOLS(NCOL))
            NCOL = NCOLS(NCOL)
            RETURN
         ENDIF
   10 CONTINUE
      NCOL = ncols(1)
      CALL SETCOL(NCOL)
      RETURN
      END
