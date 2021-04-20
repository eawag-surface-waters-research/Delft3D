      SUBROUTINE READARCINFOBLOCK(MINP,D,MC,NC,RMIS)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: j
      integer :: mc
      integer :: minp
      integer :: nc
      double precision :: rmis
      double precision :: D(MC,NC)
      double precision, dimension(MC) :: dline
      CHARACTER TEX*16

      DO 10 J = NC,1,-1
         READ(MINP,*,ERR=101,END=100) (D(I,J),I = 1,MC)
   10 CONTINUE
      DO 20 I = 1,MC
         DO 20 J = 1,NC
            IF (D(I,J) .EQ. RMIS) D(I,J) = dmiss
   20 CONTINUE
      call doclose (MINP)
      RETURN

  100 CONTINUE
      CALL EOFERROR(MINP)
  101 CONTINUE
      WRITE(TEX,'(2I8)') I,J
      CALL READERROR('ERROR READING ARC-INFO BLOCK IN COLNR, ROWNR :',TEX,MINP)
      RETURN
      END
