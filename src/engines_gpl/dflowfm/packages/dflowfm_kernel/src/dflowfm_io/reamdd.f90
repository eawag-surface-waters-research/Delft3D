      SUBROUTINE REAMDD(   MMDD,   RD1,MC,NC,JA)
      implicit none

      integer :: mmdd, mc, nc, ja
      DOUBLE PRECISION :: RD1(MC,NC)
      integer :: m, n
      double precision :: af

      CHARACTER REC*132
      CALL READYY('Reading md-Dept File',0d0)
    5 CONTINUE
      READ(MMDD,'(A)',END = 999) REC
      IF (REC(1:1) .EQ. '*') GOTO 5
      BACKSPACE(MMDD)


      DO 10 N = 1, NC
         AF = dble(N) / dble(NC)
         CALL READYY('Reading md-Dept File',AF)
         READ(MMDD,*,END = 999,ERR = 888) (RD1(M,N),M = 1,MC)
   10 CONTINUE
      CALL READYY('Reading md-Dept File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 1
      RETURN

  999 CONTINUE
      CALL QNEOFERROR(MMDD)
      CALL READYY('Reading md-Dept File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 0
      RETURN

  888 CALL QNREADERROR('Reading, DD Depth File With Wrong Dimensions', ' ', MMDD)
      CALL READYY('Reading md-Dept File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 0
      END SUBROUTINE REAMDD
