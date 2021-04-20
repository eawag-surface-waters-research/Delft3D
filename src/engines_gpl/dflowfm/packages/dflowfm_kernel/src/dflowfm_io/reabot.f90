      SUBROUTINE REABOT(   MMDD,   JA)
      USE M_GRID
      implicit none

      integer :: mmdd, ja, m1, n1, m2, n2, L1, L2, L3, L4, L5
      integer :: m, n
      double precision :: af

      CHARACTER REC*132
      CALL READYY('Reading SIMONA *.bottom File',0d0)

    5 CONTINUE

      READ(MMDD,'(A)',END = 777) REC
      IF (REC(1:3) .ne. 'BOX') THEN
          GOTO 5
      ELSE
          L1 =  INDEX(REC, '=(')
          READ  (REC(L1+2:), *)   M1

          L2 =  L1 + INDEX(REC(L1:), ',')
          L3 =       INDEX(REC(:), ';') -1

          READ  (REC(L2:L3), *)        N1


          L3 =  INDEX(REC, ';' )
          READ  (REC(L3+1:), *)    M2

          L4 =  L3 + INDEX(REC(L3:), ',')
          L5 =       INDEX(REC,     ')') - 1

          READ  (REC(L4:L5), *)    N2



      ENDIF

      DO 10 M = M1,M2
         AF = dble(M) / dble(MC)
         CALL READYY('Reading SIMONA *.bottom File',AF)

         READ(MMDD,'(A)',END = 777) REC
         BACKSPACE(MMDD)


         READ(MMDD,*,END = 999,ERR = 888) (ZC(M,N),N = N1, N2)
   10 CONTINUE
      GOTO 5


  777 CALL READYY('Reading SIMONA *.bottom File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 1
      RETURN

  999 CONTINUE
      CALL QNEOFERROR(MMDD)
      CALL READYY('Reading SIMONA *.bottom File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 0
      RETURN

  888 CALL QNREADERROR('Reading ERROR SIMONA bottom File With Wrong Dimensions', ' ', MMDD)
      CALL READYY('Reading *.bottom File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 0
      END SUBROUTINE REABOT
