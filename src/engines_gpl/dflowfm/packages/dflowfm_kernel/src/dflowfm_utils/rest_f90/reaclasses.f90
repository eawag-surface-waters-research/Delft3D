      SUBROUTINE REACLASSES(MINP,CLASS,NUMQ,NUMCLASS)
      USE M_MISSING
      implicit none
      integer :: ja
      integer :: k
      integer :: l
      integer :: minp
      integer :: numc
      integer :: numclass
      integer :: numq
      CHARACTER REC*132
      double precision :: CLASS(NUMQ,NUMCLASS)
      LOGICAL EMPTY


      CALL MISARR(CLASS,NUMQ,NUMCLASS)
      CALL ZOEKAL(MINp,REC,'TEKAL BLOCK INDICATORS',JA)
      if (ja .ne. 1) return

      L = 1
   10 CONTINUE
         READ(MINP,'(A)',END = 999) REC
         IF (REC(1:1) .NE. ' ' .OR. EMPTY(REC) ) THEN
            NUMC = L - 1
            RETURN
         ENDIF
         READ(REC,*,ERR = 888) (CLASS(K,L),K=1,NUMQ)
         L = L + 1
      GOTO 10

  888 CALL READERROR('READING 5 REALS BUT GETTING',REC,MINP)

  999 CALL EOFERROR(MINP)
      END
