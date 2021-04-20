       SUBROUTINE MAKESSQ(S,A,SR,SL,SSQ,NT,MFAC,IMAX)
       implicit none
       integer :: nt, mfac, imax
      double precision :: S(IMAX), A(IMAX), SR(IMAX), SL(IMAX), SSQ(IMAX)
      double precision :: glad
      integer :: i, k, kr
      double precision :: ar, al
      GLAD(I)   = ( S(I+1)-S(I) ) / ( S(I)-S(I-1) )
      IF (NT .EQ. 2) THEN
         DO K = 1,MFAC + 1
            SSQ(K) = S(1) + ( S(2) - S(1) ) * (dble(K-1)) / dble(MFAC)
         ENDDO
      ELSE IF (NT .GE. 3) THEN
         DO I = 2,NT-1
            A(I) = GLAD(I)
         ENDDO
         A(1)  = A(2)
         A(NT) = A(NT-1)

         DO 10 I = 1,NT-1
            AR = A(I+1)**(1.0/dble(MFAC))
            CALL MAKESR(AR,S(I),S(I+1),SR,MFAC)
            AL = A(I)**(1.0/dble(MFAC))
            CALL MAKESR(AL,S(I),S(I+1),SL,MFAC)
            DO 20 K = 1,MFAC+1
               KR   = (I-1)*MFAC + K
               AR   = dble(K-1) / dble(MFAC)
               AL   = 1 - AR
               SSQ(KR) = AR*SR(K) + AL*SL(K)

               AR   = ( SSQ(KR) - S(I) ) / ( S(I+1) - S(I) )
               AL   = 1 - AR
               SSQ(KR) = AR*SR(K) + AL*SL(K)

!              AL = ( S(I+1) - SL(K) ) / ( S(I+1) - S(I) )
!              AR = ( SR(K)  -  S(I) ) / ( S(I+1) - S(I) )
!              AT = AL + AR
!              AL = AL/AT
!              AR = AR/AT
!              SSQ(KR) = AR*SR(K) + AL*SL(K)
    20      CONTINUE
    10   CONTINUE

      ENDIF

      RETURN
      END subroutine makessq
