      SUBROUTINE MAKESR(AR,S0,S1,SR,MFAC)
      implicit none
      integer :: mfac
      double precision :: ar, s0, s1
      double precision :: SR(MFAC+1)

      double precision :: ds, fac
      integer :: k
      DS    = 1
      SR(1) = 0
      DO K = 1,MFAC
         SR(K+1) = SR(K) + DS
         DS = DS*AR
      ENDDO

      FAC  = (S1-S0) / SR(MFAC+1)
      DO K = 0,MFAC
         SR(K+1) = S0 + FAC*SR(K+1)
      ENDDO
      RETURN
      END subroutine makesr
