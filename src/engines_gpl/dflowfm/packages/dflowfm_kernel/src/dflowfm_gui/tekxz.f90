      SUBROUTINE TEKXZ(NCOL)
      use m_netw
      USE M_FLOWGEOM
      implicit none
      INTEGER :: NCOL
      integer :: n
      double precision :: bar

      DO N = 1,NUMP
         CALL DCIRR ( xz(n), yz(n), YZw(N), NCOL )
      ENDDO

      RETURN
      END SUBROUTINE TEKXZ
