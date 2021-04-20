      SUBROUTINE SMODPLA(DPLA, DXS, NPL)                   ! SMOOTH WITH DESIRED
      USE M_ALLOC
      implicit none
      DOUBLE PRECISION              :: DPLA(NPL), DXS(NPL)
      DOUBLE PRECISION, ALLOCATABLE :: DH(:)
      integer :: npl

      double precision :: a1
      double precision :: a2
      integer :: k
      integer :: n


      CALL REALLOC(DH,NPL)

      DO K = 1,5

         DH = DPLA
         DO N  = 2,NPL-1
            a1 = 0.5d0*( dxs(n-1) + dxs(N) )
            a2 = 0.5d0*( dxs(n+1) + dxs(N) )
            DPLA(N) =  ( a2*DH(N-1) + a1*DH(N+1) ) / ( a2 + a1 )
         ENDDO

      ENDDO

      DEALLOCATE(DH)

      END SUBROUTINE SMODPLA
