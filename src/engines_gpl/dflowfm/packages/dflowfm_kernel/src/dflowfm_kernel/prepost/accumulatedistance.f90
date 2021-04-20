      !> Stop afstand tussen polygoonpunten vanaf begin in array
      SUBROUTINE accumulateDistance(X,Y,T,MMAX)

      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer :: mmax
      DOUBLE PRECISION, intent(in)  :: X(MMAX), Y(MMAX) !< Input polyline coordinates
      double precision, intent(out) :: T(MMAX)          !< Output accumulated distances along polyline segments.

      integer :: k

      T(1) = 0d0
      DO K = 2,MMAX
         T(K)  = T(K-1) + dbdistance( x(k), y(k), x(k-1), y(k-1), jsferic, jasfer3D, dmiss)
      ENDDO
      RETURN
      END SUBROUTINE accumulateDistance
