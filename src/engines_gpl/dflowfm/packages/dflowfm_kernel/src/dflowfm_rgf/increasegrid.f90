      SUBROUTINE INCREASEGRID(M,N)
      USE M_GRID
      USE M_MISSING
      use m_alloc
      implicit none
      integer :: m, n

      integer, dimension(2) :: ibounds, iboundsp1

      !if (m <= mmax .and. n <= nmax) return
      !Freshly allocate arrays, so that size fits exactly (e.g., for passing as 2D arrays to ecrrea)
!      if (allocated(xc)) deallocate (xc,yc,zc,ijc,ijyes)

!      mmax = m ; nmax = n ; MNMAX = MAX(M,N)
!      ibounds   = (/ mmax, nmax /)
!      iboundsp1 = (/ mmax+1, nmax+1 /)

      mmax = m+1 ; nmax = n+1 ; MNMAX = MAX(M,N)
      ibounds   = (/ mmax, nmax /)
      iboundsp1 = ibounds

      call realloc(xc, ibounds, fill=dxymis)
      call realloc(yc, ibounds, fill=dxymis)
      call realloc(zc, iboundsp1, fill=dxymis)
      call realloc(ijc, ibounds, fill=0)
      call realloc(ijyes, ibounds, fill=0)

      END SUBROUTINE INCREASEGRID
