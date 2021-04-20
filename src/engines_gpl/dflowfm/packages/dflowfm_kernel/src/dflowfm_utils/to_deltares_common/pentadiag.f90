! =================================================================================================
! =================================================================================================
   subroutine pentadiag( aavec, avec, bvec, cvec, ccvec, dvec, x, n )
      implicit none
      integer          :: i, n
      double precision :: aa(n), a(n), b(n), c(n), cc(n), d(n), x(n)
      double precision :: aavec(n), avec(n), bvec(n), cvec(n), ccvec(n), dvec(n)
      double precision :: cof0

      aa = aavec
      a  = avec
      b  = bvec
      c  = cvec
      cc = ccvec
      d  = dvec

      do i = 2,n-1
         cof0 = a(i) / b(i-1)
         b(i) = b(i) - cof0 *  c(i-1)
         c(i) = c(i) - cof0 * cc(i-1)
         d(i) = d(i) - cof0 *  d(i-1)
         cof0 = aa(i+1) / b(i-1)
         a(i+1) = a(i+1) - cof0 *  c(i-1)
         b(i+1) = b(i+1) - cof0 * cc(i-1)
         d(i+1) = d(i+1) - cof0 *  d(i-1)
      enddo

      cof0 = a(n) / b(n-1)
      b(n) = b(n) - cof0 *  c(n-1)
      c(n) = c(n) - cof0 * cc(n-1)
      d(n) = d(n) - cof0 *  d(n-1)

      x(n)   = d(n) / b(n)
      x(n-1) = ( d(n-1) - c(n-1) * x(n) ) / b(n-1)
      do i = n-2,1,-1
         x(i) = ( d(i) - c(i) * x(i+1) - cc(i) * x(i+2) ) / b(i)
      enddo

   end subroutine pentadiag
