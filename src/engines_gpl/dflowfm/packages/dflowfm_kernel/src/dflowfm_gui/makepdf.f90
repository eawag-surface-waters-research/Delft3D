  subroutine makepdf(r,n)
  use m_statistics
  integer :: n
  real    :: r(n), s
  integer :: k,L
  if (n == 0) return
  xpdf = 0d0
  do L = 1,n
     do k = 1,npdf
        if (r(L)  >= ypdf(k)) then
           xpdf(k) = xpdf(k) + 1d0
           exit
        endif
     enddo
  enddo

  s = 0
  do k = 1,npdf
     s = s+xpdf(k)
  enddo
  if (s == 0) return
  xpdf = xpdf / s
  do k = 2,npdf
     xpdf(k) = xpdf(k) + xpdf(k-1)
  enddo
  end subroutine makepdf
