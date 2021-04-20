 subroutine writeset(kk,fnam,nr,a)
 implicit none
 integer :: kk, nr
 character (len = 132) :: a(100)
 character (len = 132) :: rec
 character*(*) fnam
 integer :: l, mout, k

 L = index(fnam,'_')
 write(fnam(L+1:L+4), '(i4.4)' ) nr

 call newfil(mout, fnam)
 write(mout,'(a)') '*'//a(1)
 do k = 2,kk
    ! call correctiefile(a(k))
    write(mout,'(a)') a(k)
 enddo
 call doclose(mout)
 end subroutine writeset
