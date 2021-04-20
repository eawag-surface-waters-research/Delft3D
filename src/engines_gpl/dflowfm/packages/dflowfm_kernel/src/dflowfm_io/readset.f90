 subroutine readset(kk,mbca, a)
 implicit none
 integer :: kk
 character (len = 132) :: a(100)
 character (len = 132) :: rec
 integer :: k, mbca

 do k = 1,kk
    read(mbca,'(a)') a(k)
 enddo
 end subroutine readset
