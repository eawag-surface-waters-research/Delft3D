 subroutine chknan(a, b, n)
 use m_flow

 implicit none
 integer           :: n
 double precision  :: a(n)
 character(len=*)  :: b

 integer           :: i
 logical           :: isnan
 character(len=40) :: tex
 do i = 1,n
    if (isnan(a(i)) ) then
       write(tex,'(I10)') i
       write(*,*)  'isnan: ', b , tex
       call error ('isnan: ', b , tex)
    endif
 !   write(mdump,*) b, i, a(i)
 enddo
 end subroutine chknan
