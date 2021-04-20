! =================================================================================================
! =================================================================================================
 subroutine tridag(a,b,c,d,e,u,n)
 implicit none
 integer          :: n, j
 double precision :: a(n),b(n),c(n),d(n),e(n),u(n), bet, accur = 1d-15

 bet =b(1)
 u(1)=d(1)/bet
 do j=2,n
    e(j)=c(j-1)/bet
    bet=b(j)-a(j)*e(j)
    if (abs(bet) < accur) then
        bet = sign(accur,bet)
    endif
    u(j)=(d(j)-a(j)*u(j-1))/bet
 enddo

 do j=n-1,1,-1
    u(j)=u(j)-e(j+1)*u(j+1)
 enddo
 end subroutine tridag
