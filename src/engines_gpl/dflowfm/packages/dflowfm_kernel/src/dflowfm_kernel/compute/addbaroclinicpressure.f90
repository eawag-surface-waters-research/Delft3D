subroutine addbaroclinicpressure()
use m_flowgeom
use m_flow
implicit none
integer                    :: L,LL,Lb,Lt,n

if (jabarocterm == 1) then

   !$OMP PARALLEL DO       &
   !$OMP PRIVATE(LL,Lb,Lt)

   do LL = 1,lnxi
      if (hu(LL) == 0d0) cycle
      call getLbotLtop(LL,Lb,Lt)
      if (Lt < Lb) then
          cycle
      endif
      call addbaroc(LL,Lb,Lt)
   enddo

   !$OMP END PARALLEL DO

else if (jabarocterm == 2 .or. jabarocterm == 3 .or. kmx == 0) then

   !$OMP PARALLEL DO       &
   !$OMP PRIVATE(LL,Lb,Lt)

   do LL = 1,lnxi
      if (hu(LL) == 0d0) cycle
      call getLbotLtop(LL,Lb,Lt)
      if (Lt < Lb) then
          cycle
      endif
      call addbaroc2(LL,Lb,Lt)
    enddo

   !$OMP END PARALLEL DO

 else

    rvdn = 0d0 ; grn = 0d0

   !$OMP PARALLEL DO       &
   !$OMP PRIVATE(n)
    do n = 1,ndx
       call addbarocn(n)
    enddo
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO       &
   !$OMP PRIVATE(LL,Lb,Lt)
    do LL = 1,lnxi
      if (hu(LL) == 0d0) cycle
      call getLbotLtop(LL,Lb,Lt)
      if (Lt < Lb) then
          cycle
      endif
      call addbarocL(LL,Lb,Lt)
    enddo
   !$OMP END PARALLEL DO

 endif

 end subroutine addbaroclinicpressure
