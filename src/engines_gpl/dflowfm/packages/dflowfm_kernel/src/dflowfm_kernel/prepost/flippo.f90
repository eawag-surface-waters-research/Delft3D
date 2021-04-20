!> reverse indexing of selected polygon
subroutine flippo(ip)
   use m_polygon

   implicit none

   integer, intent(in)                         :: ip                     !< polygon point

   integer                                     :: jpoint, jstart, jend, Num
   integer                                     :: i, j, ierror

   double precision, dimension(:), allocatable :: xxp, yyp, zzp

   jpoint = 1
   jstart = 1
   jend   = NPL
   if ( ip.eq.0 ) then
      ierror = 0
   else
      ierror = 1

      call get_polstartend(NPL, XPL, YPL, ip, jstart, jend)

      if ( jstart.le.ip .and. jend.ge.ip ) then
         ierror = 0
      end if
   end if

   if ( ierror.eq.0 ) then
!      call savepol()
!     allocate
      Num = jend-jstart+1
      allocate(xxp(Num), yyp(Num), zzp(Num))
      do j=1,Num
         i = jend-j+1
         xxp(j) = xpl(i)
         yyp(j) = ypl(i)
         zzp(j) = zpl(i)
      end do
      do i=jstart,jend
!         xpl(i) = xph(jend-i+jstart)
!         ypl(i) = yph(jend-i+jstart)
!         zpl(i) = zph(jend-i+jstart)
         j = i-jstart+1
         xpl(i) = xxp(j)
         ypl(i) = yyp(j)
         zpl(i) = zzp(j)
      end do
!     deallocate
      deallocate(xxp, yyp, zzp)
   end if

   return
end subroutine flippo
