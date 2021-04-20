 subroutine checkvolnan(i)
 use MessageHandling
 use m_flowgeom
 use m_flow

 implicit none

 integer, intent(in) :: i
 integer :: n

 do n = 1,ndx
    if (isnan(vol1(n))) then
       write(msgbuf,*)  ' volnan ', i, n ; call msg_flush()
    endif
   enddo
end subroutine checkvolnan
