 subroutine pixcount(xs,ys,zs,jatel)

 USE M_FLOWGEOM
 use m_missing, only: jins, dmiss
 use geometry_module, only: pinpok

 implicit none

 double precision :: xs, ys, zs
 integer :: jatel

 double precision :: xmn, xmx, ymn, ymx
 integer :: nn, k, in
 integer, allocatable, save :: itel(:)
 double precision, allocatable, save :: ztel(:)

 if (jatel == 1) then
    if (.not. allocated (itel) ) then
       allocate(itel(ndx), ztel(ndx) ) ; itel = 0 ; ztel = 0
    endif

    do k  = 1,ndx
       xmn = minval(nd(k)%x) ; xmx = maxval(nd(k)%x)
       ymn = minval(nd(k)%y) ; ymx = maxval(nd(k)%y)
       if (xs <= xmx .and. xs >= xmn .and. ys <= ymx .and. ys >= ymn ) then
          nn = size(nd(k)%x)
          call PINPOK(Xs, Ys, Nn, nd(k)%x, nd(k)%y, IN, jins, dmiss)
          if (IN == 1) then
             itel(k) = itel(k) + 1
             ztel(k) = ztel(k) + zs
             return
          endif
       endif
    enddo
 else
    do k  = 1,ndx
       if (itel(k) .ne. 0) then
          bl(k) =  ztel(k) / dble( itel(k) )
       endif
    enddo
    if (allocated(itel) ) deallocate (itel, ztel)
 endif

 end subroutine pixcount
