 subroutine setzcs()
    use m_flow
    use m_flowgeom
    implicit none

    integer :: kk, k, kb, kt

    if ( .not. allocated (zcs) ) call realloc(zcs, Ndkx)
    do kk=1,Ndx
       call getkbotktop(kk,kb,kt)
       do k=kb,kt
          zcs(k) = 0.5d0*(zws(k)+zws(k-1))
       end do
    end do

    return
 end subroutine setzcs
