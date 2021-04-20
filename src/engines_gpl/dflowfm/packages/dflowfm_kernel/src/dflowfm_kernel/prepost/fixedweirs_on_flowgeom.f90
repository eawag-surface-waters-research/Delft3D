!> TODO: update setfixedweirs to use fxw
subroutine fixedweirs_on_flowgeom()
    use m_fixedweirs
    implicit none

    integer, dimension(:), allocatable :: idum

    integer :: ic

    allocate(idum(1))
    idum = 0

    do ic=1,nfxw
        call crspath_on_flowgeom(fxw(ic),1,0,1,idum, 0, 1)
    end do
end subroutine fixedweirs_on_flowgeom
