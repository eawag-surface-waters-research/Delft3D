  subroutine renumber_cutcellmasks(perm)
     use network_data, only: numL
     use m_cutcells
     implicit none

     integer,          dimension(numL), intent(in)  :: perm !< permuation array

     integer,          dimension(:),    allocatable :: idxL_bak
     double precision, dimension(:),    allocatable :: xdxL_bak, ydxL_bak
     integer,          dimension(:),    allocatable :: pdxL_bak

     integer                                        :: i, ii, L, LL, num

     if ( jastored.ne.1 ) then
        return ! nothing to do
     end if

!    allocate
     allocate(idxL_bak(numL+1))
     num = idxL(numL+1)-1
     allocate(xdxL_bak(num))
     allocate(ydxL_bak(num))
     allocate(pdxL_bak(num))

!    copy
     do L=1,numL+1
        idxL_bak(L) = idxL(L)
     end do

     do i=1,num
        xdxL_bak(i) = xdxL(i)
        ydxL_bak(i) = ydxL(i)
        pdxL_bak(i) = pdxL(i)
     end do

!    apply permutation
     idxL_bak(1) = 1
     do LL=1,numL
        L = perm(LL)
        num = idxL_bak(L+1)-idxL_bak(L)
        idxL(LL+1) = idxL(LL) + num
        ii = idxL_bak(L)
        do i=idxL(LL),idxL(LL+1)-1
           xdxL(i) = xdxL_bak(ii)
           ydxL(i) = ydxL_bak(ii)
           pdxL(i) = pdxL_bak(ii)
           ii = ii+1
        end do
     end do

!    deallocate
     if ( allocated(idxL_bak) ) deallocate(idxL_bak)
     if ( allocated(xdxL_bak) ) deallocate(xdxL_bak)
     if ( allocated(ydxL_bak) ) deallocate(ydxL_bak)
     if ( allocated(pdxL_bak) ) deallocate(pdxL_bak)

     return
  end subroutine renumber_cutcellmasks
