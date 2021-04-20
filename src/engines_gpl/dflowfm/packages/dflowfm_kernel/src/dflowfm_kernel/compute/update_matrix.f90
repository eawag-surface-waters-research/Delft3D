! update overlapping ghost-parts of matrix
 subroutine update_matrix(ierror)
!    use m_flow
    use m_flowgeom
    use m_reduce
    use m_partitioninfo
    use m_alloc
    implicit none

    integer, intent(out) :: ierror  ! error (1) or not (0)

    integer              :: i, k, L

    ierror = 0

!   allocate if necessary
    call realloc(workmatbd, (/2, Ndx/), keepExisting=.true., fill=0d0)
    call realloc(workmatc, (/2, Lnx/), keepExisting=.true., fill=0d0)

!   fill work arrays
    do i=1,numsend_sall
       k=isendlist_sall(i)
       workmatbd(1,k) = bbr(k)
       workmatbd(2,k) = ddr(k)
    end do

    do i=1,numsend_u
       L=isendlist_u(i)
       workmatc(1,L) = ccr(Lv2(L))
       workmatc(2,L) = 1d0   ! used to "undo" orientation correction in update_ghosts(ITYPE_U,...)
    end do

!   update work arrays
    call update_ghosts(ITYPE_SALL,2,Ndx,workmatbd,ierror)
    call update_ghosts(ITYPE_U,2,Lnx,workmatc,ierror)

!   copy from work array
    do i=1,numghost_sall
       k=ighostlist_sall(i)
       bbr(k) = workmatbd(1,k)
       ddr(k) = workmatbd(2,k)
    end do

    do i=1,numghost_u
       L=iabs(ighostlist_u(i))
       ccr(Lv2(L)) = workmatc(1,L) * workmatc(2,L)
    end do

    return
 end subroutine update_matrix
