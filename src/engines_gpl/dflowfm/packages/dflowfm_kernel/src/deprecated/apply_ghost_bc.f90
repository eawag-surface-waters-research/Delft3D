 !> apply Dirichlet conditions to non-overlapping ghost cells (i.e. effectively remove from the system)
 subroutine apply_ghost_bc(ierror)
    use m_flow
    use m_flowgeom
    use m_reduce
    use m_partitioninfo
    use m_alloc
    implicit none

    integer, intent(out) :: ierror  ! error (1) or not (0)

    integer              :: i, k, kother, L, LL

    do i=1,nghostlist_snonoverlap(ndomains-1)
       k = ighostlist_snonoverlap(i)
       do LL=1,nd(k)%lnx
          L = iabs(nd(k)%ln(LL))
          kother = ln(1,L)+ln(2,L)-k
          ddr(kother) = ddr(kother)-ccr(Lv2(L))*s1(k)
          ccr(Lv2(L)) = 0
          ddr(k) = ddr(k)-ccr(Lv2(L))*s1(k)
       end do
    end do

    ierror = 0

    return
 end subroutine apply_ghost_bc
