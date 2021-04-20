!! Initialise net link based kd-tree for trachytopes or calibration
subroutine netlink_tree(phase)

    use network_data, only: numl, xk, yk, kn
    use kdtree2Factory
    use m_missing
    use m_sferic, only: jsferic, jasfer3D
    use geometry_module, only :half
    implicit none

    integer :: L, k1, k2, ierror
    integer, intent(in)  :: phase

    double precision, dimension(:), allocatable :: xuL       !< xu points on net-links
    double precision, dimension(:), allocatable :: yuL       !< yu points on net-links

    if (phase == 0) then
    !   allocation step
        allocate(xuL(numL), yuL(numL))
        xuL = DMISS
        yuL = DMISS
        do L=1,numL
           k1 = kn(1,L)
           k2 = kn(2,L)
           call half(xk(k1), yk(k1),  xk(k2), yk(k2), xuL(L), yuL(L), jsferic, jasfer3D)
        end do

        !   build kdtree
        call build_kdtree(treeglob,numL,xuL,yuL,ierror,jsferic, dmiss)
        call realloc_results_kdtree(treeglob,1)   ! safety

        if ( allocated(xuL) ) deallocate(xuL)
        if ( allocated(yuL) ) deallocate(yuL)

    endif

    if (phase == 1) then
    !   deallocation step
        if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)
    endif

end subroutine
