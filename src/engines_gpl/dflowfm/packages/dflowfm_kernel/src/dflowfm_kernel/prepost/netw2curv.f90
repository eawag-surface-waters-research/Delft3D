!> generate curvi-linear grid from net, growing from (xp,yp)
subroutine netw2curv(xp,yp)
 use m_netw
 use m_grid
 use m_alloc
 use m_missing
 use unstruc_messages
 use gridoperations

 implicit none

 double precision                   :: xp, yp       !< coordinates of starting point

 integer                            :: ierr

 integer, dimension(:), allocatable :: ic, jc       ! indices (i,j) of the nodes

! integer, parameter                 :: IMISS = -999999

 integer                            :: in, link, iexit

 logical                            :: lremovelink

!---------------------------------------------------------
! get the cells
!---------------------------------------------------------
 call findcells(0)

!---------------------------------------------------------
! allocate and initialize indices arrays
!---------------------------------------------------------
 allocate(ic(numk), stat=ierr)
 call aerr('ic(numk)', ierr, numk)

 allocate(jc(numk), stat=ierr)
 call aerr('jc(numk)', ierr, numk)

 ic = IMISS
 jc = IMISS
 in = 0

maindo:do
    if ( nump.lt.1 ) exit maindo

! allocate and initialize cellmask array
    call realloc(cellmask, numP)

! allocate and initialize ijc array
    if ( allocated(ijc) ) deallocate(ijc)
    call realloc(ijc, (/ 3, 3 /), (/ 0, 0 /), fill=IMISS)

!---------------------------------------------------------
! assigns node-based indices (ic,jc)
!---------------------------------------------------------
    call assign_icjc(xp,yp, ic, jc, iexit)
    if (iexit.ne.1) exit maindo
!---------------------------------------------------------
! generate the curvi-grid
!---------------------------------------------------------
    call makecurvgrid(ic, jc)

!---------------------------------------------------------
! remove curvi-grid nodes from net
!    firstly, disable links that do not neighbor a
!       non-curvi-grid cell: set kn to 0
!    secondly, rely on setnodadm to remove the proper nodes
!---------------------------------------------------------
     do link=1,numL
        lremovelink = .false.
        if ( lnn(link).gt.0)   &
           lremovelink = ( cellmask(lne(1,link)).eq. -1 )
        if ( lnn(link).gt.1)   &
           lremovelink = lremovelink .and. ( cellmask(lne(2,link)).eq. -1 )
        if ( lremovelink) kn(1:2, link) = 0
     end do

    call setnodadm(0)
    exit
 end do maindo
!---------------------------------------------------------
! done, clean up
!---------------------------------------------------------
 deallocate(ic)
 deallocate(jc)

!set network status
netstat = NETSTAT_CELLS_DIRTY

end subroutine netw2curv
