!> Sets initial water level based on a 'flood fill' that originates from
!! the active sample points.
!!
!! The active samples are used as starting points, with their z-values as
!! initial water level. This level spreads out to all surrounding cells,
!! until a higher bottom (shore) is encountered, or a flood front from one
!! of the other samples.
!! Also used by flow_flowinit() for the <tt>WaterLevIniFile</tt> from the MDU.
subroutine flow_initfloodfill()
use m_samples
use m_flow
use m_flowgeom
use m_GlobalParameters, only: INDTP_ALL
use m_alloc
use kdtree2Factory
implicit none

integer :: i, inod, iL, Lf, k, k2, nx
integer, allocatable :: kcsfill(:)
integer, allocatable :: ndroot(:)
integer, allocatable :: ndqueue(:)
integer, dimension(:), allocatable :: inodes
double precision, allocatable :: s1queue(:)
integer :: iqcur, iqtail
integer :: ierror, knew
integer :: jakdtree

jakdtree = 1

if (ndx <= 0) then
    return
end if

! Each node is visited at most once: work array size <= ndx.
nx = ns+ndx-1
call realloc(kcsfill, nx, fill = 0)
call realloc(ndqueue, nx, fill = 0)
call realloc(s1queue, nx, fill = 0d0)

iqcur = 0  !< Index of current node in queue.
iqtail = 0 !< Index of most recently added element in work queue.

!find flowcells
if ( jakdtree.eq.1 ) then
   allocate(inodes(Ns))
   call find_flowcells_kdtree(treeglob,Ns,xs,ys,inodes,1,INDTP_ALL, ierror)
end if

if ( ierror.ne.0 ) then
   if ( allocated(inodes) ) deallocate(inodes)
   jakdtree = 0
end if

! First associate all samples with a single flow node (1D or 2D) and put them in work queue.
do i = 1,NS
    if ( jakdtree.eq.1 ) then
       k = inodes(i)
    else
    call in_flowcell(xs(i), ys(i), k)
    end if

    if (k > 0  ) then
        if ( zs(i) > bl(k) ) then
           s1(k)      = zs(i)
           kcsfill(k) = 1

           iqtail          = iqtail+1
           ndqueue(iqtail) = k
           s1queue(iqtail) = zs(i)
        endif
    end if
end do

if ( iqtail.eq.0 ) return


! Loop over flow node queue: for each node, water level is already set,
! but now also visit its neighbouring flow nodes (this is the 'flood' step).
iqcur = 0
do
    iqcur = iqcur + 1
    k = ndqueue(iqcur)
    if (k == 0) then
       exit
    endif
    do iL=1,nd(k)%lnx
        Lf = abs(nd(k)%ln(iL))
        if (s1queue(iqcur) < minval(bob(:,Lf))) cycle ! Water level lower than link's bottom level, cannot flood across this link.
        k2 = ln(1,Lf)
        if (k2 == k) then
            k2 = ln(2, Lf)
        end if

        if (kcsfill(k2) == 1) then
        !   Two flood areas meet: average waterlevel on their interface
            s1(k2) = .5d0*(s1(k2) + s1queue(iqcur))
        else if (kcsfill(k2) == 0) then
        !   Newly flooded point: set waterlevel and enqueue it for further flooding.
            s1(k2) = s1queue(iqcur)
            kcsfill(k2) = 1
            iqtail = iqtail+1
            ndqueue(iqtail) = k2
            s1queue(iqtail) = s1queue(iqcur)
        end if
    end do
    ! All reachable nodes have been visited, rest (if any) remains unflooded at s1ini:
    if (iqcur==iqtail) exit
end do

! Update water depth explicitly here, for direct plotting.
! NOTE: all other quantities a1, hu, etc. need to be updated by flow_initimestep().
hs = s1-bl

deallocate(kcsfill,ndqueue,s1queue)

if ( allocated(inodes) ) deallocate(inodes)

end subroutine flow_initfloodfill
