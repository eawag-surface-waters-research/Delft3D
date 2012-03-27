subroutine inibcparl(nto    , mnbnd     ,typbnd    , &
                   & guu       ,gvv       ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!!--description-----------------------------------------------------------------
!
!    Function: When running parallel,
!              the start/end pivot points of an open boundary may lay outside
!              the current partition.
!              Store the distance from the start pivot to the first point inside
!              this partition in dist_pivot_part(start_pivot,n), zero when inside
!              Store the distance from the end   pivot to the last point inside
!              this partition in dist_pivot_part(  end_pivot,n), zero when inside
!     Method used: 
!     - Collect needed gu/vu/v of the full global model in the master node
!     - Broadcast it to all partitions
!     - Each partition calculates the needed distances
!     - Deallocate the temporary full global model arrays
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use dfparall
    use dffunctionals
    !
    use globaldata
    !
    implicit none
    !
    ! Enumeration
    !
    integer, parameter :: start_pivot = 1
    integer, parameter ::   end_pivot = 2
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                 , pointer :: kmax           !  Description and declaration in esm_alloc_int.f90
    integer                 , pointer :: mmax           !  Description and declaration in esm_alloc_int.f90
    integer                 , pointer :: nmaxus         !  Description and declaration in esm_alloc_int.f90
    integer                 , pointer :: mfg
    integer                 , pointer :: mlg
    integer                 , pointer :: nfg
    integer                 , pointer :: nlg
    integer                 , pointer :: mmaxgl
    integer                 , pointer :: nmaxgl
    integer , dimension(:)  , pointer :: bct_order       !  Description and declaration in bcdat.igs
    integer , dimension(:,:), pointer :: mnbnd_global    !  Description and declaration in bcdat.igs
    integer                 , pointer :: lundia
    integer                 , pointer :: ntof            !  Description and declaration in dimens.igs
    integer                 , pointer :: ntoq            !  Description and declaration in dimens.igs
    real(fp), dimension(:,:), pointer :: dist_pivot_part !  Description and declaration in bcdat.igs
!
! Global variables
!
    integer                                                           , intent(in)  :: nto    !!  Max. number of open boundaries
    integer      , dimension(7, nto)                                  , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    character(1) , dimension(nto)                                     , intent(in)  :: typbnd !  Description and declaration in esm_alloc_char.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                :: i
    integer                                :: incx
    integer                                :: incy
    integer                                :: istat
    integer                                :: j
    integer                                :: maxinc
    integer                                :: msta
    integer                                :: mend
    integer                                :: nsta
    integer                                :: nend
    integer                                :: n
    integer , dimension(4,0:nproc-1)       :: iarrc        ! array containing collected grid indices 
    integer                                :: lenlo        ! length of field of current subdomain
    integer                                :: lengl        ! length of field containing collected data
    integer                                :: mgg          ! M-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering
    integer                                :: ngg          ! N-coord. of the actual open boundary point, which may differ from the ori- ginal position due to grid staggering
    integer                                :: lb           ! lowerboundary of loopcounter
    integer                                :: ub           ! upperboundary of loopcounter
    integer , dimension(0:nproc-1)         :: mf           ! first index w.r.t. global grid in x-direction
    integer , dimension(0:nproc-1)         :: ml           ! last index w.r.t. global grid in x-direction
    integer , dimension(0:nproc-1)         :: nf           ! first index w.r.t. global grid in y-direction
    integer , dimension(0:nproc-1)         :: nl           ! last index w.r.t. global grid in y-direction
    integer                                :: pivot        ! loops over start_pivot and end_pivot
    logical                                :: horiz
    real(fp)                               :: distx
    real(fp)                               :: disty
    real(sp), dimension(:,:), allocatable  :: guu_global   ! temporary array storing guu of the full global domain
    real(sp), dimension(:,:), allocatable  :: gvv_global   ! temporary array storing gvv of the full global domain
!
!! executable statements -------------------------------------------------------
!
    if (.not. parll) return
    !
    mmax         => gdp%d%mmax
    nmaxus       => gdp%d%nmaxus
    kmax         => gdp%d%kmax
    ntof         => gdp%d%ntof
    ntoq         => gdp%d%ntoq
    mfg          => gdp%gdparall%mfg
    mlg          => gdp%gdparall%mlg
    nfg          => gdp%gdparall%nfg
    nlg          => gdp%gdparall%nlg
    nmaxgl       => gdp%gdparall%nmaxgl
    mmaxgl       => gdp%gdparall%mmaxgl
    bct_order    => gdp%gdbcdat%bct_order
    mnbnd_global => gdp%gdbcdat%mnbnd_global
    lundia       => gdp%gdinout%lundia
    !
    allocate(gdp%gdbcdat%dist_pivot_part(2, nto), stat=istat)
    dist_pivot_part => gdp%gdbcdat%dist_pivot_part
    if (istat /= 0) then
       call prterr(lundia, 'P004', 'memory alloc error in inibcparl(dist_pivor_part)')
       call d3stop(1, gdp)
    endif
    dist_pivot_part = 0.0_fp
    !
    call dfsync(gdp)
    call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
       &                 nf, nl, mf, ml, iarrc, lengl, lenlo, gdp )
    !
    ! broadcast LOCAL grid indices to ALL partitions
    ! so every partition knows the dimensions and positions
    ! of the other partitions in the global domain
    !
    call dfbroadc( iarrc, 4*nproc, dfint, gdp )
    call dfbroadc( nf, nproc, dfint, gdp )
    call dfbroadc( nl, nproc, dfint, gdp )
    call dfbroadc( mf, nproc, dfint, gdp )
    call dfbroadc( ml, nproc, dfint, gdp )
    !
                  allocate(guu_global(1:nmaxgl,1:mmaxgl), stat=istat)
    if (istat==0) allocate(gvv_global(1:nmaxgl,1:mmaxgl), stat=istat)
    if (istat /= 0) then
       call prterr(lundia, 'P004', 'memory alloc error in inibcparl(guu_global/gvv_global)')
       call d3stop(1, gdp)
    endif
    !
    ! Collect all guu values from all partitions in the (single precision) array glbarr2 in the master partition
    !
    call dfgather(guu,nf,nl,mf,ml,iarrc,gdp)
    if (inode == master) then
       guu_global = glbarr2
    endif
    !
    ! The master partition broadcasts this guu array to all partitions
    !
    call dfbroadc (guu_global, (nmaxgl)*(mmaxgl), dfreal, gdp )
    !
    ! Collect all gvv values from all partitions in the (single precision) array glbarr2 in the master partition
    !
    call dfgather(gvv,nf,nl,mf,ml,iarrc,gdp)
    if (inode == master) then
       gvv_global = glbarr2
    endif
    !
    ! The master partition broadcasts this gvv array to all partitions
    !
    call dfbroadc (gvv_global, (nmaxgl)*(mmaxgl), dfreal, gdp )
    !
    ! loop over all boundaries that are (partly) inside this partition
    !
    do n=1,nto - ntof - ntoq
       !
       ! loop over start_pivot (1) and end_pivot (2)
       do pivot=start_pivot,end_pivot
          if (pivot == start_pivot) then
             !
             ! (msta,nsta): boundary pivot point outside partition
             ! (mend,nend): first point along boundary inside partition
             !
             msta   = mnbnd_global(1, bct_order(n))
             nsta   = mnbnd_global(2, bct_order(n))
             mend   = mnbnd(1, n)+mfg-1
             nend   = mnbnd(2, n)+nfg-1
          else ! end_pivot
             !
             ! (msta,nsta): first point along boundary inside partition
             ! (mend,nend): boundary pivot point outside partition
             !
             msta   = mnbnd(3, n)+mfg-1
             nsta   = mnbnd(4, n)+nfg-1
             mend   = mnbnd_global(3, bct_order(n))
             nend   = mnbnd_global(4, bct_order(n))
          endif
          incx   = mend - msta
          incy   = nend - nsta
          maxinc = max(abs(incx), abs(incy))
          incx   = incx/max(1, maxinc)
          incy   = incy/max(1, maxinc)
          select case (typbnd(n))
             case ('Z')
                ! Waterlevel boundary, ityp=2
                do while ((msta<=mend) .and. (nsta<=nend))
                   !
                   ! This loop is NOT entered when the boundary is completely inside this partition
                   ! This loop is entered for horizontal (nsta=nend), vertical (msta=mend) and diagonal (msta!=mend,nsta!=nend) boundaries
                   ! The easiest way to check that we reached the first point inside the partition is:
                   if ((msta==mend) .and. (nsta==nend)) exit
                   msta  = msta + incx
                   nsta  = nsta + incy
                   !
                   ! Pythagoras is used to calculate the distance from xz,yz(nsta-incy,msta-incx) to xz,yz(nsta,msta),
                   ! using distx = |xz,yz(nsta     ,msta-incx) - xz,yz(nsta,msta     )|
                   !       disty = |xz,yz(nsta-incy,msta-incx) - xz,yz(nsta,msta-incx)|
                   ! Assumption: the grid is more or less rectangular locally
                   !
                   if (incx == 0) then
                      distx = 0.0_fp
                   else
                      if (mnbnd(7,n) == 4) then
                         !
                         ! Boundary on top side of the domain
                         ! The correct gvv is one index down (staggered grid)
                         !
                         ngg = nsta - 1
                      else
                         ngg = nsta
                      endif
                      !
                      ! General case: incx > 1
                      ! The distance in the x-direction between the two zeta points is:
                      ! 0.5*gvv(lowest_point) + sum(gvv(intermediate_points)) + 0.5*gvv(highest_point)
                      !
                      distx = 0.5_fp * (real(gvv_global(ngg,msta-incx),fp)+real(gvv_global(ngg,msta),fp))
                      lb = min(msta-incx,msta)
                      ub = max(msta-incx,msta)
                      do i=lb+1,ub-1
                         distx = distx + real(gvv_global(ngg,i),fp)
                      enddo
                   endif
                   distx = distx*distx
                   if (incy == 0) then
                      disty = 0.0_fp
                   else
                      if (mnbnd(7,n) == 3) then
                         !
                         ! Boundary on right side of the domain
                         ! The correct guu is one index down (staggered grid)
                         !
                         mgg = msta - incx - 1
                      else
                         mgg = msta - incx
                      endif
                      !
                      ! General case: incy > 1
                      ! The distance in the y-direction between the two zeta points is:
                      ! 0.5*guu(lowest_point) + sum(guu(intermediate_points)) + 0.5*guu(highest_point)
                      !
                      disty = 0.5_fp * (real(guu_global(nsta-incy,mgg),fp)+real(guu_global(nsta,mgg),fp))
                      lb = min(nsta-incy,nsta)
                      ub = max(nsta-incy,nsta)
                      do i=lb+1,ub-1
                         disty = disty + real(guu_global(i,mgg),fp)
                      enddo
                   endif
                   disty = disty*disty
                   dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) + sqrt(distx + disty)
                enddo
             case ('T')
                !
                ! Distances are not used at Total discharge boundaries
                !
             case default ! 'C', 'Q', 'R'
                mgg   = msta
                ngg   = nsta
                if (msta == mend) then
                   !
                   ! Opening in the vertical direction
                   ! When on the right side of the domain, the correct guu is one index down (staggered grid)
                   !
                   horiz = .false.
                   if (mnbnd(7,n) == 3) mgg = mgg - 1
                else
                   !
                   ! Opening in the horizontal direction
                   ! When on the top side of the domain, the correct gvv is one index down (staggered grid)
                   !
                   horiz = .true.
                   if (mnbnd(7,n) == 4) ngg = ngg - 1
                endif
                !
                ! Distance between points calculated
                ! When MSTA/NSTA are updated first use lower GVV/GUU
                !
                do while ((msta<=mend) .and. (nsta<=nend))
                   !
                   ! This loop is NOT entered when the boundary is completely inside this partition
                   ! This loop is ONLY entered for horizontal (nsta=nend,incx=1) and vertical (msta=mend,incy=1) boundaries,
                   ! not for diagonal boundaries (compare with case ('Z'))
                   ! The easiest way to check that we reached the first point inside the partition is:
                   if ((msta==mend) .and. (nsta==nend)) exit
                   msta = msta + incx
                   nsta = nsta + incy
                   if (horiz) then
                      !
                      ! The distance between xz,yz(ngg,msta) and xz,yz(ngg,msta-1) is:
                      ! 0.5*guu(ngg,msta) + 0.5*guu(ngg,msta-1)
                      !
                      dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) &
                                               &  + 0.5_fp * (real(gvv_global(ngg,msta),fp)+real(gvv_global(ngg,msta-incx),fp))
                   else
                      !
                      ! The distance between xz,yz(nsta,mgg) and xz,yz(nsta-1,mgg) is:
                      ! 0.5*guu(nsta,mgg) + 0.5*guu(nsta-1,mgg)
                      !
                      dist_pivot_part(pivot,n) = dist_pivot_part(pivot,n) &
                                               & + 0.5_fp * (real(guu_global(nsta,mgg),fp)+real(guu_global(nsta-incy,mgg),fp))
                   endif
                enddo
          end select
       enddo ! pivot
    enddo ! boundary n
    !
    ! mnbnd_global was stored for usage in this subroutine and can be deallocated now
    !
    deallocate (gdp%gdbcdat%mnbnd_global, stat=istat)
    deallocate (guu_global, stat=istat)
    deallocate (gvv_global, stat=istat)
end subroutine inibcparl
