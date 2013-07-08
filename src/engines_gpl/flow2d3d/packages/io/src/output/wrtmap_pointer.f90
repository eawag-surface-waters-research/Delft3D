module wrtmap_pointer
    
    contains
    
    subroutine wrtmap_nmkl_ptr(fds, grpnam_in, uindex, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ul,ierr, varptr, varnam_in, kfmin, kfmax)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr4, dfgather, dfgather_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                                    :: ierr
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: lk            ! lowerbound dim3(0 or 1)
    integer                                                                      , intent(in)  :: uk            ! upperbound dim3(kmax or kmax+1)
    integer                                                                      , intent(in)  :: kmaxout       ! length of smlay
    integer                                                                      , intent(in)  :: ul            ! upperbound dim4
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer      , dimension(3,5)                                                , intent(in)  :: uindex
    integer      , dimension(:)                                                  , intent(in)  :: smlay
    integer      , dimension(:,:)                                                , intent(in)  :: kfmin
    integer      , dimension(:,:)                                                , intent(in)  :: kfmax
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam_in
    character(*)                                                                 , intent(in)  :: grpnam_in
    ! local
    integer                                       :: istat
    integer                                       :: namlen
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    character(16)                                 :: varnam
    character(16)                                 :: grpnam
    integer                        , external     :: putelt
    ! body
    if (associated(varptr)) then
       call wrtmap_nmkl(fds, grpnam_in, uindex, nf, nl, mf, ml, iarrc, gdp, smlay, &
                      & kmaxout, lk, uk, ul,ierr, varptr, varnam_in, kfmin, kfmax)
    else
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       namlen = min (16,len(grpnam_in))
       grpnam = grpnam_in(1:namlen)
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout,ul ), stat = istat )
       rbuff4(:,:,:,:) = -999.0_fp
       if (parll) then
          call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff4, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
       endif   
       deallocate(rbuff4)
       if (inode == master) then
          ierr = putelt(fds, grpnam, varnam, uindex, 1, glbarr4)
       endif
    endif
    end subroutine wrtmap_nmkl_ptr

end module wrtmap_pointer