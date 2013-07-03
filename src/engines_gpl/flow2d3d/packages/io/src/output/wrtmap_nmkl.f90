subroutine wrtmap_nmkl(fds, grpnam_in, uindex, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ul,ierr, var, varnam_in, kfmin, kfmax)
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
    integer                                                                      , intent(in)  :: ul            ! upperbound dim4
    integer                                                                      , intent(in)  :: kmaxout       ! length of smlay
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer      , dimension(3,5)                                                , intent(in)  :: uindex
    integer      , dimension(1:kmaxout)                                          , intent(in)  :: smlay
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kfmin
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kfmax
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk, ul), intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam_in
    character(*)                                                                 , intent(in)  :: grpnam_in
    ! local
    integer                                       :: istat
    integer                                       :: k
    integer                                       :: l
    integer                                       :: m
    integer                                       :: n
    integer                                       :: namlen
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    character(16)                                 :: varnam
    character(16)                                 :: grpnam
    integer                        , external     :: putelt
    ! body
    namlen = min (16,len(varnam_in))
    varnam = varnam_in(1:namlen)
    namlen = min (16,len(grpnam_in))
    grpnam = grpnam_in(1:namlen)
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmaxout, ul ), stat = istat )
    rbuff4(:,:,:,:) = -999.0_fp
    do l = 1, ul
       do k = 1, kmaxout
          do m = 1, gdp%d%mmax
             do n = 1, gdp%d%nmaxus
                if (gdp%gdprocs%zmodel) then
                   if (smlay(k)<(kfmin(n,m)-1+lk) .or. smlay(k)>kfmax(n, m)) then
                      cycle
                   endif
                endif
                rbuff4(n,m,k,l) = var(n,m,smlay(k),l)
             enddo
          enddo
       enddo
    enddo
    if (parll) then
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    else 
       call dfgather_seq(rbuff4, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif   
    deallocate(rbuff4)
    if (inode == master) then
       ierr = putelt(fds, grpnam, varnam, uindex, 1, glbarr4)
    endif
end subroutine wrtmap_nmkl
