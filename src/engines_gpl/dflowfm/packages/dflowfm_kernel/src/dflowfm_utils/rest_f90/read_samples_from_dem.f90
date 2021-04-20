      subroutine read_samples_from_dem(filnam, jadoorladen)
    use dem
    use m_missing
    use m_samples
    implicit none
    character(len=*), intent(in) :: filnam
    integer, intent(in) :: jadoorladen
    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)

    integer :: i, j, is, istep
    type(DEMInfo) :: dem_info
    integer, allocatable :: arr(:,:)
    double precision, allocatable :: xarr(:,:), yarr(:,:)
    character(len=10) :: TEX


    call savesam()
    if (jadoorladen == 0) then
        ns = 0
    end if

    call read_dem_file(trim(filnam),dem_info, xarr, yarr, arr)
    if (dem_info%rows <= 0 .or. dem_info%cols <= 0) then
        call message('No samples read from file ', filnam, ' ')
        return
    end if

    call increasesam(ns + dem_info%rows*dem_info%cols)

    WRITE(TEX,'(I10)') dem_info%rows*dem_info%cols
    CALL READYY('Filtering '//TRIM(TEX)//' Samples Points',0d0)

    istep = int(dem_info%rows/100d0)
    do i=1,dem_info%rows
        do j=1,dem_info%cols
            if (arr(i,j) == NODATA) then
                continue
            else
                ns = ns + 1
                xs(ns) = xarr(i,j)
                ys(ns) = yarr(i,j)
                zs(ns) = dble(arr(i,j))
            end if
        end do
        IF (MOD(i,istep) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(i)/dem_info%rows) )
        ENDIF
    end do
    deallocate(xarr, yarr, arr)
    CALL READYY(' ',-1d0)

    IF (NS .GT. 100000) NDRAW(32) = 7 ! Squares (faster than circles)
    IF (NS .GT. 500000) NDRAW(32) = 3 ! Small dots (fastest)

    WRITE(TEX,'(I10)') NS
    CALL READYY('Sorting '//TRIM(TEX)//' Samples Points',0d0)
    IF (NS .GT. 1) THEN
      CALL TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      call get_samples_boundingbox()
      IPSTAT = IPSTAT_OK
    END IF
    CALL READYY(' ',-1d0)
   end subroutine read_samples_from_dem
