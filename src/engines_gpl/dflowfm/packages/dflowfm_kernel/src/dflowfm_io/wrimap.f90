subroutine wrimap(tim)
    use m_flow
    use m_flowtimes
    use m_observations
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    use m_dad, only: dad_included
    use m_fm_update_crosssections, only: fm_update_mor_width_mean_bedlevel
    use m_flowgeom, only: ndx2d, ndxi

    implicit none
    double precision, intent(in) :: tim

    ! locals
    integer            :: ierr
    integer            :: i
    integer            :: len
    integer, save      :: mtecfil = 0
    character(len=256) :: filnam
    logical            :: unitused
    double precision, save :: curtime_split = 0d0 ! Current time-partition that the file writer has open.
    integer            :: ndx1d, ndims
    integer            :: jabndnd

    ! Another time-partitioned file needs to start, reset iteration count (and file).
    if (ti_split > 0d0 .and. curtime_split /= time_split0) then
        mapids%id_tsp%idx_curtime = 0
        it_map       = 0
        it_map_tec   = 0
        curtime_split = time_split0
    end if

     if ( md_mapformat.eq.IFORMAT_NETCDF .or. md_mapformat.eq.IFORMAT_NETCDF_AND_TECPLOT .or. md_mapformat == IFORMAT_UGRID) then   !   NetCDF output
       if (mapids%ncid /= 0 .and. ((md_unc_conv == UNC_CONV_UGRID .and. mapids%id_tsp%idx_curtime == 0) .or. (md_unc_conv == UNC_CONV_CFOLD .and. it_map == 0))) then
           ierr = unc_close(mapids%ncid)
           mapids%ncid = 0
       end if

       if (mapids%ncid/=0) then  ! reset stord ncid to zero if file not open
		  ierr = nf90_inquire(mapids%ncid, ndims)
		  if (ierr/=0) mapids%ncid = 0
       end if

       if (mapids%ncid == 0) then
           if (ti_split > 0d0) then
               filnam = defaultFilename('map', timestamp=time_split0)
           else
               filnam = defaultFilename('map')
           end if
           ierr = unc_create(filnam , 0, mapids%ncid)
           if (ierr /= nf90_noerr) then
               call mess(LEVEL_WARN, 'Could not create map file.')
               mapids%ncid = 0
           end if
       endif

       if (mapids%ncid .ne. 0) then
          if (md_unc_conv == UNC_CONV_UGRID) then
             ndx1d = ndxi - ndx2d
             if (ndx1d > 0 .and. stm_included) then
                if (stmpar%morpar%moroutput%blave) then
                   call fm_update_mor_width_mean_bedlevel()
                endif
             endif
             jabndnd = 0
             if (jamapbnd > 0) jabndnd = 1
             call unc_write_map_filepointer_ugrid(mapids,tim,jabndnd)  ! wrimap
          else
             call unc_write_map_filepointer(mapids%ncid,tim)  ! wrimap
          endif
       endif

       ierr = nf90_sync(mapids%ncid) ! Flush file
    end if

    if ( md_mapformat.eq.IFORMAT_TECPLOT .or. md_mapformat.eq.IFORMAT_NETCDF_AND_TECPLOT ) then      ! TecPlot output
       !if (mtecfil /= 0 .and. it_map_tec == 0) then
       !   call doclose(mtecfil)
       !end if

       !if (it_map_tec == 0) then
       !     if (ti_split > 0d0) then
       !         filnam = defaultFilename('tec', timestamp=time_split0)
       !     else
       !         filnam = defaultFilename('tec')
       !     end if
       !   call newfil(mtecfil, filnam)
       !endif

       !call tecplot_out(mtecfil, tim, it_map_tec==0)

!      write grid in Tecplot format only once
       if ( it_map_tec.eq.0 ) then
          filnam = defaultFilename('net.plt')
          call wrinet_tecplot(filnam)   ! do not call findcells
       end if

!      write solution in Tecplot format
       filnam = defaultFilename('map.plt', timestamp=tim)
       call wrimap_tecplot(filnam)

       it_map_tec = it_map_tec+1
    end if
 end subroutine wrimap
