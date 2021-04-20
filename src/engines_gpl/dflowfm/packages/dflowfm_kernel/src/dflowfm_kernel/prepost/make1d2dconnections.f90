 subroutine make1D2Dconnections()
 use network_data, only: imake1d2dtype, searchRadius1D2DLateral, I1D2DTP_1TO1, I1D2DTP_1TON_EMB, I1D2DTP_1TON_LAT
 use geometry_module
 use gridoperations
 use m_samples
 use m_polygon
 use m_sferic, only: jsferic, jasfer3D
 integer :: minp, ierr
 character FILNAM*86
 minp = 0
 Filnam = '*.pliz, *.xyz'
 select case (imake1d2dtype)
 case (I1D2DTP_1TO1) ! 0: HK
    CALL FILEMENU(minp,FILNAM,ierr)
    if (ierr == -1) then
       ierr = make1D2Dinternalnetlinks(xpl(1:npl), ypl(1:npl), zpl(1:npl))
    else if (index(filnam, '.pliz' ) > 0)  then
       call reapol (minp, 0)
       call make1D2Droofgutterpipes()
    else if (index(filnam, '.xyz'  ) > 0)  then
       call reasam(minp, 0)
       call make1D2Dstreetinletpipes()
    endif
    call doclose(minp)
 case (I1D2DTP_1TON_EMB)
    ierr = ggeo_make1D2Dembeddedlinks(jsferic, jasfer3D)
 case (I1D2DTP_1TON_LAT)
    ierr = ggeo_make1D2DRiverLinks(jsferic, jasfer3D, searchRadius1D2DLateral)
 case default
    call QNERROR('Invalid 1D2D algorithm selected. Please check your network parameters.', '', '')
 end select

 end subroutine make1D2Dconnections
