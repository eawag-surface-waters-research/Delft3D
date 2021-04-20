 subroutine setcfuhi()                                 ! set friction coefficients g/C2 etc
 use m_flowtimes                                       ! sqrt(g/C2) in both in 2D and in 3D
 use m_flow
 use m_flowgeom
 use m_missing

 implicit none

 ! locals
 double precision ::  h0, dzb, cz, sixth = 1d0/6d0, frcn, z00, sqcf
 integer :: l, ll, n, kb, Lb, ifrctyp

 ! NOTE: When frcuni==0, the initial friction fields in frcu also become noneffective:
 if ( jatrt.eq.0 .and. (frcmax == 0d0 .or. ifrctypuni == -999) ) then
    cfuhi = 0 ; return
 endif
 if (jaconveyance2D >= 1 ) then ! .and. kmx <=1 ) then
    return
 endif

 if (kmx <= 1) then                                  ! 2D
    if (ifrctypuni == 4) then
       cfuhi = 0
    else

       !$OMP PARALLEL DO                             &
       !$OMP PRIVATE(L,h0,frcn,cz)
       do L = lnx1D+1,lnx
          if (hu(L) > 0) then
             if (jaconveyance2D == 0) then           ! original default
                h0 = max(epshs, 1d0 / huvli(L))
             else if (jaconveyance2D == -1) then     ! better for straight test
                h0 = max(epshs, hu(L)) ! does it whole not
             endif
             frcn = frcu(L)
             if ( frcn.gt.0d0 ) then
                call getcz(h0, frcn, ifrcutp(L), cz, L)
                cfuhi(L) = ag/(h0*cz*cz)
             else
                cfuhi(L) = 0d0
             end if
          endif
       enddo
       !$OMP END PARALLEL DO

    endif

 endif

 end subroutine setcfuhi
