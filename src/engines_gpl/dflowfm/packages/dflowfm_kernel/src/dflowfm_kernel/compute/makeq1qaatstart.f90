 subroutine makeq1qaAtStart()
 use m_flow
 use m_flowgeom
 use unstruc_model, only: md_restartfile
 use unstruc_netcdf
 implicit none

 integer :: L, idfile, idvar, ierr, jaq1, jaqa

 jaq1 = 1
 jaqa = 1

 ! When it is a restart simulation, check if the restart file (rst/map) contains variable 'q1'/'qa'.
 ! If it contains 'q1'/'qa', then 'q1'/'qa' has been read before, and no need to compute it again here.
 if (len_trim(md_restartfile) > 0) then
    ierr = unc_open(md_restartfile, nf90_nowrite, idfile)
    call check_error(ierr, 'file '''//trim(md_restartfile)//'''')

    ierr = nf90_inq_varid(idfile, 'q1', idvar)
    if (ierr == nf90_noerr) then
       jaq1 = 0
    end if

    ierr = nf90_inq_varid(idfile, 'qa', idvar)
    if (ierr == nf90_noerr) then
       jaqa = 0
    end if
 end if

 if (jaq1 == 1) then
    do L = 1,lnx
       if (hu(L) > 0) then
          q1(L) = au(L)*( teta(L)*u1(L) + (1d0-teta(L))*u0(L) )
       else
          q1(L) = 0
       endif
    enddo
 end if

 if (jaqa == 1) then
    do L = 1,lnx
       if (hu(L) > 0) then
          qa(L) = au(L)*u1(L)
       else
          qa(L) = 0
       endif
    enddo
 end if

   end subroutine makeq1qaAtStart
