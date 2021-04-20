!> Fourier Analysis, copied from Delft3D:
!! Opens and reads .fou file (md_foufile, specified in the mdu)
!! and prepares the gd_fourier structure
subroutine flow_fourierinit()
use m_fourier_analysis
use m_transport, only: NUMCONST, ISALT, ITEMP
use unstruc_model, only: md_foufile, md_tunit, getoutputdir
use unstruc_files, only : defaultFilename
use m_flow, only: kmxd
use m_physcoef, only: ag
use m_flowtimes, only: tstart_user, tstop_user

implicit none
integer  :: minp, ierr
logical  :: success
call oldfil(minp, md_foufile)
call fouini(minp, success, ag, md_tunit,'S')
FouOutputFile = trim(getoutputdir()) // defaultFilename('fou')

call alloc_fourier_analysis_arrays()
call reafou(minp, md_foufile, kmxd, NUMCONST, ISALT, ITEMP, tstart_user, tstop_user, success)
call doclose(minp)

end subroutine flow_fourierinit
