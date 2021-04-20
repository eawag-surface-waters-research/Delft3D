 subroutine reabl(mout)                              ! read bottom level
 use m_flowgeom
 use M_samples
 use m_missing
 implicit none
 integer            :: mout
 character(len=256) :: rec

 integer            :: K, L1
 integer            :: ndxr
 double precision   :: rd

 CALL reasam(mout,0)

 bl = dmiss

 call interpdivers(1)

 call delsam(-1) ! deallocate

 return

888 call qnreaderror('trying to read nr of internal flow nodes but getting',rec,mout)
 call doclose(mout)

   end subroutine reabl
