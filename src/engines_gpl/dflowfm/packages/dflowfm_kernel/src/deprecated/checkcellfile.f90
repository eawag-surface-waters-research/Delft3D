 subroutine checkcellfile(mout)                      ! check first two lines for consistency
 use m_netw
 use m_flowgeom

 implicit none
 integer             :: mout, numkr, numlr, L1
 character (len=256) :: rec

 read(mout,'(A)',end = 999) rec
 L1 = index(rec,'=') + 1
 read (rec(L1:), *, err = 888) numkr
 if (numkr .ne. numk) then
    call doclose(mout) ; mout = 0 ; return
 endif

 read(mout,'(A)',end = 999) rec
 L1 = index(rec,'=') + 1
 read (rec(L1:), *, err = 777) numlr
 if (numLr .ne. numL) then
    call doclose(mout) ; mout = 0 ; return
 endif
 return

 999 call    eoferror(mout)
 888 call qnreaderror('trying to read nr of net nodes but getting',rec,mout)
 777 call qnreaderror('trying to read nr of net links but getting',rec,mout)

 end subroutine checkcellfile
