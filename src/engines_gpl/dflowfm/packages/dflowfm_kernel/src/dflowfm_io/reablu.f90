 subroutine reablu(mout)                             ! read bottom level u points
 USE M_FLOWGEOM
 implicit none
 integer            :: mout
 character(len=256) :: rec

 integer            :: L, L1
 integer            :: lnxr
 double precision   :: rd
 read(mout,'(a)') rec
 L1 = index(rec,'=') + 1
 read (rec(L1:), *, err = 888) lnxr
 if (lnxr .ne. lnx) then
    call doclose(mout)
    call qnerror('nr of flowlinks read .ne. nr of flowlinks', ' ',' ')
    return
 endif

 do L  = 1,lnx
    read(mout,* ) rd, rd, blu(L)
 enddo
 call doclose(mout)

 call setbobs()

 return

888 call qnreaderror('trying to read nr of flowlinks but getting',rec,mout)
 call doclose(mout)

 end subroutine reablu
