 subroutine csmfinebnds2unstruc()
 implicit none
 double precision x,y,amp,phas, x0, y0, d
 character fnam*132, cmp*8, rec*132
 integer :: mou2, k, kkk, L, minp, mout

 mou2 = 0

 do k = 1,3
    kkk = 0
    if (k == 1) fnam = 'zuid'
    if (k == 2) fnam = 'west'
    if (k == 3) fnam = 'noord'

    L  = len_trim(fnam)

    call oldfil (minp, fnam(1:L)//'rand_new10')
    call newfil (mout, fnam(1:L)//'.pli')
    write(mout,'(a)') 'bl01'
    if (k == 1) write(mout,'(a)') ' 23  2 '
    if (k == 2) write(mout,'(a)') ' 85  2 '
    if (k == 3) write(mout,'(a)') ' 99  2 '


8      continue
       read(minp,'(a)',end = 999) rec
       read (rec,*) cmp,y,x,d,d,amp,phas
       if (cmp == 'Q1') then
          if (mou2>0) call doclose(mou2)
          write(mout, *) x, y
          kkk = kkk + 1
          write(fnam(l+1:l+1) , '(a)'   )  '_'
          write(fnam(l+2:l+5) , '(i4.4)') kkk
          call newfil (mou2, fnam(1:L+5)//'.cmp')
          write(mou2,'(a)') '* COLUMNN=3'
          write(mou2,'(a)') '* COLUMN1=Period (min) or Astronomical Componentname'
          write(mou2,'(a)') '* COLUMN2=Amplitude (m)'
          write(mou2,'(a)') '* COLUMN3=Phase (deg)'
        endif

       if (cmp == 'PHI1')    cmp = 'FI1'
       if (cmp == 'LAMBDA2') cmp = 'LABDA2'
       if (cmp == 'RHO1')    cmp = 'RO1'

       write(mou2,'(a,2f14.6)') cmp, 0.01d0*amp, phas
       goto 8



999 call doclose(minp)
    call doclose(mout)
    call doclose(mou2); mou2 = 0
 enddo
 end subroutine csmfinebnds2unstruc
