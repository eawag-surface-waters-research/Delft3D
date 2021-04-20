 subroutine readprofilesloc(minp)
 use m_profiles
 implicit none
 integer :: minp
 character rec*256
 integer :: ierr, n

 minproflocnr = 99999999 ; maxproflocnr = 0

 n = 0
 10 read(minp,'(a)',end=999) rec
 if (rec(1:1) == '*') goto 10
 n = n + 1
 goto 10

 999 rewind(minp)
 allocate (xpr(n), ypr(n), zpr(n), npr(n), stat=ierr)

 n = 0
 20 read(minp,'(a)',end=888) rec
 if (rec(1:1) == '*') goto 20
 n = n + 1
 read(rec,*) xpr(n), ypr(n), npr(n)

 goto 20

 888 call doclose(minp)
 nproflocs = n

 end subroutine readprofilesloc
