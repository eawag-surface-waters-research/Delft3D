 subroutine correctiefile(a)
 implicit none
 character*(*) a
 double precision :: am, ph
 character*8   cmp

 read (a,'(a)') cmp
 read (a(8:),*) am, ph

 if ( index(cmp,'O1')        .ne. 0 ) then
     am = am*1.100d0  ; ph = ph -  10d0
 else if ( index(cmp,'K1')   .ne. 0 ) then
     am = am*1.050d0  ; ph = ph -   5d0
 else if ( index(cmp,'P1')   .ne. 0 ) then
     am = am*1.050d0  ; ph = ph -   0d0
 else if ( index(cmp,'N2')   .ne. 0 ) then
     am = am*1.000d0  ; ph = ph -   5d0
 else if ( index(cmp,'M2')   .ne. 0 ) then
     am = am*1.150d0  ; ph = ph -   5d0
 else if ( index(cmp,'S2')   .ne. 0 ) then
     am = am*1.100d0  ; ph = ph -   0d0
 else if ( index(cmp,'L2')   .ne. 0 ) then
     am = am*1.000d0  ; ph = ph -  20d0
 else if ( index(cmp,'K2')   .ne. 0 ) then
     am = am*1.100d0  ; ph = ph - 0d0
 endif

 a =  ' '
 write(a,*) cmp, am, ph

 end subroutine correctiefile
