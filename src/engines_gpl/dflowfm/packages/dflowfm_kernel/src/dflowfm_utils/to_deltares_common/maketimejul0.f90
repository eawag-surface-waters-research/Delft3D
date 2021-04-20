 SUBROUTINE MAKETIMEjul0(TEX,TNr) ! maketime with jul0 already in module
 use m_flowtimes
 implicit none
 double precision :: tnr          ! time in hours
 CHARACTER TEX*(*)

 double precision :: Tuur, Tmin
 integer          :: nuur, nmin, nsec, iyyy,mm,id,ndag, jul0

 TEX  = '20010101 000000'
 JUL0 = julrefdat
 NDAG = TNR / 24.0
 CALL CALDAT(JUL0+NDAG,MM,ID,IYYY)

 TUUR = TNR - NDAG*24
 NUUR = TUUR
 TMIN = (TUUR - NUUR)*60
 NMIN = TMIN
 NSEC = (TMIN - NMIN)*60

 WRITE(TEX(1:4),'(I4.4)') IYYY
 WRITE(TEX(5:6),'(I2.2)') MM
 WRITE(TEX(7:8),'(I2.2)') ID

 WRITE(TEX(10:11),'(I2.2)') NUUR
 WRITE(TEX(12:13),'(I2.2)') NMIN
 WRITE(TEX(14:15),'(I2.2)') NSEC
 END  SUBROUTINE MAKETIMEjul0
