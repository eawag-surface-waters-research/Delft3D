 subroutine hurdlestive(U10,fetchL,fetchD,Hsig,Tsig)
 use m_physcoef
 IMPLICIT NONE
 double precision :: U10,fetchL,fetchD,Hsig,Tsig
 double precision :: rt, ua,fs,ds,aa1,aa2,aa3,aa4
 double precision, external :: tanhsafe

 rt   = 1.1d0                           ! temperature and density  dependent
 ua   = 0.71d0*(rt*U10)**1.23           ! reference wind speed
 fs   = ag * fetchl / ua**2             ! dimensieloze strijklengte
 ds   = ag * fetchd / ua**2             ! dimensieloze diepte

 aa1  = 0.60d0 *ds**0.750d0             ! formulae from : Coastal stabilisation, R. Silvester, J.R.C. Shu, 2.35 en 2.36
                                        ! taken from Hurdle, Stive 1989 , RESULTS SEEM VERY SIMILAR TO THOSE OF DELWAQ CODE ABOVE
 aa3  = 4.3d-5*fs/tanhsafe(aa1)**2
 hsig = 0.25d0*tanhsafe(aa1)*tanhsafe(aa3)**0.5000000d0
 hsig = hsig*ua*ua/ag

 aa2  = 0.76d0 *ds**0.375d0
 aa4  = 4.1d-5*fs/tanhsafe(aa2)**3
 tsig = 8.30d0*tanhsafe(aa2)*tanhsafe(aa4)**0.3333333d0
 tsig = tsig*ua/ag
 end subroutine hurdlestive
