subroutine getczz0(h1, frcn, ifrctyp, cz, z0)       ! basic get z0 (m),  this routine is not safe for frcn == 0
 use m_physcoef, only : sag, vonkar, ee, ee9, c9of1
 use m_flowparameters, only: epshu
 implicit none

 double precision, intent(in) :: h1
 integer, intent(in) :: ifrctyp
 double precision, intent(in) :: frcn
 double precision, intent(out) :: cz
 double precision, intent(out) :: z0

 double precision    :: h0, sqcf, hurou  ! hydraulic radius, friction coeff, friction typ, chezy coeff
 double precision    :: sixth = 1d0/6d0

 h0    = max(h1,epshu)

 if (ifrctyp == 0) then                              ! Chezy type
     cz   = frcn
     ! z0   = h0 / ( exp (vonkar*cz/sag + 1d0) - c9of1) !
     z0   = h0*exp(-1d0 - vonkar*cz/sag)
 else if (ifrctyp == 1) then                         ! Manning type
     cz   = ( h0**sixth ) / frcn
     !z0  = h0 / ( exp (vonkar*cz/sag + 1d0) - c9of1)
     z0   = h0*exp(-1d0 - vonkar*cz/sag)
 else if (ifrctyp == 2) then                         ! White Colebrook Delft3D
     z0   = min( frcn / 30d0 , h0*0.3d0)
     sqcf = vonkar/log( h0/(ee*z0) )                 ! true white colebrook d3d would be vonkar/log( 1d0+h0/(ee*z0) )
     cz   = sag/sqcf
 else if (ifrctyp == 3) then                         ! White Colebrook WAQUA
     hurou = max(0.5d0, h0/frcn)
     cz    = 18d0*log10(12d0*hurou)
     !z0   = h0 / ( exp (vonkar*cz/sag + 1d0) - c9of1)
     z0    = h0*exp(-1d0 - vonkar*cz/sag)
 else
     cz  = 60d0                                      ! dummies for hydraulically smooth
     !z0 = h0 / ( exp (vonkar*cz/sag + 1d0) - c9of1)
     z0  = h0*exp(-1d0 - vonkar*cz/sag)
 endif
 end subroutine getczz0
