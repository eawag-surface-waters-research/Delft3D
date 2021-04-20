 subroutine getcz(h1, frcn, ifrctyp, cz, L)         ! basic get chezy coefficient,  this routine is not safe for frcn == 0
 use m_physcoef, only : sag, vonkar, ee
 use m_flow,     only : u1, v
 implicit none
 integer,          intent(in)  :: ifrctyp, L  !< friction type
 double precision              :: h0, h1      !< hydraulic radius
 double precision, intent(in)  :: frcn        !< friction coeff
 double precision, intent(out) :: cz          !< Computed Chezy coeff
 double precision :: hurou, sixth = 1d0/6d0, sqcf, z0, umod

 h0 = max(h1,1d-4)
 if (ifrctyp == 0) then                              ! Chezy type
     cz = frcn
 else if (ifrctyp == 1) then                         ! Manning type
     cz = ( h0**sixth ) / frcn
 else if (ifrctyp == 2) then                         ! White Colebrook Delft3
     z0   = min( frcn / 30d0 , h0*0.3d0)
     sqcf = vonkar/log( h0/(ee*z0) )
     cz   = sag/sqcf
 else if (ifrctyp == 3) then                         ! White Colebrook WAQUA
     hurou = max(0.5d0, h0/frcn)
     cz    = 18d0*log10(12d0*hurou)
 else if ( ifrctyp == 4 .or. ifrctyp == 5 .or. ifrctyp == 6 ) then   ! also manning, just testing implicitness in furu
     cz = ( h0**sixth ) / frcn
 else
     cz   = 60d0                                     ! dummy
     umod = sqrt(u1(L)*u1(L) + v(L)*v(L))
     call hydraulicallysmooth(umod,h0,sqcf)
     if (sqcf > 0d0) then
        Cz = sag/sqcf
     else
        Cz = 0d0
     endif
 endif
 end subroutine getcz
