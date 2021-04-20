subroutine getsoulsbywci(modind, z00, ustc2, ustw2, fw, cdrag, umod, abscos, taubpuLL, taubxuLL)
 use m_physcoef, only : rhomean
 implicit none
 integer         , intent(in)  :: modind
 double precision, intent(in)  :: z00, ustc2, ustw2, fw, cdrag, umod, abscos           ! Cdrag = ag/C2, abscos = wav relative to link dir
 double precision, intent(out) :: taubpuLL, taubxuLL
 double precision              :: ypar, ymxpar
 double precision              :: tauwav, taucur

 tauwav = ustw2*rhomean
 taucur = ustc2*rhomean

 call getymxpar(modind, tauwav, taucur, fw, cdrag, abscos, ypar, ymxpar)

 taubpuLL = ypar   * (taucur + tauwav) / ( umod*rhomean + 1d-4 ) ! umod*ag/C2, (m/s)
 taubxuLL = ymxpar * (taucur + tauwav)                           ! Max shear stress needed in Erosed, (N/m2)

end subroutine getsoulsbywci
