subroutine qsun_nominal(rlon, rlat, timhr, qs)
   use m_sferic
   use m_flowtimes, only : timjan, tzone
   implicit none

   double precision :: rlat, rlon, timhr, qs

   double precision :: decln, w0, w1, d, e, tm , snh


   ! Calculate sine of the angle of the sun above the horizon: SNH
   ! d is the declination angle
   ! June 21st is the 171st day after TM=0

   tm     = timjan + timhr
   !if (jsferic > 0) then
      tm     = tm + 24.0d0*rlon/360.0d0 - tzone
   !endif
   w0     = twopi / (365.24d0*24d0)
   w1     = twopi / (24d0)
   decln  = 23.5d0*dg2rd
   d      = decln * cos(w0*tm - 2.950d0)
   e      = rlat*dg2rd
   snh    = -cos(e) * cos(d) * cos(w1*tm) + sin(e) * sin(d)
   snh    = max(0d0,min(1d0,snh))
   qs     = 1368d0 * snh *  0.76d0
end subroutine qsun_nominal
