!>    gives link length
      double precision function dLinklength(L)

      use m_netw
      use m_missing, only: dmiss, JINS
      use m_polygon, only: NPL, xpl, ypl, zpl
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dbpinpol, dbdistance
      use gridoperations

      implicit none

      integer,          intent(in)  :: L  !< link number
      double precision              :: dx, dy
      integer                       :: La, k1, k2

      La = iabs(L)
      k1 = kn(1,La)
      k2 = kn(2,La)

      dLinklength = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)

      end function dLinklength
