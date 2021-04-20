      subroutine triangulate_quadsandmore(ja) ! ja==1, findcells moet opnieuw

      use m_netw
      use m_flowgeom
      use m_polygon
      use m_missing, only: dmiss, JINS
      use geometry_module, only: dbpinpol, dbdistance
      use m_sferic, only: jsferic, jasfer3D
      use gridoperations

      implicit none

      integer ja
      integer in, k, k1, k2, k3, k4, k5, lnu

      call findcells(0)

      in   = -1
      do k = 1,nump
         if (netcell(k)%n >= 4) then
            call dbpinpol(xz(k), yz(k), in, dmiss, JINS, NPL, xpl, ypl, zpl)
            if (in == 1) then
               k1 = netcell(k)%nod(1); k2 = netcell(k)%nod(2); k3 = netcell(k)%nod(3); k4 = netcell(k)%nod(4)
               if (netcell(k)%n == 4) then
                  if (dbdistance( xk(k1), yk(k1), xk(k3), yk(k3), jsferic, jasfer3D, dmiss) < dbdistance( xk(k2), yk(k2), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)  ) then
                     call connectdbn(k1, k3, lnu) ; ja = 1
                  else
                     call connectdbn(k2, k4, lnu) ; ja = 1
                  endif
               else if (netcell(k)%n == 5) then
                  call connectdbn(k1, k3, lnu)    ; ja = 1
                  call connectdbn(k1, k4, lnu)
               else if (netcell(k)%n == 6) then
                  k5 = netcell(k)%nod(5)          ; ja = 1
                  call connectdbn(k1, k3, lnu)
                  call connectdbn(k1, k4, lnu)
                  call connectdbn(k1, k5, lnu)
               endif
            endif
         endif
      enddo

      end subroutine triangulate_quadsandmore
