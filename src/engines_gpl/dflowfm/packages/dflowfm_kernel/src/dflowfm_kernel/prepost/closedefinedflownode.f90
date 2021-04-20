      SUBROUTINE CLOSEdefinedflownode(XP1,YP1,N1)  !

      use m_flowgeom
      use m_flow
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      integer          :: n1
      double precision :: XP1, YP1
      double precision :: dismin, dis
      integer          :: n

      N1 = 0
      DISMIN = 9d33
      DO n = 1,ndxi
         if (laydefnr(n) > 0) then
            dis =  dbdistance(XP1,YP1,XZ(n),YZ(n), jsferic, jasfer3D, dmiss)
            IF (dis < dismin) then
               n1 = n ; dismin = dis
            endif
         endif
      enddo
      end subroutine CLOSEdefinedflownode
