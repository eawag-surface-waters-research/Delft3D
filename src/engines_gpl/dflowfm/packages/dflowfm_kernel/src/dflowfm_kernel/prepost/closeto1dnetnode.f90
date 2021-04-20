   SUBROUTINE CLOSETO1Dnetnode(XP1,YP1,N1,dist) !

      use m_netw
      use geometry_module, only: dbdistance
      use m_sferic
      use m_missing

      implicit none
      double precision, intent(in)  :: XP1, YP1
      double precision, intent(out) :: dist     ! find 1D point close to x,y:
      integer         , intent(out) :: n1       ! 1D point found


      double precision :: dismin
      integer          :: ja, k, k1, k2, L
      double precision :: dis,dis1,dis2

      N1 = 0
      DISMIN = 9E+33
      DO L = 1,numl
         IF (kn(3,L) == 1 .or. kn(3,L) == 6) then !  .or. kn(3,L) == 4) THEN
             K1 = kn(1,L) ; K2 = kn(2,L)
             dis1 = dbdistance(XP1,YP1,Xk(K1),Yk(K1),jsferic, jasfer3D, dmiss)
             dis2 = dbdistance(XP1,YP1,Xk(K2),Yk(K2),jsferic, jasfer3D, dmiss)
             if (dis1 < dis2) THEN
                k = k1 ; dis = dis1
             else
                k = k2 ; dis = dis2
             endif
             IF (DIS .LT. DISMIN) THEN
                N1 = k
                DISMIN = DIS
             ENDIF
          ENDIF
      ENDDO
      dist = dismin
      END SUBROUTINE CLOSETO1Dnetnode
