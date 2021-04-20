      SUBROUTINE CLOSETO1Dnetlink(XP1,YP1,N1,XN1,YN1,DIST,kn3channelonly) !

      use m_netw
      use geometry_module, only: dbdistance, dlinedis
      use m_missing, only: dmiss, imiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer          :: n1
      double precision :: XP1, YP1, XN1,YN1
      double precision, intent(  out) :: DIST           !< distance to 1D link
      integer,          intent(in   ) :: kn3channelonly !< Whether or not (1/0) the kn3 code can only be a
                                                        !< regular channel netlink (1 or 6), OR also allows
                                                        !< 1D2D links (5 or 7).

      double precision :: dismin
      integer          :: ja, k1, k2, L
      double precision :: xa,ya,xb,yb,dis,xn,yn

      N1 = 0
      DISMIN = 9E+33
      DO L = 1,numl
         IF (kn(3,L) == 1 .or. kn(3,L) == 6  .or. (kn3channelonly == 0 .and. (kn(3,L) == 5 .or. kn(3,L) == 7))) then !  .or. kn(3,L) == 4) THEN
             K1 = kn(1,L) ; K2 = kn(2,L)
             XA = Xk(K1)
             YA = Yk(K1)
             XB = Xk(K2)
             YB = Yk(K2)
             CALL dLINEDIS(XP1,YP1,XA,YA,XB,YB,JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)
             !IF (JA .EQ. 1 .AND. DIS < 0.5D0*DBDISTANCE(XA,YA,XB,YB,jsferic, jasfer3D, dmiss)) THEN
             IF (JA .EQ. 1) THEN
                IF (DIS .LT. DISMIN) THEN
                   N1     = L
                   DISMIN = DIS
                   XN1    = XN ; YN1 = YN
                ENDIF
             ENDIF
          ENDIF
      ENDDO
   !   IF (N1 .NE. 0) THEN
   !       K1 = kn(1,n1) ; K2 = kn(2,n1)
   !       IF (dbdistance(XP1,YP1,Xk(K1),Yk(K1)) < dbdistance(XP1,YP1,Xk(K2),Yk(K2)) ) THEN
   !          N1 = K1
   !       ELSE
   !          N1 = K2
   !       ENDIF
   !   ENDIF

      DIST = DISMIN

   END SUBROUTINE CLOSETO1Dnetlink
