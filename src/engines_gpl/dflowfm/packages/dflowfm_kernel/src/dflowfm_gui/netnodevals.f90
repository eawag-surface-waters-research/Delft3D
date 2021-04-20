  SUBROUTINE NETNODEVALS(MET)
  USE M_FLOW
  USE M_FLOWGEOM
  use m_netw
  use m_sediment
  USE M_MISSING
  use m_ship

  implicit none
  integer :: MET

  integer :: k, L, j, K1, K2, K3,K4
  double precision :: x, y, z, uar
  double precision :: xn, yn, dis, rL  ! for smallest distance to land boundary (method=7)


  IF (MET .EQ. 1) RETURN

  IF (MET == 9) THEN
     RNOD = 0D0
     ! u1  =  yu*csu
     DO L = 1,LNXi
        K3  = LNCN(1,L)
        K4 = LNCN(2,L)
       ! UAR = U1(L)
        K1  = LN(1,L)
        K2 = LN(2,L)
        UAR = CSU(L)*( ACL(L)*UCX(K1) + (1D0-ACL(L))*UCX(K2) )    +    &
              SNU(L)*( ACL(L)*UCY(K1) + (1D0-ACL(L))*UCY(K2) )
        UAR = UAR*DX(L)
        RNOD(K3) = RNOD(K3) - UAR
        RNOD(K4) = RNOD(K4) + UAR
     ENDDO
     DO L = LNXi+1,lnx
        K3  = LNCN(1,L)
        K4 = LNCN(2,L)
        UAR = DX(L)*(1d0-acl(L))*U1(L)
        RNOD(K3) = RNOD(K3) - UAR
        RNOD(K4) = RNOD(K4) + UAR
     ENDDO

     do k = 1, mxwalls
        k3  = walls(2,k)
        k4  = walls(3,k)
        if (irov == 0) then
           RNOD(K3) = 0d0
           RNOD(K4) = 0d0
        else
           if (irov == 1) then
              uar = walls(16,k)
           else if (irov == 2) then
              uar = 0.d0 ! walls(16,k) ! *(1d0/walls(6,k) - 1d0/vonkar)
           endif
           UAR = 0.5d0*UAR*WALLS(9,K)
           RNOD(K3) = RNOD(K3) + UAR
           RNOD(K4) = RNOD(K4) + UAR
        endif
     enddo

     DO K = 1,NUMK
        IF (BAN(K) > 0D0) THEN
           RNOD(K) = RNOD(K) / BAN(K)
        ENDIF
     ENDDO

  ELSE
     DO K  = 1,NUMK
       X = XK(K)
       Y = YK(K)
       Z = ZK(K)

       IF (MET .EQ. 2) THEN
          RNOD(K) = K
       ELSE IF (MET .EQ. 3) THEN
          RNOD(K) = NMK(K)
       ELSE IF (MET .EQ. 5) THEN
          if (allocated(NB)) then
              if (size(NB) /= NUMK) then
                  exit
              else
                  RNOD(K) = NB(K)
              end if
          else
              RNOD(K) = 0
          end if
       ELSE IF (MET .EQ. 6) THEN
          RNOD(K) = ZK(K)
       ELSE IF (MET .EQ. 7) THEN
          call toland(x,y,1,MXLAN,1,xn,yn,dis,j,rL)
          rnod(k) = dis
       ELSE IF (MET .EQ. 8 .and. jased > 0 .and. jaceneqtr > 1) THEN
          RNOD(K) = grainlay(jgrtek,k)  ! erodable layer
       ELSE IF (MET .EQ. 10) THEN
          RNOD(k) = BAN(K)
       ELSE IF (MET .EQ. 11) THEN
          RNOD(k) = zspc(k)
       ENDIF
    ENDDO
  ENDIF
  RETURN
  END SUBROUTINE NETNODEVALS
