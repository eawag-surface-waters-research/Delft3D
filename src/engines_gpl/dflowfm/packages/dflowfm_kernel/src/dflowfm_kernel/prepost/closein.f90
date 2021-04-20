   SUBROUTINE CLOSEIN(XA,YA,INNUMP,KIN,NKIN,KK)  ! KK IS HET MEEST DICHTBIJ GELEGEN POINT VAN INNUMP
   use m_netw
   implicit none
   double precision :: xa
   double precision :: ya
   integer :: innump
   INTEGER :: KIN(NKIN)
   integer :: nkin
   integer :: kk

   double precision :: dx
   double precision :: dy
   integer :: k
   integer :: k1
   integer :: nn
   double precision :: ra
   double precision :: ramin
   RAMIN = 1E30

   KK = 0
   DO NN = 1, netcell(INNUMP)%N
      K1 = netcell(INNUMP)%NOD(NN)
      DO K = 1,NKIN
         IF (KIN(K) == K1) THEN
            DX = XK(K1) - XA ; DY = YK(K1) - YA
            RA = SQRT(DX*DX + DY*DY)
            IF ( RA < RAMIN ) THEN
               RAMIN = RA
               KK    = K1
            ENDIF
         ENDIF
      ENDDO
   ENDDO

   RETURN
   END SUBROUTINE CLOSEIN
