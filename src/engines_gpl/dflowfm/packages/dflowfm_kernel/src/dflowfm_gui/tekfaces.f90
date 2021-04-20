      SUBROUTINE TEKFACES()

      use unstruc_colors
      use m_netw
      use sorting_algorithms, only: indexx
      use gridoperations

      implicit none
      integer :: ierr
      integer :: k
      integer :: l
      integer :: n
      integer :: ncol
      integer :: ni

      DOUBLE PRECISION   XX,YY,ZZ, XH(10), YH(10), ZH(10)
      INTEGER, ALLOCATABLE, SAVE        :: NP(:)
      double precision :: XP, YP
      double precision, ALLOCATABLE, SAVE        :: ZP(:)

      IF (SIZE(NP) .LT. NUMP) THEN
         IF ( ALLOCATED(NP) ) DEALLOCATE(NP,ZP)
         ALLOCATE (NP(NUMP),ZP(NUMP),STAT = IERR)
      ENDIF

      IF (NUMP .NE. 0) THEN
         DO N  = 1, NUMP
            XX = 0
            YY = 0
            ZZ = 0
            DO K = 1,netcell(N)%N
               XX = XX + XK(netcell(N)%NOD(K))
               YY = YY + YK(netcell(N)%NOD(K))
               ZZ = ZZ + ZK(netcell(N)%NOD(K))
            ENDDO
            XX = XX/netcell(N)%N
            YY = YY/netcell(N)%N
            ZZ = ZZ/netcell(N)%N
            CALL DRIETWEE(XX,YY,ZZ,XP,YP,ZP(N))
         ENDDO
         call indexx(NUMP,ZP,NP)

         DO L  = NUMP, 1, -1
            N  = NP(L)
            NI = netcell(N)%N
            DO K = 1, NI
               XH(K) = XK(netcell(N)%NOD(K))
               YH(K) = YK(netcell(N)%NOD(K))
               ZH(K) = ZK(netcell(N)%NOD(K))
            ENDDO
            IF (NI .EQ. 6) THEN
               NCOL = 221
            ENDIF
            IF (NI .EQ. 5) THEN
               NCOL = 111
            ENDIF
            IF (NI .EQ. 4) THEN
               NCOL =  31
            ENDIF
            IF (NI .EQ. 3) THEN
               NCOL = 171
            ENDIF
            CALL PFILLER (XH,YH,NI,NCOL,NCOLLN)
         ENDDO
      ENDIF

      RETURN
      END SUBROUTINE TEKFACES
