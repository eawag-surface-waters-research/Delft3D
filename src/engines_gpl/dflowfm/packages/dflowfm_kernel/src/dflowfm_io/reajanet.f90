      SUBROUTINE REAJANET(MNET,JA,JADOORLADEN)
      use m_netw
      use gridoperations

      implicit none
      integer :: MNET, JA, JADOORLADEN

      integer :: k
      integer :: k0
      integer :: l
      integer :: l0
      integer :: n1
      integer :: numkn
      integer :: numln
      double precision :: x10

      CHARACTER REC*3320

      IF (JADOORLADEN .EQ. 0) THEN
         K0 = 0
         L0 = 0
      ELSE
         K0 = NUMK
         L0 = NUML
      ENDIF

      JA = 0
      READ(MNET,'(A)',end = 777) REC   ! COMMENT

      READ(MNET,'(A)',end = 777) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, err = 555) NUMKN

      READ(MNET,'(A)',end = 777) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, err = 555) NUMP



      READ(MNET,'(A)',end = 777) REC


      READ(MNET,'(A)',end = 777) REC
      N1 = INDEX(REC,'=') + 1
      READ(REC(N1:),*, err = 555) NUMLN



      READ(MNET,'(A)',end = 777) REC

      READ(MNET,'(A)',end = 777) REC

      READ(MNET,'(A)',end = 777) REC

      DO K = 1,4
         READ(MNET,'(A)',end = 777) REC
      ENDDO

      CALL INCREASENETW(K0+NUMKN, L0 + NUMLN)

      DO K = K0+1, K0+NUMKN
         READ(MNET,'(A)',END = 777) REC
         READ(REC,*,ERR = 999) XK(K), YK(K)
      ENDDO
      !XK   = XK - 270000
      !YK   = YK - 2700000

      NUMK = K0+NUMKN
      KC   = 1

      DO K = 1,NUMP
         READ(MNET,*)
      ENDDO

      DO L = L0+1, L0+NUMLN
         READ(MNET,'(A)',END = 777) REC
         READ(REC,*,ERR = 888) x10, KN(1,L), KN(2,L)
         KN(1,L) = KN(1,L) + K0
         KN(2,L) = KN(2,L) + K0
         KN(3,L) = 2
      ENDDO
      NUML = L0+NUMLN

      CALL SETNODADM(0)

      ja = 1
      return

  999 CALL QNREADERROR('READING NETNODES, BUT GETTING ', REC, MNET)
      RETURN

  888 CALL QNREADERROR('READING NETLINKS, BUT GETTING ', REC, MNET)

  777 CALL QNEOFERROR(MNET)
      RETURN

  555 CALL QNREADERROR('READING NR OF NETNODES, BUT GETTING ', REC, MNET)
      RETURN

  444 CALL QNREADERROR('READING NR OF NETLINKS, BUT GETTING ', REC, MNET)
      RETURN

      END SUBROUTINE REAJANET
