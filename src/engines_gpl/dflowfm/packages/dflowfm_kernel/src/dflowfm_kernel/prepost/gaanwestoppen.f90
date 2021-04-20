  SUBROUTINE GAANWESTOPPEN(K,KN316,JASTOP,LO)  !SET JASTOP = 1 ALS WE GAAN STOPPEN
  USE M_NETW
  IMPLICIT NONE
  INTEGER :: K2,KN316,JASTOP,N1,N6,KK,L,K1,K,LO

  JASTOP = 0 ; N1 = 0 ; N6 = 0

  IF (NMK0(K) == 1) THEN
      JASTOP = 1 ; RETURN
  ENDIF

  DO KK = 1,NMK(K)
     L  = NOD(K)%LIN(KK)
     IF (KN(3,L) == 1) THEN
        N1 = N1 + 1
     ELSE IF (KN(3,L) == 6) THEN
        N6 = N6 + 1
     ENDIF
  ENDDO
  IF (KN316 == 1) THEN
     IF (N1 + N6 .NE. 2) THEN  ! altijd stoppen bij niet doorgaande node
        JASTOP = 1
     ENDIF
  ELSE IF (KN316 == 6) THEN    ! alleen stoppen bij aantal 6 jes ongelijk 2
     IF (N6 .NE. 2) THEN
        JASTOP = 1
     ENDIF
  ENDIF

  END SUBROUTINE GAANWESTOPPEN
