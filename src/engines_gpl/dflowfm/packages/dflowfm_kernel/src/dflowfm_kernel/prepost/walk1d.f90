  RECURSIVE SUBROUTINE WALK1D(K1,IBR,NRL,JASTOP,KN316)

  use m_netw
  use gridoperations

  IMPLICIT NONE
  INTEGER :: K1,K2,K,IBR,NRL,JASTOP,LX,KN316

  INTEGER :: KK,L, KA

  JASTOP = 0
  DO KK = 1,NMK(K1)
     L  = NOD(K1)%LIN(KK)
     IF (LC(L) == 0 .AND. KN(3,L) == KN316) THEN

        CALL OTHERNODE (K1,L,K2)
        CALL GAANWESTOPPEN(K2,KN316,JASTOP,L)

        LC(L) = IBR ; NRL = NRL + 1
        LIB(NRL) = L ; K1BR(NRL) = K1 ; IBN(NRL) = IBR; NRLB(L) = NRL

        IF (JASTOP == 1) THEN
           RETURN
        ENDIF

        KA = K2
        CALL WALK1D(KA,IBR,NRL,JASTOP,KN316)

        IF (JASTOP == 1) THEN
           RETURN
        ENDIF
     ENDIF
  ENDDO
  END SUBROUTINE WALK1D
