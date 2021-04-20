  SUBROUTINE MINMXNETNODS()
  use m_netw
  use m_missing
  use unstruc_display

  implicit none

  integer          :: i, k
  double precision :: rd, rmax, rmin

  double precision :: VMAX, VMIN, DV, VAL
  integer          :: NCOLS,NV,NIS,NIE,JAAUTO
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
  logical inview


  ! BEPAAL MINIMUM EN MAXIMUM VAN DIEPTES BINNEN VIEWING AREA

  IF (JAAUTO > 0) THEN
     RMIN =  1.0D30
     NODMIN = 0
     RMAX = -1.0D30
     NODMAX = 0
     DO K = 1,NUMK
        IF ( INVIEW(XK(K),YK(K) ) ) THEN
           RD = RNOD(K)
           IF (rd .ne. dmiss) then
              IF (RD < RMIN ) THEN
                 RMIN = RD
                 NODMIN = K
              ENDIF
              IF (RD > RMAX) THEN
                 RMAX = RD
                 NODMAX = K
              ENDIF
           ENDIF
        ENDIF
     ENDDO
     VMAX = RMAX
     VMIN = RMIN
  ENDIF

  DV   = VMAX - VMIN
  DO I = 1,NV
     VAL(I) = VMIN + (I-1)*DV/(NV-1)
  ENDDO

  RETURN
  END SUBROUTINE MINMXNETNODS
