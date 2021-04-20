      SUBROUTINE GETSPL2(X,XI2,XJ2,MC,NC,MMAX,NMAX)
      USE M_MISSING
      implicit none
      integer :: i
      integer :: iff
      integer :: il
      integer :: in
      integer :: jalin
      integer :: jn
      integer :: k
      integer :: mc
      integer :: mmax
      integer :: mnmax
      integer :: nc
      integer :: nmax
!     VUL DE ARRAY MET TWEEDE AFGELEIDES IN I EN J RICHTING
!     HAAL TELKENS EEN LIJNTJE, DOE SPLINE EN ZET TERUG

      DOUBLE PRECISION              :: X(MMAX,NMAX), XI2(MMAX,NMAX), XJ2(MMAX,NMAX)
      DOUBLE PRECISION, ALLOCATABLE :: XH1(:), XH21(:), XHH(:)

      MNMAX = MAX(MMAX,NMAX)

      ALLOCATE ( XH1(MNMAX), XH21(MNMAX), XHH(MNMAX) )


      XI2 = DXYMIS
      XJ2 = DXYMIS

      DO 20 JN = 1,NC
         CALL GETIJ(X,XH1,MMAX,NMAX,MNMAX,1,MC,JN,JN)
         JALIN = 0
         K     = 0
         DO 30 I = 1,MC
            IF (XH1(I) .NE. DXYMIS) THEN
               IF (JALIN .EQ. 0) THEN
!                 BEGIN LIJN BIJ I
                  IFF    = I
                  JALIN  = 1
               ENDIF
               K      = K + 1
               XHH(K) = XH1(I)
               IF (JALIN .EQ. 1 .AND. I .EQ. MC) THEN
!                 EINDE LIJN BIJ MC DUS SPLINE VRAGEN
                  CALL SPLINE(XHH,K,XH21)
                  CALL PUTIJ(XJ2,XH21,MMAX,NMAX,MNMAX,IFF,MC,JN,JN)
               ENDIF
            ELSE IF (JALIN .EQ. 1) THEN
!              EINDE LIJN BIJ I - 1 DOORDAT I NUL IS, SPLINE VRAGEN
               JALIN = 0
               IL    = I - 1
               CALL SPLINE(XHH,K,XH21)
               CALL PUTIJ(XJ2,XH21,MMAX,NMAX,MNMAX,IFF,IL,JN,JN)
               K     = 0
            ENDIF
    30   CONTINUE
    20 CONTINUE

      DO 40 IN = 1,MC
         CALL GETIJ(X,XH1,MMAX,NMAX,MNMAX,IN,IN,1,NC)
         JALIN = 0
         K     = 0
         DO 50 I = 1,NC
            IF (XH1(I) .NE. DXYMIS) THEN
               IF (JALIN .EQ. 0) THEN
!                 BEGIN LIJN BIJ I
                  IFF    = I
                  JALIN  = 1
               ENDIF
               K      = K + 1
               XHH(K) = XH1(I)
               IF (JALIN .EQ. 1 .AND. I .EQ. NC) THEN
!                 EINDE LIJN BIJ MC DUS SPLINE VRAGEN
                  CALL SPLINE(XHH,K,XH21)
                  CALL PUTIJ(XI2,XH21,MMAX,NMAX,MNMAX,IN,IN,IFF,NC)
               ENDIF
            ELSE IF (JALIN .EQ. 1) THEN
!              EINDE LIJN BIJ I - 1 DOORDAT I NUL IS, SPLINE VRAGEN
               JALIN = 0
               IL    = I - 1
               CALL SPLINE(XHH,K,XH21)
               CALL PUTIJ(XI2,XH21,MMAX,NMAX,MNMAX,IN,IN,IFF,IL)
               K     = 0
            ENDIF
    50   CONTINUE
    40 CONTINUE

      DEALLOCATE ( XH1, XH21, XHH )

      RETURN
      END SUBROUTINE GETSPL2
