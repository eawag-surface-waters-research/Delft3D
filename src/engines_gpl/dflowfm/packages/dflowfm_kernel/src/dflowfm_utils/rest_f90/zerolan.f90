      SUBROUTINE ZEROLAN( KEY)
      use m_landboundary
      use m_polygon
      use m_missing
      use geometry_module, only: dbpinpol
      implicit none
      integer :: i
      integer :: inhul
      integer :: istart
      integer :: ja
      integer :: k
      integer :: key
      integer :: mxol
      integer :: ntot
      KEY = 3
      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYON, SO DELETE all BOUNDARY POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
!        CALL SAVESAM()
         DO 5 I = 1,MXLAN
            XLAN(I)  = dmiss
            YLAN(I)  = dmiss
            ZLAN(I)  = dmiss
            NCLAN(I) = 0
    5    CONTINUE
         MXLAN = 0
         RETURN
      ENDIF
!     CALL SAVESAM()
      INHUL = -1
      DO 10 I = 1,MXLAN
            CALL DBPINPOL( XLAN(I), YLAN(I), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
            IF (INHUL .EQ. 1) THEN
               XLAN(I)  = dmiss
               YLAN(I)  = dmiss
               ZLAN(I)  = dmiss
               NCLAN(I) = 0
            ENDIF
   10 CONTINUE

      K = 0
      MXOL   = MXLAN
      ISTART = 0
      NTOT   = 0
      DO 20 I = 1,MXLAN
         IF (XLAN(I) .NE. dmiss) THEN
            ISTART   = 1
            K        = K + 1
            XLAN(K)  = XLAN(I)
            YLAN(K)  = YLAN(I)
            ZLAN(K)  = ZLAN(I)
            NCLAN(K) = NCLAN(I)
         ELSE IF (ISTART .EQ. 1) THEN
            K       = K + 1
            XLAN(K)  = dmiss
            YLAN(K)  = dmiss
            ZLAN(K)  = dmiss
            NCLAN(K) = 0
            ISTART   = 0
         ENDIF
   20 CONTINUE
      MXLAN = K

      DO 30 I = MXLAN+1,MXOL
         XLAN(I)  = dmiss
         YLAN(I)  = dmiss
         ZLAN(I)  = dmiss
         NCLAN(I) = 0
   30 CONTINUE

      RETURN
      END
