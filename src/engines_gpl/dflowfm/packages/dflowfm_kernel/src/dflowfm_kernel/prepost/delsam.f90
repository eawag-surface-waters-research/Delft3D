!>    delete samples
!>      jaconfirm=0: do not prompt for confirmation,       keep arrays,        make copy
!>                1:        prompt for confirmation,       keep arrays,        make copy
!>               -1: do not prompt for confirmation, deallocate arrays, do not make copy
      SUBROUTINE DELSAM(JACONFIRM)      ! SPvdP: need promptless delsam in orthogonalisenet
      USE M_SAMPLES
      use m_polygon
      USE m_missing
      use geometry_module, only: dbpinpol


      implicit none

      integer, intent(in) :: JACONFIRM  !< prompt for confirmation (1) or not (0)

      integer :: i
      integer :: inhul
      integer :: ja
      integer :: k
      integer :: key
      integer :: nsol
      double precision :: rd
      double precision :: xi
      double precision :: yi

      if (jaconfirm == -1) then
         if (nsmax > 0) then
            nsmax = 0 ; ns = 0
            if ( allocated(xs)    ) deallocate (xs, ys, zs)
            if ( allocated(ipsam) ) deallocate(ipsam)
         endif
         return
      endif

      IF (Npl .LE. 2) THEN
         if ( JACONFIRM.eq.1 ) then
            CALL CONFRM('NO POLYON, SO DELETE all SAMPLE POINTS ? ',JA)
         else
            JA = 1
         end if
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
         CALL SAVESAM()
         DO 5 I = 1,NS
            XS(I) = DMISS
            YS(I) = DMISS
            ZS(I) = DMISS
            ipsam(i) = 0
    5    CONTINUE
         NS = 0
         RETURN
      ENDIF
      ! Else: check in polygon
      CALL SAVESAM()
      INHUL = -1
      DO 10 I = 1,NS
            RD = ZS(I)
            XI = XS(I)
            YI = YS(I)
            CALL DBPINPOL(xI, yI, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
            IF (INHUL .EQ. 1) ZS(I) = dmiss
   10 CONTINUE

      K = 0
      NSOL = NS
      DO 20 I = 1,NS
         IF (ZS(I) .NE. dmiss) THEN
            K     = K + 1
            XS(K) = XS(I)
            YS(K) = YS(I)
            ZS(K) = ZS(I)
            ipsam(k) = ipsam(i)
         ENDIF
   20 CONTINUE
      NS = K

      DO 30 I = NS+1,NSOL
         XS(I) = DMISS
         YS(I) = DMISS
         ZS(I) = DMISS
         ipsam(i) = 0
   30 CONTINUE

      RETURN
      END SUBROUTINE DELSAM
