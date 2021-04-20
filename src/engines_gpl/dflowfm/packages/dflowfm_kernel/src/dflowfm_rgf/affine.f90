      SUBROUTINE AFFINE(XX,YY,XG,YG,INI)
      USE M_BITMAP
      use string_module, only: find_first_letter
      implicit none
      integer :: ini
      logical :: jawel
      integer :: k
      integer :: minp
      integer :: numbersonline
      double precision :: xg4
      double precision :: xx4
      double precision :: yg4
      double precision :: yy4
      CHARACTER REC*132
	  DOUBLE PRECISION :: XX,YY,XG,YG
      XX4 = XX ; YY4 = YY

      IF (INI .EQ. 1) THEN

         INQUIRE(FILE = 'AFFINE'//'.xyx', EXIST = JAWEL)

         IF (JAWEL) THEN
            CALL OLDFIL(MINP, 'AFFINE'//'.xyx')
            READ(MINP,'(A)') REC
            IF (find_first_letter(REC) .EQ. 1) THEN
               READ(MINP,'(A)') REC
               DO K = 1,4
                  READ(MINP,'(A)') REC
                  IF (NUMBERSONLINE(REC) .EQ. 2) THEN
                     READ(REC,*) XP(K),YP(K)
                  ELSE IF (NUMBERSONLINE(REC) .EQ. 4) THEN
                     READ(REC,*) XP(K),YP(K),XB(K),YB(K)

                  ENDIF
               ENDDO
            ELSE
               CALL QNERROR('Cannot Read AFFINE.XYX File',' ',' ')
            ENDIF
            CALL DOCLOSE(MINP)
            CALL BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
            INI = 0
         ELSE
            CALL QNERROR ('NO AFFINE.XYX FILE FOUND', ' ',' ')
         ENDIF
      ENDIF

      CALL BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
      XG = XG4
	  YG = YG4

      RETURN
	  END
