      SUBROUTINE READLOCATOR(X,Y,KEY)
      use m_wearelt
      use m_devices
      use m_partitioninfo
      implicit none
      double precision :: dpx
      double precision, save :: f = 1d0
      integer :: ini, jashow, jmouse, key, key_all, ixp, iyp
      integer, save :: keyold = 0
      real :: xloc, yloc
      double precision :: x, y
      double precision :: xa, ya, xlc, ylc
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      REAL, external :: INFOGRAPHICS

      DPX   = (X2-X1)/NPX
      CALL IMOUSECURSORSHAPE(1,'G')
      CALL IMouseCursorShow()
      INI = KEY

   10 CONTINUE

      IF (NOPSYS .EQ. 1) THEN
!        CALL InKeyEventIMM(KEY)
         CALL InKeyEvent(KEY)
      ELSE
         if ( jampi.eq.0 ) then
            CALL InKeyEvent(KEY)
         else
            CALL InKeyEventIMM(KEY)
!           reduce key
!            call reduce_key(key)
         end if
      ENDIF

      IF (KEY .EQ. -999) THEN
!        er gebeurt helemaal niets
         GOTO 10
!      ELSE IF (KEY .GE. 128 .AND. KEY .LE. 131) THEN
!        pijltjesbeweging
         IF (KEYOLD .NE. KEY) THEN
            F   = 1
         ENDIF
         KEYOLD = KEY
         F      = F*1.08d0
         F      = MIN(F,10d0)
         IF (KEY .EQ. 128) THEN
            YLC = YLC + DPX*F
         ELSE IF (KEY .EQ. 129) THEN
            YLC = YLC - DPX*F
         ELSE IF (KEY .EQ. 130) THEN
            XLC = XLC + DPX*F
         ELSE IF (KEY .EQ. 131) THEN
            XLC = XLC - DPX*F
         ENDIF
         CALL IMOUSECURSORXYG(real(XLC),real(YLC))
         X = XLC
         Y = YLC
         IF (INI .EQ. 999) THEN
            KEY = -10
            CALL IMOUSECURSORHIDE()
            CALL GIVEKEY(KEY)
            RETURN
         ENDIF
      ENDIF

!     muisbeweging
      Xloc = InfoGraphics(5)
      Yloc = InfoGraphics(6)
      X    = dble(xloc)
      y    = dble(yloc)

      CALL IGRUNITSTOPIXELS(Xloc,Yloc,IXP,IYP)
      CALL dPROJECT(X,Y,XLC,YLC,2)
      X = XLC
      Y = YLC

!     buiten veld?
      IF (INI .NE. 999) THEN
         IF (IYP .GT. NPY-15) THEN
            KEY = 1
            CALL IMOUSECURSORSHAPE(0,'G')
            RETURN
         ELSE IF (IYP .LT. 15) THEN
            KEY = 2
            CALL IMOUSECURSORSHAPE(0,'G')
            RETURN
         ENDIF
      ENDIF



!     muisbeweging
!      Xloc   = InfoGraphics(5)
!      Yloc   = InfoGraphics(6)
!      X=dble(xloc)
!      y=dble(yloc)
!      y = min(max(y, y1), y2)

!      CALL dPROJECT(X,Y,XLC,YLC,2)
!      X = XLC
!      Y = YLC

!     buiten veld?
!      IF (INI .NE. 999) THEN
!         IF (Y .GT. Y1 + 0.98d0*(Y2-Y1) ) THEN
!            KEY = 1
!            CALL IMOUSECURSORSHAPE(0,'G')
!            RETURN
!         ELSE IF (Y .LT. Y1 + 0.02d0*(Y2-Y1) ) THEN
!            KEY = 2
!            CALL IMOUSECURSORSHAPE(0,'G')
!            RETURN
!         ENDIF
!      ENDIF

!      XLC = X
!      YLC = Y

      IF (INI .EQ. 999) THEN
         IF (KEY .GE. 254 .AND. KEY .LE. 257) THEN
!           zo snel mogelijk lopen, geen keys of display
            KEY = -10
            RETURN
         ELSE
            CALL DISPOS()
            CALL DISDIS()
            CALL GETKEY2(KEY)
            CALL GIVEKEY(KEY)
            CALL IMOUSECURSORHIDE()
            RETURN
         ENDIF
      ELSE
         CALL DISPOS()
         CALL DISDIS()
         IF ( (KEY .GE. 254 .AND. KEY .LE. 257) .OR.      &
              (KEY .GE. 128 .AND. KEY .LE. 131) ) THEN
!        IF (KEY .EQ. 257 .OR. KEY .GE. 128 .AND. KEY .LE. 131) THEN
!           zo snel mogelijk lopen
            GOTO 10
         ELSE
            CALL GETKEY2(KEY)
            CALL GIVEKEY(KEY)
            CALL TIMLIN()
            CALL IMOUSECURSORHIDE()
         ENDIF
      ENDIF
      RETURN
      END
