      SUBROUTINE INTINI()
      use m_sferic
      use unstruc_version_module, only : unstruc_company, unstruc_program, unstruc_version
      use m_wearelt
      use m_devices
      implicit none
      double precision :: croshrsz
      integer :: icrhf
      integer :: infoopsystem
      integer :: jashow
      integer :: jaxis
      integer :: jmouse
      integer :: jvga
      integer :: ncolnow
      integer :: ntxcols
      integer :: ntxrows
      integer :: nxpix
      integer :: nypix
      double precision :: xa
      double precision :: xlc
      double precision :: xleft
      double precision :: ya
      double precision :: ybot
      double precision :: ylc

      COMMON /INITSCREEN/  CROSHRSZ,JVGA,NXPIX,NYPIX,NTXCOLS,NTXROWS
      COMMON /SCREENAREA/ XLEFT,YBOT,JAXIS
      COMMON /COLNOW/ NCOLNOW
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      JSFERIC = 0
     ! CALL ISCREENMODEOPTIONS(1,NTXCOLS)
     ! CALL ISCREENMODEOPTIONS(2,NTXROWS)
      CALL ISCREENMODEOPTIONS(6,1)
      CALL ISCREENMODEOPTIONS(9,1)
      NOPSYS = INFOOPSYSTEM(1)
      NCOLR  = 256

      IF (NOPSYS .EQ. 1 .AND. JVGA .EQ. 1) THEN
         NXPIX    = 640
         NYPIX    = 480
         NCOLR    =  16
!        CALL VGA@()
      ENDIF
      CALL ISCREENOPEN(' ','GR',NXPIX,NYPIX,NCOLR)

      CALL ISCREENTITLE('G',trim(unstruc_company)//'-'//trim(unstruc_program)//' '//trim(unstruc_version))

      !CALL ISCREENTITLE('G', PROGNM)

      CALL SETGRAFMOD()
      CALL SETCOLORTABLE()

      CALL INIKEYS()
!      CALL INSERTOVER('OVER')

!     set size crosshair cursor
      ICRHF = 1d0/CROSHRSZ

      CALL IGRINPUTOPTIONS(5,ICRHF)
!
      CALL InEventSelect(0,0)
      IF (NOPSYS .EQ. 1) THEN
!        Mouse button down, up, move is an event
         CALL InEventSelect(0,1+2+8)
      ELSE
!        Mouse button down, up, resize an event
         CALL InEventSelect(0,1+2+32)
         CALL InEventSelect(0,1+2+8+32)
!        Enable processing of expose/resize events
         CALL InControlKey(50,259)
      ENDIF
!
      CALL ICURSOR(' ')

!     exit on mouse click outside input area
      CALL INMOUSEOPTIONS(2,1)

!     only BUTTON DOWN
      CALL INMOUSEOPTIONS(3,0)

      CALL IFRAMEOPTIONS(6,15)
      CALL IFRAMEOPTIONS(7,0)

!     CALL IFRAMETYPE(9)
!     CALL IFORMDEFAULTS(3)

      CALL SETTEXTSIZE()
      CALL IGRFILLPATTERN(4,0,0)

      YBOT  = 0d0
      XLEFT = 0d0
      JAXIS = 0
      CALL viewport(0.0,0.0,1.0,1.0)
!      CALL IPGAREA(0.0,0.0,1.0,1.0)

      XMIN   = 0d0
      XMAX   = 1d0
      YMIN   = 0d0
      YMAX   = 1d0
      X1     = XMIN
      X2     = XMAX
      Y1     = YMIN
      Y2     = YMAX
      NCOLNOW = 31
      XLC = 0
      YLC = 0

      CALL MINMAXWORLD(XMIN,YMIN,XMAX,YMAX)

      RETURN
      END
