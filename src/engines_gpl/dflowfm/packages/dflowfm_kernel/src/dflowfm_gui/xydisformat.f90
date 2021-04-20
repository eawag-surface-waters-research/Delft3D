      SUBROUTINE XYDISFORMAT ()
      use m_sferic
      use m_wearelt
      implicit none

      double precision :: dv
      integer :: ix
      integer :: ixmax
      integer :: ixmin
      integer :: ixy
      integer :: iy
      integer :: iymax
      integer :: iymin
      integer :: izmax
      integer :: izmin
      integer :: jaauto, JMOUSE,JASHOW
      integer :: ncols
      integer :: ndec
      integer :: nie
      integer :: nis
      integer :: nv
      integer :: nxy
      integer :: nz
      double precision :: val
      double precision :: vmax, XLC,YLC,XA,YA
      double precision :: vmin
      double precision :: dlen


      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

      COMMON /DISPFOR/ XYFORM, ZFORM, DISFORM
      CHARACTER*7      XYFORM, ZFORM, DISFORM

      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW


      ZFORM  = '(F7.1)'

      xlc   = max(x1, min(x2, xlc) )
      ylc   = max(y1, min(y2, ylc) )

      IXMIN = INT(LOG10(MAX(1d-6,ABS(X1))))
      IXMAX = INT(LOG10(MAX(1d-6,ABS(X2))))
      IYMIN = INT(LOG10(MAX(1d-6,ABS(Y1))))
      IYMAX = INT(LOG10(MAX(1d-6,ABS(Y2))))
      IZMIN = INT(LOG10(MAX(1d0,ABS(VMIN))))
      IZMAX = INT(LOG10(MAX(1d0,ABS(VMAX))))

      IX  = MAX (IXMIN, IXMAX)
      IY  = MAX (IYMIN, IYMAX)
      IXY = MAX (IX,    IY   )

!     -------------------
!     1 VOOR +-
!     1 VOOR .
!     1 VOOR LOG(100) = 2
!     -------------------

      NXY  = IXY + 4
      NDEC = 10  - NXY
      IF (NDEC .GE. 0) THEN
         XYFORM = '(F10.1)'
         WRITE ( XYFORM(6:6),'(I1)') NDEC
      ELSE
         XYFORM = '(E10.3)'
      ENDIF


      DISFORM='F17.5'


      NZ  = IZMAX + 3
      WRITE (  ZFORM(5:5),'(I1)') max(0, 9 - NZ)

      RETURN
      END
