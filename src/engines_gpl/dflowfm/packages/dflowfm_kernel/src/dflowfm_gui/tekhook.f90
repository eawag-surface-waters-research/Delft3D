      SUBROUTINE TEKHOOK(XP,YP)
      use m_sferic
      implicit none
      double precision :: dx
      double precision :: dy
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: xp
      double precision :: ya
      double precision :: ylc
      double precision :: yp
      COMMON /LOCATORA/ XLC,YLC,XA,YA,JMOUSE,JASHOW


      DX = XA - XP
      DY = YA - YP
      CALL MOVABS(XA,YA)
      CALL  LNABS(XP-DX,YP-DY)
      CALL MOVABS(XP+DY,YP-DX)
      CALL  LNABS(XP-DY,YP+DX)
      RETURN
      END
