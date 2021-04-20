      SUBROUTINE DISDIS()

      use m_devices
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none

      double precision :: dis
      integer :: jashow
      integer :: jmouse
      double precision :: xa
      double precision :: xlc
      double precision :: ya
      double precision :: ylc
!     -------------------------------
!     write distance
!     -------------------------------
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      common /dispfor/ xyform, zform, disform
      character*7      xyform, zform, disform
      CHARACTER DISTAN*25

      DISTAN = 'DIS:'
      DIS    = dbdistance(xa,ya,xlc,ylc, jsferic, jasfer3D, dmiss)
      WRITE(DISTAN(6:),'(F17.5)') min(DIS,1d9)
      CALL KTEXT(DISTAN,IWS-24,3,15)

  !   checkdislin()

      RETURN
      END
