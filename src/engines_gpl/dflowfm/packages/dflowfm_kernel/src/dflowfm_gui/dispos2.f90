      SUBROUTINE DISPOS2(X,Y)
      USE M_DEVICES
      implicit none
      double precision :: x
      double precision :: y
      common /dispfor/ xyform, zform, disform
      character*7      xyform, zform, disform
      CHARACTER POSITI*25

      POSITI = 'X,Y:         ,         '
      WRITE(POSITI (5:14),xyform) X
      WRITE(POSITI(16:25),xyform) Y

      CALL KTEXT(POSITI,IWS-24,2,15)
      CALL DISDIS()

      RETURN
      END
