      SUBROUTINE TXTTIM()
      use m_devices
      implicit none
      integer :: l
      double precision :: txtimsize
      double precision :: txtimx
      double precision :: txtimy
      COMMON /TEXTIM/  TXTIMSIZE, TXTIMX, TXTIMY, TXTIM

      CHARACTER TXTIM*60
      L = len_trim(TXTIM)
      IF (L .EQ. 0) RETURN
      CALL IGRCHARSIZE(real(TXTIMSIZE),real(TXTIMSIZE))
      CALL IGRCHARFONT(3)
      CALL MTEXT(TXTIM,TXTIMX,TXTIMY,35)
      CALL IGRCHARFONT(1)
      CALL SETTEXTSIZE()

      RETURN
      END
