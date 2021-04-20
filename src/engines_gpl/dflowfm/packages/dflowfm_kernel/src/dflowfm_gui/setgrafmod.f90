      SUBROUTINE SETGRAFMOD()
      use m_devices
      implicit none
      integer :: infoscreen
      integer :: infoscreenmode
      integer :: mode

      MODE   = INFOSCREEN(1)
      IWS    = INFOSCREEN(2)
      IHS    = INFOSCREEN(3)
      NPX    = INFOSCREEN(4)
      NPY    = INFOSCREEN(5)
      NCOLR  = INFOSCREENMODE(6,MODE)
      NDEV   = MODE

     CALL ISCREENMODEOPTIONS(1,iws)
     CALL ISCREENMODEOPTIONS(2,ihs)

!     IF (NOPSYS .EQ. 1) THEN

!     ENDIF
      RETURN
      END
