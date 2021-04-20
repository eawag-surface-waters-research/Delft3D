      SUBROUTINE TIMLIN()
      implicit none
      CHARACTER TIME*5
!     CALL IOsTime(IH,IM,IS)
!     WRITE (TIME,'(I2,1A,I2)') IH,':',IM
!     IF (IM .LE. 9) WRITE (TIME(4:4),'(1A)') '0'
!     IXP = INFOSCREEN(2) - 4
!     IYP = INFOSCREEN(3) - 1
!     CALL ITEXTCOLOUR('BRED','CYAN')
!     CALL IOutStringXY(IXP,IYP,TIME)
      RETURN
      END
