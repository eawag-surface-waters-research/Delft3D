      SUBROUTINE STOPJA(JA)
      use unstruc_files
      use m_devices
      implicit none
      integer :: imenutwo
      integer :: infocursor
      integer :: iopt
      integer :: ixp
      integer :: iyp
      integer :: ja
      IXP = INFOCURSOR(1)
      IYP = INFOCURSOR(2)
      CALL INPOPUP('ON')
      CALL ITEXTCOLOUR('BWHITE','RED')
      CALL INHIGHLIGHT('BLUE','BWHITE')
      CALL OKAY(0)
      IOPT = IMenuTwo                                                &
       ('NO','YES',(IWS-41)/2,IHS/2,'DO YOU REALLY WANT TO '//       &
        'QUIT THE PROGRAM ? ',1,1)
      CALL INPOPUP('OFF')
      IF (IOPT .EQ. 1) THEN
         JA = 0
      ELSE
         WRITE(msgbuf,'(A)') 'YOU STOPPED THE PROGRAM'
         call msg_flush()
         CALL IWinClose(1)
         CALL STOPINT()
      ENDIF
      RETURN
      END
