      SUBROUTINE INIKEYS()
      use m_devices
      implicit none
      integer :: i
      integer :: nkey
      integer :: numc
      integer :: numkeys
      COMMON /NKEYS/ NUMKEYS, NKEY(20), NUMC(20)
!     Keyboard
      NKEY( 1) = 142
      NKEY( 2) = 166
      NKEY( 3) =  13
      NKEY( 4) =  27
      NKEY( 5) = 171
      NKEY( 6) = 172
      NKEY( 7) = 173
      NKEY( 8) =   9

      IF (NOPSYS .GT. 1) THEN
         NKEY(9) = 259
      ELSE
         NKEY(9) = NKEY(8)
      ENDIF

!     Muistoetsen
      NKEY(10) = 251
      NKEY(11) = 252
      NKEY(12) = 253


      NUMC( 1) =  21
      NUMC( 2) =  22
      NUMC( 3) =  22
      NUMC( 4) =  23
      NUMC( 5) =  24
      NUMC( 6) =  25
      NUMC( 7) =  26
      NUMC( 8) =  27

      IF (NOPSYS .GT. 1) THEN
         NUMC(9) = 50
      ELSE
         NUMC(9) = NUMC(8)
      ENDIF

      NUMC(10) =  21
      NUMC(11) =  22
      NUMC(12) =  22

      NUMKEYS = 9
      DO 10 I = 1,NUMKEYS
         CALL INCONTROLKEY(NUMC(I),NKEY(I))
   10 CONTINUE
      NUMKEYS = 12

!     INS CONFIRM                  CALL INConTRoLkey(21, 142)
!     ENTER CONFIRM                CALL INConTRoLkey(22,  13)
!     ENTER KEYPAD CONFIRM         CALL INConTRoLkey(22, 166)
!     ESC                          CALL INConTRoLkey(23,  27)
!     F1 HELP                      CALL INConTRoLkey(24, 171)
!     F2 HISTORY                   CALL INConTRoLkey(25, 172)
!     F3 COMMAND                   CALL INConTRoLkey(26, 173)
!     TAB SWITCH TUSSEN 3 SCHERMEN CALL INConTRoLkey(27,   9)
!     EXPOSE RESIZE                CALL INCONTROLKEY(50, 259)
      RETURN
      END
