      SUBROUTINE TXTLINES()
      use m_devices
      use m_textlines
      implicit none
      integer :: i

      CALL IGRCHARSIZE(real(TXSIZE),real(TXSIZE))

       do i = 1,3
          if (len_trim(TXLIN(i)) > 0) then
             CALL MTEXT(TXLIN(i), TXXpos, TXYpos+0.04d0*(4-i), 3)
          endif
       enddo

      CALL SETTEXTSIZE()

      RETURN
      END
