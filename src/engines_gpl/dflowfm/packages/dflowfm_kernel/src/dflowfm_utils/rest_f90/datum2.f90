      SUBROUTINE DATUM2(DATE)
      use unstruc_display, only : jadatetime
      use string_module, only: get_dirsep
      implicit none
      integer :: iyear, month, iday, ihour, minute, isecnd
      CHARACTER DATE*20

!              1  4  7   11 14 17

      if (jadatetime == 0) then
         DATE = get_dirsep()
      else
         DATE = '_yymmddhhmmss'//get_dirsep()

         call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

         WRITE(DATE(2:3),'(I2.2)') IYEAR - 2000
         WRITE(DATE(4:5),'(I2.2)') month
         WRITE(DATE(6:7),'(I2.2)') iday
         WRITE(DATE(8:9),'(I2.2)') Ihour
         WRITE(DATE(10:11),'(I2.2)') minute
         WRITE(DATE(12:13),'(I2.2)') isecnd
      endif
      RETURN
      END
