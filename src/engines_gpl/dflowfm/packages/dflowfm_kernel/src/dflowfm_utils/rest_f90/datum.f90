      SUBROUTINE DATUM(DATE)
      implicit none
      integer :: iyear, month, iday, ihour, minute, isecnd
      CHARACTER DATE*20
!              1  4  7   11 14 17
      DATE = 'hh:mm:ss, dd-mm-yyyy'

      call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

      WRITE(DATE( 1:2 ),'(I2.2)') IHOUR
      WRITE(DATE( 4:5 ),'(I2.2)') MINUTE
      WRITE(DATE( 7:8 ),'(I2.2)') ISECND
      WRITE(DATE(11:12),'(I2.2)') IDAY
      WRITE(DATE(14:15),'(I2.2)') MONTH
      WRITE(DATE(17:20),'(I4)') IYEAR
      RETURN
      END
