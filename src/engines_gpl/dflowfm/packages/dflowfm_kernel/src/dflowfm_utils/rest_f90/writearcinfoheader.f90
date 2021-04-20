      SUBROUTINE WRITEARCINFOHEADER(MARC,MC,NC,X0,Y0,DX,DY,dmiss)
      implicit none
      double precision :: dmiss
      double precision :: dx, dy
      integer :: marc
      integer :: mc
      integer :: nc
      double precision :: x0
      double precision :: y0
      WRITE(MARC,'(A,I8)')    'ncols         ', MC
      WRITE(MARC,'(A,I8)')    'nrows         ', NC
      WRITE(MARC,'(A,F13.3)') 'xllcorner     ', X0 - DX/2
      WRITE(MARC,'(A,F13.3)') 'yllcorner     ', Y0 - DY/2
      WRITE(MARC,'(A,2F13.3)') 'cellsize      ', DX, DY
      WRITE(MARC,'(A,F13.3)') 'NODATA_value  ', dmiss
      RETURN
      END
