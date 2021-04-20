      integer FUNCTION USECP()
      implicit none
      integer :: ihour
      integer :: isecnd
      integer :: minute
      integer :: iy,im,id
      call dateandtimenow(iy,im,id,IHOUR,MINUTE,ISECND)
      USECP = 3600*IHOUR+60*MINUTE+ISECND
      RETURN
      END
