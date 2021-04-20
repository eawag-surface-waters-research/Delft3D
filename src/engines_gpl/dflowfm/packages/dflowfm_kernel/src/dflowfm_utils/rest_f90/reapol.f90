      subroutine reapol(mpol, jadoorladen)
      implicit none
      integer :: mpol
      integer, intent(in)           :: jadoorladen !< Append to existing polygons (intended to read multiple crs files)
      integer                       :: ipli
      ipli = 0
      call reapol_nampli(mpol, jadoorladen, 0, ipli)
      end subroutine reapol
