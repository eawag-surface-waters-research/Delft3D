      subroutine dhnlun(istart,lun)

!     function : gives next free unit number, starting at istart till istart + 98

!     (c) Deltares

!     declaration of the arguments

      integer               , intent(in)    :: istart       ! start looking from here
      integer               , intent(out)   :: lun          ! next free unit number

!     local declaration

      integer                               :: ilun         ! loop counter
      logical                               :: lopen        ! opened indicator

      lun = 0
      do ilun = istart, istart + 1000
         inquire(ilun,opened=lopen)
         if ( .not. lopen ) then
            lun = ilun
            exit
         endif
      enddo

      return
      end
