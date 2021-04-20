!> select link for directional refinement in GUI
   subroutine getlink_GUI(xp, yp, L)
      implicit none

      double precision, intent(out) :: xp, yp   !< coordinates of clicked point
      integer,          intent(out) :: L        !< clicked link number

      double precision     :: zp

      integer              :: num, nwhat, nput, numb, key

      L     = 0

      call ktext(' Refine net       ',1,3,15)

      num   = 0
      nwhat = 0
      nput  = 55
      numb  = 10
      key   = 0

      do
         CALL DRAWNU(KEY)
         call putget_un(num,nwhat,nput,numb,xp,yp,key)

         if ( key.eq.23 )  then     ! escape
            exit
         else if ( key.eq.21) then  ! left mouse button
            call islink(L,xp,yp,zp)
            if ( L.gt.0 ) then   ! link found
               call teklink(L,31)
               exit
            end if
!        the following is copied from editnetw (zoom, panning)
         else if (key .eq. 43 .or. key .eq. 140) then
            call kplotplusmin(1)
            key = 3
         else if (key .eq. 45 .or. key .eq. 141) then
            call kplotplusmin(-1)
            key = 3
         else if (key .eq. 133) then        ! page down
            call nplotplusmin(1)
            key = 3
         else if (key .eq. 143) then        ! delete
            call nplotplusmin(-1)
            key = 3
         end if
      end do

      if ( L.lt.1 ) then
         call qnerror('no link clicked: exitting', ' ', ' ')
      end if

      return
   end subroutine
