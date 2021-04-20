!>    move a whole spline
      subroutine movespline(ispline, inode, xp, yp)
         use m_splines
         implicit none


         integer,          intent(in) :: ispline !< spline number
         integer,          intent(in) :: inode   !< spline control point
         double precision, intent(in) :: xp, yp !< new active spline control point (np) coordinates

         double precision             :: dx, dy

         integer                      :: num

         call nump(ispline,num)

         if ( ispline.gt.0 .and. ispline.le.maxspl .and. inode.gt.0 .and. inode.le.num) then
            dx = xp - xsp(ispline, inode)
            dy = yp - ysp(ispline, inode)
            xsp(ispline, 1:maxsplen) = xsp(ispline, 1:maxsplen) + dx
            ysp(ispline, 1:maxsplen) = ysp(ispline, 1:maxsplen) + dy
         end if
         return
      end subroutine movespline
