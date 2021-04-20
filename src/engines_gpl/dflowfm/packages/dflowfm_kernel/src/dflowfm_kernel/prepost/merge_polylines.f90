!> merge polylines
subroutine merge_polylines()

   use m_polygon
   use m_missing
   use geometry_module, only: dbdistance, get_startend
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   double precision            :: xstart1, ystart1, xend1, yend1, xstart2, ystart2, xend2, yend2

   integer                     :: jpoint1, jstart1, jend1, jpoint2, jstart2, jend2
   integer                     :: ipol1, ipol2

   double precision, parameter :: dtol=1d-2


   jpoint1 = 1
   do while ( jpoint1.lt.NPL )
      call get_startend(NPL-jpoint1+1, xpl(jpoint1:NPL), ypl(jpoint1:NPL), jstart1, jend1, dmiss)
      jstart1 = jstart1 + jpoint1-1
      jend1   = jend1   + jpoint1-1

      xstart1 = xpl(jstart1)
      ystart1 = ypl(jstart1)
      xend1   = xpl(jend1)
      yend1   = ypl(jend1)

!     loop over the remaining polylines
      jpoint2 = jend1+1
      do while ( jpoint2.lt.NPL )
         call get_startend(NPL-jpoint2+1, xpl(jpoint2:NPL), ypl(jpoint2:NPL), jstart2, jend2, dmiss)
         jstart2 = jstart2 + jpoint2-1
         jend2   = jend2   + jpoint2-1

         xstart2 = xpl(jstart2)
         ystart2 = ypl(jstart2)
         xend2   = xpl(jend2)
         yend2   = ypl(jend2)

!        check if polylines end/start points are coinciding
         ipol1 = 0
         ipol2 = 0
         if ( dbdistance(xstart1,ystart1,xstart2,ystart2,jsferic, jasfer3D, dmiss).le.dtol ) then
            ipol1 = jstart1
            ipol2 = jstart2
         else if ( dbdistance(xstart1,ystart1,xend2,yend2,jsferic, jasfer3D, dmiss).le.dtol ) then
            ipol1 = jstart1
            ipol2 = jend2
         else if ( dbdistance(xend1,yend1,xstart2,ystart2,jsferic, jasfer3D, dmiss).le.dtol ) then
            ipol1 = jend1
            ipol2 = jstart2
         else if ( dbdistance(xend1,yend1,xend2,yend2,jsferic, jasfer3D, dmiss).le.dtol ) then
            ipol1 = jend1
            ipol2 = jend2
         end if

!        merge polylines
         if ( ipol1.gt.0 .and. ipol2.gt.0 ) then
!           delete first coinciding node
            call modln2(xpl,ypl,zpl,MAXPOL,NPL,ipol1,0d0,0d0,-2)
            if ( ipol1.eq.jend1 ) ipol1 = ipol1-1
            ipol2 = ipol2-1
!           merge
            call mergepoly(xpl,ypl,zpl,MAXPOL,NPL,ipol1,ipol2)

!           polygons may have been flipped
            call get_startend(NPL-jpoint1+1, xpl(jpoint1:NPL), ypl(jpoint1:NPL), jstart1, jend1, dmiss)
            jstart1 = jstart1 + jpoint1-1
            jend1   = jend1   + jpoint1-1

            xstart1 = xpl(jstart1)
            ystart1 = ypl(jstart1)
            xend1   = xpl(jend1)
            yend1   = ypl(jend1)

            jpoint2 = jend1+1
         else
!           advance pointer to second polyline
            jpoint2 = jend2+1
         end if

      end do

!     advance pointer to second polyline
      jpoint1 = jend1+1
   end do

!  remove trailing missing values
   if ( NPL.gt.1 ) then
      do while ( ( xpl(NPL).eq.DMISS .or. ypl(NPL).eq.DMISS ) .and. NPL.gt.1 )
         NPL=NPL-1
      end do
   end if

1234 continue

   return
end subroutine merge_polylines
