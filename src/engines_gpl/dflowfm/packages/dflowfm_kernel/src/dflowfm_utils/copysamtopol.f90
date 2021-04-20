!> copy samples to polygon (for further operations)
subroutine copysamtopol()

   use M_SAMPLES
   use m_missing, only: dmiss, JINS
   use m_polygon, only: NPL, xpl, ypl, zpl, increasepol, savepol
   use geometry_module, only: dbpinpol

   implicit none

   integer, dimension(:), allocatable :: jacopy ! sample wil be copied (1) or not (0)

   integer                            :: i, inside, numcopy

!  allocate
   allocate(jacopy(Ns))
   jacopy = 1
   numcopy = NS

!  check if selecting polygon exists
   if ( NPL.gt.2 ) then
!     mark and count samples to be copied to polygon
      inside = -1 ! initialization of dbpinpol
      do i=1,NS
         call dbpinpol(xs(i), ys(i), inside, dmiss, JINS, NPL, xpl, ypl, zpl)
         if ( inside.ne.1 ) then
            jacopy(i) = 0
            numcopy = numcopy-1
         end if
      end do
   end if

!  check if samples were selected
   if ( numcopy.gt.0 ) then
!     copy selected samples to polygon
      call savepol()

      call increasepol(numcopy,0)

      NPL = 0
      do i=1,NS
         if ( jacopy(i).eq.1 ) then
            NPL=NPL+1
            xpl(NPL) = xs(i)
            ypl(NPL) = ys(i)
            zpl(NPL) = zs(i)
         end if
      end do
   end if

!  deallocate
   if ( allocated(jacopy) ) deallocate(jacopy)

   return
end subroutine copysamtopol
