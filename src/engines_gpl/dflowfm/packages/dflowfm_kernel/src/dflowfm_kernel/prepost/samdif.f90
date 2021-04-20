!> take difference of samples with second sample set within tooclose distance
subroutine samdif()

   use m_polygon
   use m_samples
   use network_data, only: tooclose
   use kdtree2Factory
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance

   implicit none

   double precision            :: dist

   integer                     :: i, ipnt, ierror
   integer                     :: numnoval

   double precision, parameter :: VAL_NOPNT = 1234d0
   double precision, parameter :: dtol = 1d-8

   if ( NS.lt.1 .or. NS3.lt.2 ) goto 1234

!  build kdtree
   call build_kdtree(treeglob,NS3, xs3, ys3, ierror, jsferic, dmiss)

!  reallocate results vector (fixed size)
   call realloc_results_kdtree(treeglob,1)

   if ( ierror.ne.0 ) goto 1234

   call savesam()

   numnoval = 0   ! count number of samples without a polygon node
   do i=1,Ns
!     fill query vector
      call make_queryvector_kdtree(treeglob,xs(i),ys(i), jsferic)

!     find nearest polygon point
      call kdtree2_n_nearest(treeglob%tree,treeglob%qv,1,treeglob%results)
      ipnt = treeglob%results(1)%idx

      if ( ipnt.gt.0 .and. ipnt.le.Ns3 ) then   ! safety
!        check distance to nearest polygon node
         dist = dbdistance(xs(i),ys(i),xs3(ipnt),ys3(ipnt),jsferic, jasfer3D, dmiss)
         if ( dist.lt.tooclose .and. zs(i).ne.DMISS .and. zs3(ipnt).ne.DMISS ) then
            zs(i) = zs(i) - zs3(ipnt)

!           remove (nearly) zero values
            if ( abs(zs(i)).lt.dtol ) then
               zs(i) = DMISS
            end if
         else
            zs(i) = VAL_NOPNT
            numnoval = numnoval+1
         end if
      else
         zs(i) = VAL_NOPNT
         numnoval = numnoval+1
      end if
   end do

   call delpol()

   if ( numnoval.gt.0 ) then
!     copy unassociated samples to polygon
      call increasepol(numnoval,0)

      NPL = 0
      do i=1,NS
         if ( zs(i).eq.VAL_NOPNT ) then
            zs(i) = DMISS
            NPL=NPL+1
            xpl(NPL) = xs(i)
            ypl(NPL) = ys(i)
            zpl(NPL) = zs(i)
         end if
      end do
   end if

   ierror = 0

1234 continue

!  deallocate kdtree if it was created
   if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)

   return
end subroutine samdif
