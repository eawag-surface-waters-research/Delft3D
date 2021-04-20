!> Refine entire current polyline from start to end.
      subroutine refinepolygon()
      use m_polygon  !, only: npl, dxuni
      use m_tpoly
      use m_sferic
      use m_missing
      use geometry_module, only: dbdistance, half
      implicit none
      integer :: i1, i2
      integer :: key

      type(tpoly), dimension(:), allocatable :: pli, pliout              ! tpoly-type polygons

      double precision                       :: dl, xnew, ynew, znew

      integer                                :: numpols, numpolsout      ! number of tpoly-type polygons
      integer                                :: i
      integer                                :: iter, j
      integer                                :: M, NPUT

      i1 = 1
      i2 = npl
      call refinepolygonpart(i1,i2,0)

      call TYPEVALUE(dxuni,key)

      call pol_to_tpoly(numpols, pli, keepExisting=.false.)
      call delpol()

      write(6,*) numpols
      do i=1,numpols
         write(6,*) i
         call tpoly_to_pol(pli,iselect=i)
!         i1 = 1
!         i2 = NPL
!         call refinepolygonpart(i1,i2,1)

!        loop over polygon points
         j = 1
         do while ( j.lt.NPL )
!           get length
            dl = dbdistance(xpl(j), ypl(j), xpl(j+1), ypl(j+1), jsferic, jasfer3D, dmiss)

!           check length
            if ( dl.gt.dxuni ) then
!              compute new point coordinates
               call half(xpl(j), ypl(j), xpl(j+1), ypl(j+1),xnew,ynew,jsferic,jasfer3D)
               znew = DMISS
               if ( zpl(j).ne.DMISS .and. zpl(j+1).ne.DMISS ) then
                  znew = 0.5*(zpl(j)+zpl(j+1))
               end if
!              add point
               call increasepol(NPL+1, 1)
               NPUT = -1
               M = j
               CALL MODLN2(XPL, YPL, ZPL, MAXPOL, NPL, M, xnew, ynew, NPUT)
               ZPL(M) = znew
            else
               j = j+1
            end if
         end do

         call pol_to_tpoly(numpolsout, pliout, keepExisting=.true.)
         call delpol()
      end do

      call tpoly_to_pol(pliout)
      call dealloc_tpoly(pli)
      call dealloc_tpoly(pliout)

      end subroutine refinepolygon
