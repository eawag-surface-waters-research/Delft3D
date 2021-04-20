subroutine droptracer(xp, yp, dval)
   use m_transport
   use m_flowgeom
   use m_flow, only: kmxn, kbot
   use m_polygon
   use m_missing, only: dmiss, JINS
   use geometry_module, only: dbpinpol

   implicit none

   double precision :: xp, yp   !< point coordinates
   double precision :: dval     !< value

   integer, dimension(:), allocatable :: icelllist
   integer                            :: Ncells

   integer                            :: i, k, kk, kb, kt
   integer                            :: N, in
   integer                            :: ja
   integer                            :: iconst

!  allocate
   allocate(icelllist(Ndx))
   icelllist = 0

!  add a tracer if no current tracer is selected (in visualization)
   if ( ITRA1.eq.0 .or. iconst_cur.eq.0 .or. iconst_cur.lt.ITRA1 ) then ! note: tracers always at the back
      call add_tracer('', iconst)
!     set current tracer (for visualization)
      iconst_cur = iconst
   else
!     select current tracer
      iconst = iconst_cur
   end if

!  find active flow nodes
   Ncells = 0
   if ( NPL.le.2 ) then ! no (usable) polygon
      call in_flowcell(xp,yp,kk)
      if ( kk.gt.0 ) then
         Ncells = Ncells + 1
         icelllist(1) = kk
      end if
   else
      in = -1
      do kk=1,Ndx
         N = size(nd(kk)%x)
         call dbpinpol(xz(kk), yz(kk), in, dmiss, JINS, NPL, xpl, ypl, zpl)
         if ( in.eq.1 ) then
            Ncells = Ncells+1
            icelllist(Ncells) = kk
         end if
      end do
   end if

!  fill active flow nodes
   do i=1,Ncells
      kk = icelllist(i)
      do k=kbot(kk),kbot(kk) + kmxn(kk) - 1
         constituents(iconst,k) = constituents(iconst,k) + dval
      end do
   end do

!  plot
   call tekflowstuff(ja)

!  deallocate
   if ( allocated(icelllist) ) deallocate(icelllist)

   return
end subroutine droptracer
