!> reconstruct velocity in cells
subroutine reconst_vel_coeffs()

   use m_flowgeom, only: Ndx, Lnx, bl  !, lne2ln  !, csu, snu, wu  !, xu, yu
   use m_flowparameters, only: epshs
   use m_partrecons
   use m_partmesh
   use m_alloc
   use m_sferic
   use geometry_module, only: dbdistance, gaussj

   implicit none

   integer,                          parameter   :: N = 4

   double precision, dimension(N,N)              :: Amat ! matrix
   double precision, dimension(N)                :: rhs

   double precision                              :: cs, sn

   integer                                       :: i, icell, j, jj, k, L, NN
   integer                                       :: k1, k2
   integer                                       :: i12, isign
   integer                                       :: ierror

   ierror = 1

!  allocate startpointers
   call realloc(jreconst, numcells+1, keepExisting=.false., fill=0)

!  set startpointers
   jreconst(1) = 1
   do icell=1,numcells
      jreconst(icell+1) = jcell2edge(icell) + jcell2edge(icell+1)-jcell2edge(icell)
   end do

!  allocate column indices and matrix entries
   NN = jreconst(numcells+1)-1
   call realloc(ireconst, NN, keepExisting=.false., fill=0)
   if ( jsferic.eq.0 ) then
      call realloc(Areconst, (/3, NN /), keepExisting=.false., fill=0d0)
   else
      call realloc(Areconst, (/4, NN /), keepExisting=.false., fill=0d0)
   end if

!  dummy rhs
   rhs = 0d0

!  loop over internal cells
   jj = 0
   do icell=1,numcells
!     check for triangles
!      if ( jcell2edge(k+1)-jcell2edge(k).ne.3 ) then
!         cycle
!      end if

!     get flownode number (for s, bl)
      k = iabs(cell2nod(icell))

!     fill system for (ux,uy) = (ux0, uy0) + alpha (x-x0, y-y0)

!     loop over edges (netlinks)
      i = 0
      do j=jcell2edge(icell),jcell2edge(icell+1)-1
         i = i+1
         L = icell2edge(j) ! netlink

         k1 = edge2node(1,L)
         k2 = edge2node(2,L)

         if ( jsferic.eq.0 ) then
            cs = dnx(1,L)
            sn = dny(1,L)

!           add to system
            Amat(i,1) = cs
            Amat(i,2) = sn
            Amat(i,3) = (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*cs + (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*sn
         else
            i12 = 1
            isign = 1
            if ( edge2cell(2,L).eq.icell ) then
              i12 = 2
              isign = -1d0
            end if

            Amat(i,1) = dnx(i12,L)*isign
            Amat(i,2) = dny(i12,L)*isign
            Amat(i,3) = dnz(i12,L)*isign
            Amat(i,4) = ( (0.5d0*(xnode(k1)+xnode(k2))-xzwcell(icell))*dnx(i12,L) + &
                          (0.5d0*(ynode(k1)+ynode(k2))-yzwcell(icell))*dny(i12,L) + &
                          (0.5d0*(znode(k1)+znode(k2))-zzwcell(icell))*dnz(i12,L) ) * isign
         end if

         jj = jj+1
         ireconst(jj) = L
      end do

      if ( jsferic.eq.0 ) then
!        solve system
         call gaussj(Amat,3,N,rhs,1,1)

         do i=1,3
            L = icell2edge(jcell2edge(icell)+i-1)
            Areconst(1:3,jreconst(icell)+i-1) = Amat(1:3,i)
         end do
      else
!        impose zero cell normal velocity
         Amat(4,1:3) = dnn(:,icell)
         Amat(4,4) = 0d0

!        solve system
         call gaussj(Amat,4,N,rhs,1,1)

         do i=1,3
            L = icell2edge(jcell2edge(icell)+i-1)
            Areconst(1:4,jreconst(icell)+i-1) = Amat(1:4,i)
         end do

      end if
   end do

   ierror = 0
!  error handling
1234 continue

   return
end subroutine reconst_vel_coeffs
