!> copy growth velocities to the front, and add points in the front at corners
subroutine copy_vel_to_front(mc, nc, j, vel, ifront, nf, numf, xf, yf, velf, idxf)
   use m_missing

   implicit none

   integer,                             intent(in)    :: mc       !< number of grid points
   integer,                             intent(in)    :: nc       !< number of grid layers
   integer,                             intent(in)    :: j        !< grid layer

   double precision, dimension(2,mc),   intent(in)    :: vel      !  growth velocity vector at grid layer, per node

   integer,          dimension(mc),     intent(inout) :: ifront   !< active nodes (1) or not (0)

   integer,                             intent(inout) :: nf       !< front dimension
   integer,                             intent(in )   :: numf     !< array size
   double precision, dimension(numf),   intent(inout) :: xf, yf   !< front point coordinates
   double precision, dimension(2,numf), intent(inout) :: velf     !< front growth velocity vectors
   integer,          dimension(2,numf), intent(inout) :: idxf     !< (i,j)-indices of front points

   double precision, dimension(mc)                    :: xc1, yc1 !  active grid layer coordinates

   integer                                            :: i, ii, iprev, jprev, inext, jnext, num

   logical                                            :: LL, LR

   velf = 0d0

   num = 0  ! number of cornernodes (for ouput purposes only)
   i = 0
   do while ( i.lt.nf )
      i = i+1
      if ( idxf(2,i).eq.j .and. ifront(idxf(1,i)).eq.1 ) then
         velf(:,i) = vel(:,idxf(1,i))
         if ( velf(1,i).eq.DMISS ) velf(:,i) = 0d0

!        check for cornernodes
         iprev = idxf(1,max(i-1,1))
         jprev = idxf(2,max(i-1,1))
         inext = idxf(1,min(i+1,nf))
         jnext = idxf(2,min(i+1,nf))

         LL = (iprev.eq.idxf(1,i)-1 .and. jprev.eq.idxf(2,i) .and. ifront(iprev).eq.0 )
         LR = (inext.eq.idxf(1,i)+1 .and. jnext.eq.idxf(2,i) .and. ifront(inext).eq.0 )
         LL = LL .or. (iprev.eq.idxf(1,i) .and. jprev.lt.idxf(2,i))
         LR = LR .or. (inext.eq.idxf(1,i) .and. jnext.lt.idxf(2,i))
         if ( LL .or. LR ) then  ! stationary edge
            num = num+1
            if ( nf+1.gt.numf ) then
               call qnerror('growlayer: numf too small', ' ', ' ')
               cycle
            end if
!            if ( num.eq.1 ) write(6,"('cornernode: ', $)")
!            write (6,"(I0, ' ', $)") idxf(1,i)
            do ii=nf,i,-1
               xf(ii+1) = xf(ii)
               yf(ii+1) = yf(ii)
               velf(:,ii+1) = velf(:,ii)
               idxf(:,ii+1) = idxf(:,ii)
            end do
            nf = nf+1
            if ( LL ) then
               velf(:,i) = 0d0
            else
               velf(:,i+1) = 0d0
            end if
            i = i+1
         end if
      end if
   end do

!   if ( num.gt.0 ) write(6,*)

   return
end subroutine copy_vel_to_front
