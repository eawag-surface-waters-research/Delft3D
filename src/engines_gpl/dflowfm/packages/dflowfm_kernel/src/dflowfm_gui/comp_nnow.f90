!> compute the difference with the optimal number of links by counting the numbers of links that:
!>   connect nodes k1 and k2, and
!>   are at the same side of the land boundary path through node k, or
!>   are on the land boundary path
subroutine comp_nnow(k,k1in,k2in,n)
   use m_netw
   use m_landboundary
   implicit none

   integer, intent(in)  :: k      !< center node
   integer, intent(in)  :: k1in, k2in !< connected nodes
   integer, intent(out) :: n      !< difference from optimum

   integer              :: num    !< number of links at one side of the path
   integer              :: numopt !< optimal number of links

   integer             :: k1, k2
   integer             :: kother
   integer             :: kk, kk1, kk2
   integer             :: L, Lp1, Lp2

   logical             :: Lfound

   integer, external   :: nmk_opt

   logical, external   :: rechtsaf_active

   if ( lanseg_map(k).lt.1 ) then
      n = nmk(k)-nmk_opt(k)
      return
   end if


!  links connected to k1 and k2 need to be counterclockwise
   if ( rechtsaf_active(k, k1in, k2in ) ) then
      k1 = k2in
      k2 = k1in
   else
      k1 = k1in
      k2 = k2in
   end if

   num    = 0
   numopt = 0
   n      = 0

   Lp1 = 0  ! first link in path
   Lp2 = 0  ! second link in path

!  find the link that connects node k1
   Lfound = .false.
   do kk=1,nmk(k)
      L = nod(k)%lin(kk)
      if ( kn(1,L).eq.k1 .or. kn(2,L).eq.k1 ) then
         kk1 = kk
         Lfound = .true.
         exit
      end if
   end do

   if ( .not.Lfound ) goto 1234

   if ( kn(1,L).ne.k .and. kn(2,L).ne.k ) then ! something wrong
      goto 1234
   end if

!  find the link that connects node k2
   Lfound = .false.
   do kk=1,nmk(k)
      L = nod(k)%lin(kk)
      if ( kn(1,L).eq.k2 .or. kn(2,L).eq.k2 ) then
         kk2 = kk
         Lfound = .true.
         exit
      end if
   end do

   if ( .not.Lfound ) goto 1234

   if ( kn(1,L).ne.k .and. kn(2,L).ne.k ) then ! something wrong
      goto 1234
   end if

!  start counting

!  count the numbers of links clockwise from the one connecting k1 that are not in a land/net boundary path
   kk=kk1
   L = nod(k)%lin(kk)
   kother = kn(1,L)+kn(2,L)-k
   num = 1
   do while ( lanseg_map(kother).lt.1 .and. kk.ne.kk2 .and. lnn(L).gt.1 )
      kk=kk-1
      if ( kk.lt.1 ) kk=kk+nmk(k)
      L = nod(k)%lin(kk)
      kother = kn(1,L)+kn(2,L)-k
      num = num+1
   end do

   if ( lanseg_map(kother).gt.0 .or. lnn(L).lt.2 ) Lp1 = L ! first link in path

!  if not all links are visited, count counterclockwise from the one connecting k2
   if ( kk.ne.kk2 ) then
      kk=kk2
      L = nod(k)%lin(kk)
      kother = kn(1,L)+kn(2,L)-k
      num = num+1
      do while( lanseg_map(kother).lt.1 .and. kk.ne.kk1 .and. L.ne.Lp1 .and. lnn(L).gt.1)
         kk=kk+1
         if ( kk.gt.nmk(k) ) kk=kk-nmk(k)
         L = nod(k)%lin(kk)
         kother = kn(1,L)+kn(2,L)-k
         if ( kk.ne.kk1 .and. L.ne.Lp1 ) num = num+1   ! kk1 already visited
      end do

      if ( (lanseg_map(kother).gt.0 .or. lnn(L).lt.2) .and. L.ne.lp1  ) Lp2 = L ! second link in path
   end if

   if ( num.gt.nmk(k) ) then  ! should not happen
      call qnerror('comp_nnow: num>nmk', ' ', ' ')
   end if

   if ( Lp1.gt.0 .and. Lp2.gt.0 ) then  ! internal boundary
      numopt = 4
   else
      numopt = 6
   end if

   n = num-numopt

   return

1234 continue

   return
end subroutine comp_nnow
