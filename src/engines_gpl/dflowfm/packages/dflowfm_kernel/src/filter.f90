! filter
   
!> initialize filter
subroutine ini_filter(ierr)
   use m_flowgeom, only: Lnx, ln, ln2lne, nd, lne2ln, dx, wu, ba, ban, lncn, xu, yu, csu, snu
   use network_data, only: lne, nmk, kn, nod, nb, kc
   use m_filter
   use m_solver
   use m_alloc
   use unstruc_messages
   use unstruc_model, only: md_netfile
   use dfm_error
   implicit none
   
   integer,               intent(out) :: ierr   ! error (1) or not (0)
   
   
   integer,          dimension(:), allocatable :: iLvec  !< vector Laplacian in CRS format, startpointers
   integer,          dimension(:), allocatable :: jLvec  !< vector Laplacian in CRS format, row numbers
   double precision, dimension(:), allocatable :: ALvec  !< vector Laplacian in CRS format, matrix entries
   
   integer, dimension(:), allocatable :: num
   
   integer, dimension(:), allocatable :: dum
   
   integer, parameter                 :: LENFILNAM = 128
   
   character(len=LENFILNAM)           :: FNAM
   
   double precision                   :: dfacDiv, dfacCurl, dfac, val
                                      
   integer                            :: kk, k
   integer                            :: L, L2, Lf, Lf2, LL2
   integer                            :: nn, n
   integer                            :: numtot
                                      
   integer                            :: istart, iend
   integer                            :: i, ipoint
   
   integer                            :: len
                                      
   integer                            :: jadebug = 1
   
   interface
      subroutine matprodCRS(NrowsB, ib,jb,b, NrowsC, ic,jc,c, ia,ja,a)
         integer,                                     intent(in)  :: NrowsB !< number of rows of matrix B
         integer,          dimension(:),              intent(in)  :: ib     !< startpointers     matrix B
         integer,          dimension(:),              intent(in)  :: jb     !< row numbers       matrix B
         double precision, dimension(:),              intent(in)  :: b      !< entries           matrix B
         integer,                                     intent(in)  :: NrowsC !< number of rows of matrix C
         integer,          dimension(:),              intent(in)  :: ic     !< startpointers     matrix C
         integer,          dimension(:),              intent(in)  :: jc     !< row numbers       matrix C
         double precision, dimension(:),              intent(in)  :: c      !< entries           matrix C
         integer,          dimension(:), allocatable, intent(out) :: ia     !< start pointers    matrix A
         integer,          dimension(:), allocatable, intent(out) :: ja     !< row numbers       matrix A
         double precision, dimension(:), allocatable, intent(out) :: a      !< entries           matrix A
      end subroutine matprodCRS
   end interface
   
!  construct vector Laplacian
!  boundary conditions: u.n = 0, n.du/ds = 0
   
   ierr = 1
   
!  set node mask: 1 internal 2D, other elsewhere
   kc = 1
   call MAKENETNODESCODING()
   
!  get upper bound for number of non-zero entries per row
   allocate(num(Lnx))
   num = 0
   
!  loop over flowlinks
   do Lf=1,Lnx
!     Div-part: loop over left,right neighboring cell   
      do kk=1,2
!        get cell number
         k = ln(kk,Lf)
         if ( k.eq.0 ) cycle

!        add to upper bound
         num(Lf) = num(Lf) + nd(k)%lnx
      end do
      
!     Curl-part: get netlink
      L = iabs(ln2lne(Lf))
!     loop over left,right netnode
      do nn=1,2
!        get netnode number
         n = kn(nn,L)
         
!        add to upper bound
         num(Lf) = num(Lf) + nmk(n)
      end do
   end do
   
!  allocate and construct startpointers with upper bound
   call realloc(iLvec,Lnx+1,keepExisting=.false.)
   iLvec(1)=1
   do Lf=1,Lnx
      iLvec(Lf+1) = iLvec(Lf) + num(Lf)
   end do
   
!  allocate CRS with upper bound
   numtot = iLvec(Lnx+1)-1
   call realloc(jLvec,numtot,fill=0,keepExisting=.false.)
   call realloc(ALvec,numtot,fill=0d0,keepExisting=.false.)
   
!  construct row numbers and fill matrix
   num = 0
!  loop over edges
   do Lf=1,Lnx
!     get start- and endpointer
      istart = iLvec(Lf)
      iend = iLvec(Lf+1)-1
         
!     Div-part: loop over left, right neighboring cell
      dfacDiv = 1d0/Dx(Lf)
      do kk=1,2
!        account for orientation
         dfacDiv = -dfacDiv
         
         k = ln(kk,Lf)
         
         if ( k.eq.0 ) cycle
         
         dfac = dfacDiv/ba(k)
         
!        loop over edges of cell that are flowlinks
         do LL2=1,nd(k)%lnx
            Lf2 = iabs(nd(k)%ln(LL2))
            if ( Lf2.eq.0 ) then
               call mess(LEVEL_ERROR, 'ini_filter: zero link number')
               goto 1234
            end if
            
            if ( k.eq.ln(1,Lf2) ) then
               val = dfac*wu(Lf2)
            else if ( k.eq.ln(2,Lf2) ) then
               val = -dfac*wu(Lf2)
            else
               call mess(LEVEL_ERROR, 'ini-filter: error in div')
            end if
            
!           add element on row
            call add_rowelem(jLvec(istart),ALvec(istart),iend-istart+1,Lf2,val,num(Lf))
         end do
      end do
      
!     Curl-part: loop over left, right netnode
      dfacCurl = -1d0/wu(Lf)
      do nn=1,2
!        account for orientation
         dfacCurl = -dfacCurl
         
!        get netnode number
         n = lncn(nn,Lf)
         
!        boundary condition: curl u = 0
         if ( nb(n).ne.1 ) cycle
         
         if ( n.eq.0 ) then
            call mess(LEVEL_ERROR, 'ini_filter: zero node number')
            goto 1234
         end if
         
         dfac = dfacCurl/(ban(n))
         
!        loop over attached flowlinks
         do LL2=1,nmk(n)
!           get netlink number
            L2 = nod(n)%lin(LL2)
!           get flowlink number
            Lf2 = lne2ln(L2)
            
            if ( Lf2.le.0 ) cycle ! boundary
            
            if ( n.eq.lncn(2,Lf2) ) then
               val = dfac*Dx(Lf2)
            else if( n.eq.lncn(1,Lf2) ) then
               val = -dfac*Dx(Lf2)
            else
               call mess(LEVEL_ERROR, 'ini_filter: error in curl')
            end if
            
!           add row element
            call add_rowelem(jLvec(istart),ALvec(istart),iend-istart+1,Lf2,val,num(Lf))
         end do
      end do
   end do
   
!  remove zeros
   ipoint=1
   istart=iLvec(1)
   do Lf=1,Lnx
      do i=istart,iLvec(Lf+1)-1
         if ( jLvec(i).ne.0 ) then
            jLvec(ipoint) = jLvec(i)
            ALvec(ipoint) = ALvec(i)
            ipoint=ipoint+1
         else
            exit
         end if
      end do
      istart = iLvec(Lf+1)
      iLvec(Lf+1) = ipoint
   end do
   
   N = iLvec(Lnx+1)-1
   call realloc(jLvec,N,keepExisting=.true.)
   call realloc(ALvec,N,keepExisting=.true.)
   
!  compute biharmonic operator
   call matprodCRS(Lnx, iLvec,jLvec,ALvec, Lnx, iLvec,jLvec,ALvec, iLvec2,jLvec2,ALvec2)

   if ( jadebug.eq.1 ) then
   
!     get netfile basename
      len = index(md_netfile, '_net')-1

      if ( len.lt.1 .or. len+6.gt.LENFILNAM ) then
         call qnerror('write domains: net filename error', ' ', ' ')
         goto 1234
      end if
   
      FNAM = md_netfile(1:len) // '_flt.m'
      call realloc(num, Lnx+1)
      call realloc(dum, Lnx)
      dum = 1
      do Lf=1,Lnx+1
         num(Lf) = Lf
      end do

      call writematrix(FNAM, Lnx, iLvec, jLvec, ALvec, 'L', 0)
      call writematrix(FNAM, Lnx, iLvec2, jLvec2, ALvec2, 'L2', 1)
      call writematrix(FNAM, Lnx, num, dum, xu, 'xu', 1)
      call writematrix(FNAM, Lnx, num, dum, yu, 'yu', 1)
      call writematrix(FNAM, Lnx, num, dum, csu, 'csu', 1)
      call writematrix(FNAM, Lnx, num, dum, snu, 'snu', 1)
   end if
   
   ierr = 0
1234 continue
   

!  deallocate
   if ( allocated(num) ) deallocate(num)
   if ( allocated(dum) ) deallocate(dum)
   if ( allocated(iLvec) ) deallocate(iLvec)
   if ( allocated(jLvec) ) deallocate(jLvec)
   if ( allocated(ALvec) ) deallocate(ALvec)
   
   return
end subroutine ini_filter

!> add element to row
subroutine add_rowelem(jA,A,N,j,val,num)
   use unstruc_messages
   implicit none
   
   integer,                        intent(in)    :: N      !< array length
   integer, dimension(N),          intent(inout) :: jA     !< rownumber array
   double precision, dimension(N), intent(inout) :: A      !< matrix values
   integer,                        intent(in)    :: j      !< rownumber to be inserted
   double precision,               intent(in)    :: val    !< value to be inserted
   integer,                        intent(inout) :: num    !< number of nonzero entries
   
   integer :: i, i2
   
   
   do i=1,N
      if ( jA(i).eq.0 ) then
!        no non-zero array members remaining
         jA(i) = j
         num = num+1
         A(i) = val
         exit
      else if ( jA(i).eq.j ) then
!        entry already exists
         A(i) = A(i) + val
         exit
      else if ( jA(i).gt.j ) then
!        insert entry
         do i2=N,i+1,-1
            jA(i2) = jA(i2-1)
            A(i2) = A(i2-1)
         end do
         jA(i) = j
         A(i) = val
         num = num+1
         exit
      else if ( i.eq.N ) then
!        check for suffucient array size
         call mess(LEVEL_ERROR, 'insert_rownumber: error')
      else
!        proceed to next array member
      end if
   end do
   
   return
end subroutine add_rowelem

!> compute matrix product A = B C in CRS format
subroutine matprodCRS(NrowsB, ib,jb,b, NrowsC, ic,jc,c, ia,ja,a)
   use m_alloc
   implicit none
   
   integer,                                     intent(in)  :: NrowsB !< number of rows of matrix B
   integer,          dimension(:),              intent(in)  :: ib     !< startpointers     matrix B
   integer,          dimension(:),              intent(in)  :: jb     !< row numbers       matrix B
   double precision, dimension(:),              intent(in)  :: b      !< entries           matrix B
   integer,                                     intent(in)  :: NrowsC !< number of rows of matrix C
   integer,          dimension(:),              intent(in)  :: ic     !< startpointers     matrix C
   integer,          dimension(:),              intent(in)  :: jc     !< row numbers       matrix C
   double precision, dimension(:),              intent(in)  :: c      !< entries           matrix C
   
   integer,          dimension(:), allocatable, intent(out) :: ia     !< start pointers    matrix A
   integer,          dimension(:), allocatable, intent(out) :: ja     !< row numbers       matrix A
   double precision, dimension(:), allocatable, intent(out) :: a      !< entries           matrix A
   
   integer,          dimension(:), allocatable              :: jdum
   
   integer                                                  :: irowB, icolB, irowC, icolC
   integer                                                  :: ipoint
   integer                                                  :: irowA, icolA
   integer                                                  :: kB, kC
   integer                                                  :: j
   integer                                                  :: NDIMA
   
   call realloc(iA, NrowsB+1, fill=0, keepExisting=.false.)
   call realloc(jdum, NrowsC, fill=0, keepExisting=.false.)
   
!  initialize matrix A
   NDIMA = max(iB(NrowsB+1)-1,iC(NrowsC+1)-1)
   call realloc(jA, NDIMA, fill=0)
   call realloc(a, NDIMA, fill=0d0)
   
   ipoint = 1
   
!  loop over rows of B ( "row loop" of output matrix A)
   do irowB=1,NrowsB
!     set start pointer
      iA(irowB) = ipoint
      
!     empty array with column pointers
      jdum = 0
      
!     for this row of B, loop over columns of B ("inner loop")
      do kB=iB(irowB),iB(irowB+1)-1
         icolB = jB(kB)
         
!        pair row of C with column of B
         irowC = icolB
         
!        for this row of C, loop over columns of C  ( "column loop" of output matrix A)
         do kC=iC(irowC),iC(irowC+1)-1
            icolC=jC(kC)
            
!           see if entry at (irowB,icolC) was already encountered
            j=jdum(icolC)
            if ( j.eq.0 ) then
!              check array sizes
               if ( ipoint.gt.NDIMA ) then
!                 increase array sizes
                  NDIMA = floor(1.2*dble(ipoint))+1
                  call realloc(ja,NDIMA,keepExisting=.true.,fill=0)
                  call realloc(a,NDIMA,keepExisting=.true.,fill=0d0)
               end if
               
!              add new entry
               jA(ipoint) = icolC
               
!              store location
               jdum(icolC) = ipoint
               j = ipoint
               
!              advance pointer               
               ipoint = ipoint+1
            else
!              get location            
               j = jdum(icolC)
            end if
            
!           add to matrix product
            a(j) = a(j) + b(kB)*c(kC)
         end do
      end do
   end do
   
   iA(NrowsB+1) = ipoint
   
   if ( allocated(jdum) ) deallocate(jdum)
   
!  trim arrays
   if ( NDIMA.gt.ipoint-1) then
      NDIMA = iA(NrowsB+1)-1
      call realloc(jA,NDIMA,keepExisting=.true.)
      call realloc(a,NDIMA,keepExisting=.true.)
   end if
   
   return
end subroutine matprodCRS

!> apply filter
subroutine filterfuru()
   use m_filter
   use m_flow
   implicit none
   
   integer :: k
   
!  compute matrix entries
   
   
   return
end subroutine filterfuru