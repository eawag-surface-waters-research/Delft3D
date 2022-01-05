      subroutine from_ddb1(hyd)

      ! function : work from the ddb file

      ! (c) Deltares

      ! global declarations

      use hydmod
      implicit none

      ! declaration of the arguments

      type(t_hyd)         :: hyd                    ! description of the hydrodynamics

      ! local declarations

      integer                   :: i_domain1              ! index in collection
      integer                   :: i_domain2              ! index in collection
      type(t_domain)            :: domain                 ! one domain description
      integer                   :: n_dd_bound             ! number of dd-boundaries
      integer                   :: i_dd_bound             ! index in collection
      type(t_dd_bound),pointer  :: dd_bound               ! one dd_bound description

      character(len=256)        :: outhydname             ! output filename
      character(len=256)        :: filext                 ! file extension
      integer                   :: extpos                 ! start position of file extension
      integer                   :: extlen                 ! length of file extension

      ! read the contents of the ddb file

      hyd%file_com%name = hyd%file_hyd%name
      call read_ddb(hyd)

      ! get the domains from the dd boundaries

      n_dd_bound = hyd%dd_bound_coll%cursize
      do i_dd_bound = 1 , n_dd_bound

         dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)

         ! look up the domain names

         i_domain1 = domain_coll_find(hyd%domain_coll,dd_bound%name1)
         if ( i_domain1 .le. 0 ) then
            domain%name = dd_bound%name1
            domain%mmax = -999
            domain%nmax = -999
            domain%aggr = ' '
            i_domain1 = domain_coll_add(hyd%domain_coll, domain)
         endif
         i_domain2 = domain_coll_find(hyd%domain_coll,dd_bound%name2)
         if ( i_domain2 .le. 0 ) then
            domain%name = dd_bound%name2
            domain%mmax = -999
            domain%nmax = -999
            domain%aggr = ' '
            i_domain2 = domain_coll_add(hyd%domain_coll, domain)
         endif
      enddo

      ! some prelim initialisation of hyd
      
      call dhfext(hyd%file_hyd%name,filext, extpos, extlen)
      outhydname = 'com-'//trim(hyd%file_hyd%name(1:extpos-1))
      call set_hyd(hyd,outhydname)
      hyd%task = HYD_TASK_FULL

      return
      end
