      subroutine overall_hyd(waq_output_dir,hyd,n_domain)

      ! function : create overall hyd structure

      ! (c) Deltares

      ! global declarations

      use hydmod
      use system_utils
      implicit none

      ! declaration of the arguments

      character(len=256)        :: waq_output_dir         ! directory of output for WAQ
      type(t_hyd)               :: hyd                    ! description of the hydrodynamics
      integer                   :: n_domain               ! number of domains

      ! local declarations

      character(len=256)        :: name                   ! base name
      character(len=256)        :: name_path              ! base name with path
      character(len=4)          :: sdnm                   ! domain string
      integer                   :: i_domain               ! index in collection
      integer                   :: i_domain1              ! index in collection
      type(t_domain)            :: domain                 ! one domain description
      logical                   :: result                 ! result
      integer                   :: istat                  ! status from makedir

      ! set the names
      name              = hyd%file_hyd%name
      name_path         = trim(waq_output_dir)//'_'//trim(name)//'/'//trim(name)
      istat =  makedir(trim(waq_output_dir)//'_'//trim(name))
      
      hyd%file_com%name = name
      hyd%file_hyd%name = name_path//'.hyd'
      hyd%geometry      = HYD_GEOM_UNSTRUC

      ! get the domains


      hyd%domain_coll%cursize = 0
      hyd%domain_coll%maxsize = 0
      do i_domain = 0, n_domain-1

         write(sdnm, '(i4.4)') i_domain
         domain%name = trim(waq_output_dir)//'_'//trim(name)//'_'//sdnm//'/'//trim(name)//'_'//sdnm//'.hyd'
         domain%mmax = -999
         domain%nmax = -999
         domain%aggr = ' '
         i_domain1 = domain_coll_add(hyd%domain_coll, domain)
      enddo

      ! some prelim initialisation of hyd

      call set_hyd(hyd,name_path)
      hyd%file_dps%name = ' '
      hyd%task = HYD_TASK_FULL

      return
   end