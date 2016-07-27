module m_read_1d2d_data
   implicit none
   
   private
   
   public read_1d2d_data

   contains
   
   subroutine read_1d2d_data(c_configfile)

      use iso_c_utils
      use properties
      !use tree_data_types
      !use tree_structures
      use messageHandling
      use iterative_coupler_1d2d
      
      character(kind=c_char), intent(in) :: c_configfile(*)

      character(len=strlen(c_configfile)) :: filename
      character(len=charln)               :: log
      character(len=charln)               :: modeltype
      character(len=charln)               :: inputfile
      character(len=charln)               :: directory
      type(tree_data), pointer            :: md_ptr
      integer :: istat
      integer :: juerr
      integer :: numuni
      integer :: numstr
      integer :: i
      integer :: model_index
      logical :: success
      integer :: slash
      integer :: backslash
      integer :: pos
      

       !
       !     Open error log file
       !

      
      ! Convert c string to fortran string and read md1d file into tree
      filename = char_array_to_string(c_configfile)
      call tree_create(trim(filename), md_ptr)
      call prop_inifile(trim(filename), md_ptr, istat)
      if (istat /= 0) then
         call setmessage(LEVEL_FATAL, 'Error opening ' // trim(filename))
      endif
      
      slash = index(filename, '/', back = .true.)
      backslash = index(filename, '\', back = .true.)
      pos = max(slash, backslash)
      directory = trim(filename(1:pos))

      
      ! Create sobek log file and turn on logging to file
      call prop_get_string(md_ptr, 'files', 'logFile', log, success)
      if (.not. success) then
         log = 'flow_1d2d.log'
      endif
      juerr = 101
      open (unit = juerr, file = log, status = 'replace')
      
      call SetMessageHandling(lunMessages = juerr)
      call resetMessageCount_mh()
      call resetMaxerrorLevel()

      ! Read information for DflowFM and Dflow1d models
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'model') then
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'model', 'type', modeltype, success)
            if (trim(modeltype) == 'dflow1d') then 
               model_index = DFLOW1D
            elseif (trim(modeltype) == 'dflowfm') then 
               model_index = DFLOWFM
            endif
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'model', 'name', d_1d2d%model(model_index)%name, success)
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'model', 'directory', d_1d2d%model(model_index)%working_dir, success)
            call prop_get_string(md_ptr%child_nodes(i)%node_ptr, 'model', 'modelDefinitionFile', d_1d2d%model(model_index)%md_file, success)
         endif
      enddo

      ! Read parameter information
      d_1d2d%max_iteration = 3
      call prop_get_integer(md_ptr, 'parameters', 'maximumIterations', d_1d2d%max_iteration, success)
      
      d_1d2d%max_error = 1d-4
      call prop_get_double(md_ptr, 'parameters', 'maximumError', d_1d2d%max_error, success)
      
      ! read mapping data
      call prop_get_string(md_ptr, 'Files', 'mappingFile', inputfile, success)
      inputfile = trim(directory)//trim(inputfile)
      call read_mapping_file(inputfile)
      
      call tree_destroy(md_ptr)
   end subroutine read_1d2d_data
    
   subroutine read_mapping_file(inputfile)
      use properties
      use messageHandling
      use iterative_coupler_1d2d
   
      character(len=*), intent(in)  :: inputfile
      
      type(tree_data), pointer      :: md_ptr
      integer                       :: istat
      integer                       :: numstr
      integer                       :: i
      integer                       :: count
      double precision              :: coor(2)
      
      
      call tree_create(trim(inputfile), md_ptr)
      call prop_inifile(trim(inputfile), md_ptr, istat)
      if (istat /= 0) then
         call setmessage(LEVEL_FATAL, 'Error opening ' // trim(inputfile))
      endif
      
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      count = 0
      do i = 1, numstr
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. '1d2dlink') then
            count = count + 1
         endif
      enddo
      
      d_1d2d%connections_count = count
      allocate(d_1d2d%x_1d(count))
      allocate(d_1d2d%y_1d(count))
      allocate(d_1d2d%x_2d(count))
      allocate(d_1d2d%y_2d(count))

      count = 0
      do i = 1, numstr
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. '1d2dlink') then
            count = count + 1
            call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '1d2dlink', 'xy_1d', coor, 2)
            d_1d2d%x_1d(count) = coor(1)
            d_1d2d%y_1d(count) = coor(2)
            call prop_get_doubles(md_ptr%child_nodes(i)%node_ptr, '1d2dlink', 'xy_2d', coor, 2)
            d_1d2d%x_2d(count) = coor(1)
            d_1d2d%y_2d(count) = coor(2)
         endif
      enddo

   end subroutine read_mapping_file
   
end module m_read_1d2d_data
