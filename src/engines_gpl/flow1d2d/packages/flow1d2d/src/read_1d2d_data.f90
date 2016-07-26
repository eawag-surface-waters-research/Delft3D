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
      type(tree_data), pointer            :: md_ptr
      integer :: istat
      integer :: juerr
      integer :: numuni
      integer :: numstr
      integer :: i
      integer :: model_index
      logical :: success
         

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
      
      numstr = 0
      if (associated(md_ptr%child_nodes)) then
         numstr = size(md_ptr%child_nodes)
      end if

      do i = 1, numstr
         if (tree_get_name(md_ptr%child_nodes(i)%node_ptr) .eq. 'model') then
            call prop_get_string(md_ptr, 'model', 'type', modeltype, success)
            if (trim(modeltype) == 'dflow1d') then 
               model_index = DFLOW1D
            elseif (trim(modeltype) == 'dflowfm') then 
               model_index = DFLOWFM
            endif
            call prop_get_string(md_ptr, 'model', 'name', d_1d2d%model(model_index)%name, success)
            call prop_get_string(md_ptr, 'model', 'directory', d_1d2d%model(model_index)%working_dir, success)
            call prop_get_string(md_ptr, 'model', 'modelDefinitionFile', d_1d2d%model(model_index)%md_file, success)
         endif
      enddo
      
      call prop_get_string(md_ptr, 'Files', 'mappingFile', inputfile, success)
      
      
      call tree_destroy(md_ptr)
    end subroutine read_1d2d_data
   
end module m_read_1d2d_data