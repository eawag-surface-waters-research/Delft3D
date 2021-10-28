      subroutine read_waqoutput_dir(hyd, waq_output_dir)

      ! function : read WAQOutputDir from mdu file

      ! (c) Deltares

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamics
      use properties                              
      implicit none

      ! declaration of the arguments     
      type(t_hyd), intent(in)               :: hyd                  ! description of the overall hydrodynamics
      type(tree_data) , pointer             :: mdu_ptr              ! tree for mdu file
      
      ! local declarations   
      character(len=256), intent(out)       :: waq_output_dir       ! WAQ directory
      logical                               :: waq_output_dir_found ! WAQ directory specified
      integer                               :: istat                ! reading parameter


      waq_output_dir = ''

      nullify(mdu_ptr)
      call tree_create('waqmerge-input', mdu_ptr)
      istat = 0
      call prop_file('ini',trim(hyd%file_hyd%name)//'.mdu', mdu_ptr, istat)
      if (istat /= 0) then
        select case (istat)
          case(1)
            write(*     ,'(a,a)'), '*** ERROR File: '//trim(hyd%file_hyd%name)//' not found'
          case(3)
            write(*     ,'(a,a)'), '*** ERROR Premature EOF in file: '//trim(hyd%file_hyd%name)
          case default
            write(*     ,'(a,a)'), '*** ERROR Read error from file: '//trim(hyd%file_hyd%name)
        endselect
      endif
      call prop_get_string(mdu_ptr, 'output', 'WAQOutputDir', waq_output_dir,waq_output_dir_found)
      

      
      if (.not. waq_output_dir_found) then
        waq_output_dir = 'DFM_DELWAQ' 
      end if
   
      return
    end subroutine