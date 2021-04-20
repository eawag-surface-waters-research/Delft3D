!> make directory (also for linux)
 subroutine makedir(dirname)
   use string_module, only: get_dirsep
#ifdef __INTEL_COMPILER
   use ifport
#endif
    implicit none
    character(len=*), intent(in) :: dirname

    character(len=256)           :: command
    integer                      :: istat
    logical                      :: l_exist
    integer                      :: i
    character(len=256)           :: dirnamewin

!    write(6,"('Creating directory ', A128)") trim(dirname)

#ifdef __INTEL_COMPILER
    inquire(directory = trim(dirname), exist = l_exist)
#else
    ! GNU
    inquire(file = trim(dirname)//get_dirsep()//".", exist = l_exist)
#endif
    if (l_exist) then
       return
    end if

    if ( get_dirsep().eq.'/' ) then
!     linux
      command = "mkdir -p "//trim(dirname)
    else
!     windows
       dirnamewin = trim(dirname)
       do i = 1,len(dirnamewin)
          if( dirnamewin(i:i) == '/' ) dirnamewin(i:i) = '\'
       enddo
       command = "mkdir "//trim(dirnamewin)
       ! call iosDirMAKE(dirname)
    end if

    istat = system(command)
    ! Fortran2008, not available before Intel 15:
    ! call execute_command_line(command)

    return
 end subroutine
