module flow1d2d_bmi
   use iso_c_binding
   use iso_c_utils

   implicit none
   
!
   
!  ! This is assumed.....
  integer(c_int), parameter :: MAXDIMS = 6
   type(C_PTR) :: x_temp
!
!  double precision, target :: t
!  double precision, target :: t_end
!  double precision, target :: t_start
!
!  double precision, target :: arr1(3)
!  integer, target :: arr2(2,3)
!  logical(c_bool), target :: arr3(2,2,3)
   private

   contains
 
   subroutine disable_exceptions() bind(C, name="disable_exceptions")
      !DEC$ ATTRIBUTES DLLEXPORT::disable_exceptions
   end subroutine disable_exceptions
   
   integer(c_int) function finalize() result(ierr) bind(C, name="finalize")
      !DEC$ ATTRIBUTES DLLEXPORT::finalize

      ierr = 0
      
   end function finalize
 
   integer(c_int) function initialize(c_configfile) result(ierr) bind(C, name="initialize")
      !DEC$ ATTRIBUTES DLLEXPORT::initialize

      implicit none
      
      ! Variables
      character(kind=c_char), intent(in) :: c_configfile(*)

      ierr = 0
   end function initialize
  
   !> Performs a single timestep with the current model.
   integer(c_int) function update(dt) result(ierr) bind(C,name="update")
      !DEC$ ATTRIBUTES DLLEXPORT::update
      !use m_GlobalParameters
 
      !< Custom timestep size, use -1 to use model default.
      real(c_double), value, intent(in) :: dt
      integer :: i, nstep
      !if (dt<= 0.00) then
      !   nstep = 1
      !else
      !   nstep = nint(dt/modelTimeStepData%timeStep)
      !endif   
      ierr = 0 
      !do i = 1, nstep
         !if ( ModelPerformTimeStep() ) then
         !else
         !   ierr = -1
         !   return
         !endif
      !enddo
   end function update
 
   subroutine get_var_type(c_var_name, c_type_name)  bind(C, name="get_var_type")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_type
 
      character(kind=c_char), intent(in) :: c_var_name(*)
      character(kind=c_char), intent(out) :: c_type_name(MAXSTRINGLEN)
      
      character(len=strlen(c_var_name)) :: var_name
      character(len=MAXSTRINGLEN) :: type_name
      
      var_name = char_array_to_string(c_var_name)
      
      select case(var_name)
      case('s1', 'qzeta_2d1d', 'qlat_2d1d', 'width_1d', 'qtotal_2d1d')
         type_name = 'double'
      case default
         type_name = 'double'
      end select
      
      c_type_name = string_to_char_array(trim(type_name))
 
   end subroutine get_var_type
 
   subroutine get_var_rank(c_var_name, rank) bind(C, name="get_var_rank")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var_rank
     
      character(kind=c_char), intent(in) :: c_var_name(*)
      integer(c_int), intent(out) :: rank
     
      ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name)
     
      select case(var_name)
      case('s1', 'qzeta_2d1d', 'qlat_2d1d', 'width_1d', 'qtotal_2d1d')
         rank = 1
      case default
         rank = 1
      end select
   end subroutine get_var_rank
 
   subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
     !DEC$ ATTRIBUTES DLLEXPORT :: get_var_shape
   
     !use cpluv
     !use M_calcpoints
 
     character(kind=c_char), intent(in) :: c_var_name(*)
     integer(c_int), intent(inout) :: shape(MAXDIMS)
 
     character(len=strlen(c_var_name)) :: var_name
 
     var_name = char_array_to_string(c_var_name)
     shape = (/0, 0, 0, 0, 0, 0/)
 
     select case(var_name)
     case('s1', 'qzeta_2d1d', 'qlat_2d1d', 'width_1d', 'qtotal_2d1d')
        shape(1:1) = 1
        case default
        shape(1:1) = 1
     end select
   end subroutine get_var_shape
 
   subroutine get_var(c_var_name, x) bind(C, name="get_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_var
      
      ! Return a pointer to the variable
      
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), intent(inout) :: x
      
      character(len=strlen(c_var_name)) :: var_name
      integer :: i
      double precision, dimension(1) :: value
      ! Store the name
      
      var_name = char_array_to_string(c_var_name)
      !
      !select case(var_name)
      !case('s1')
      !   do i = 1, ngrid
      !      pars_1d2d%s1(i) = s1(grds2p(i))
      !   enddo
      !   x = c_loc(pars_1d2d%s1)
      !case('width_1d')
      !   do i = 1, ngrid
      !      pars_1d2d%width_1d(i) = dads(grds2p(i))
      !   enddo
      !   do i = 1, ngrid
      !      if (pars_1d2d%width_1d(i) == 0d0 .and. i > 1) then
      !         pars_1d2d%width_1d(i) = pars_1d2d%width_1d(i-1)
      !      endif
      !   enddo
      !   x = c_loc(pars_1d2d%width_1d)
      !case('qzeta_2d1d')
      !   do i = 1, ngrid
      !      pars_1d2d%width_1d(i) = pars_1d2d%qzeta(grds2p(i))
      !   enddo
      !   x = c_loc(pars_1d2d%width_1d)
      !case('qlat_2d1d')
      !   do i = 1, ngrid
      !      pars_1d2d%width_1d(i) = pars_1d2d%qlat(grds2p(i))
      !   enddo
      !   x = c_loc(pars_1d2d%width_1d)
      !case('qtotal_2d1d')
      !   do i = 1, ngrid
      !      pars_1d2d%width_1d(i) = pars_1d2d%qlat_comp(grds2p(i))
      !   enddo
      !   x = c_loc(pars_1d2d%width_1d)
      !case default
      !   call get_external_value(var_name, value(1))
      !   x = c_loc(value)
      !end select
      !x_temp = x
   end subroutine get_var
 
   subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_var
      ! Return a pointer to the variable
      use iso_c_binding, only: c_double, c_char, c_loc, c_f_pointer
     
      character(kind=c_char), intent(in) :: c_var_name(*)
      type(c_ptr), value, intent(in) :: xptr
      real(c_double), pointer :: x_1d_double_ptr(:)
      double precision, dimension(1) :: external_value
      integer loc_type, loc_index, var_type
      integer index
 
       ! The fortran name of the attribute name
      character(len=strlen(c_var_name)) :: var_name
      ! Store the name
      var_name = char_array_to_string(c_var_name)
      !
      
       !select case(var_name)
       !case('qzeta_2d1d')
       !   call c_f_pointer(xptr, x_1d_double_ptr, shape(pars_1d2d%s1))
       !   call sbk2pluv(x_1d_double_ptr, pars_1d2d%qzeta)
       !   pars_1d2d%isused = .true.
       !case('qlat_2d1d')
       !   call c_f_pointer(xptr, x_1d_double_ptr, shape(pars_1d2d%s1))
       !   call sbk2pluv(x_1d_double_ptr, pars_1d2d%qlat)
       !   pars_1d2d%isused = .true.
       !case default
       !   call c_f_pointer(xptr, x_1d_double_ptr, (/ 1 /))
       !   external_value = x_1d_double_ptr(1)
       !   call set_external_value(var_name, external_value(1), pluv_time + dt_sobek, loc_type, & 
       !                          var_type, loc_index)
       !   select case(loc_type)
       !   case (CFiBoundaries) 
       !      index = ec_bnd_2_ec_index(var_type, loc_index)
       !      ec_item_has_been_set_externally(index) = .true.
       !   case (CFiLaterals) 
       !      index = ec_lat_2_ec_index(var_type, loc_index)
       !      ec_item_has_been_set_externally(index) = .true.
       !   end select
       !end select
       !
   end subroutine set_var
   
   subroutine get_current_time(time) bind(C, name="get_current_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
      
      real(c_double)   :: time
      !double precision :: fractionOfMJD, currentTimeAsMJD
      !
      !fractionOfMJD = pluv_time/86400.d+0
      !currentTimeAsMJD = startTimeAsMJD + fractionOfMJD
      !time = pluv_time ! currentTimeAsMJD
 
   end subroutine get_current_time
 
   subroutine get_start_time(time) bind(C, name="get_start_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
 
      real(c_double) :: time
      !time = 0.0D+0 ! startTimeAsMJD
 
   end subroutine get_start_time
 
   subroutine get_end_time(time) bind(C, name="get_end_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
      !use m_times
     
 
      real(c_double) :: time
      !time = dt_sobek * nstep ! startTimeAsMJD + (dt_sobek * nstep) / 86400.0d+0
 
   end subroutine get_end_time

     ! Variables
  subroutine get_var_count(c_var_count) bind(C, name="get_var_count") ! non-BMI
    !DEC$ ATTRIBUTES DLLEXPORT :: get_var_count
    
    use iso_c_binding, only: c_ptr, c_int

    integer(c_int), intent(out)         :: c_var_count
    
    c_var_count = 0 
  end subroutine get_var_count

character(len=idlen) function split(string)
   use messagehandling
   character(len=*) string
   
   integer islash
   
   islash = index(string, '/')
   split = string(1:islash-1)
   string = string(islash+1:)
end function split


end module flow1d2d_bmi
