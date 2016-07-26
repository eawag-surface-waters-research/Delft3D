module m_dflow1d2dError

   public dflow1d2dError

   contains

   subroutine dflow1d2dError(errorLevel, message)
      use MessageHandling
      implicit none
      integer, intent(in):: errorLevel
      character(len=*), intent(in) :: message
      if (errorLevel >= LEVEL_FATAL) then
         call THROWEXCEPTION()
      endif
      
   end subroutine dflow1d2dError
end module m_dflow1d2dError
   
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
   
   integer(c_int) function initialize(c_configfile) result(ierr) bind(C, name="initialize")
      !DEC$ ATTRIBUTES DLLEXPORT::initialize
      use messageHandling
      use iterative_coupler_1d2d
      use m_read_1d2d_data
      use m_dflow1d2dError
      
      implicit none
      
      ! Variables
      character(kind=c_char), intent(in) :: c_configfile(*)

      call SetMessageHandling(write2screen = .true., &
                        useLog = .true., &
                        callback = dflow1d2dError, &
                        reset_counters = .true., &
                        thresholdLevel = LEVEL_INFO &
                        )

      
      call read_1d2d_data(c_configfile)
      
      call init_iterative_coupler()
      ierr = 0
   end function initialize

   !> Performs a single timestep with the current model.
   subroutine update(dt) bind(C,name="update")
      !DEC$ ATTRIBUTES DLLEXPORT::update
      use iterative_coupler_1d2d
      use dummy_api
 
      !< Custom timestep size, use -1 to use model default.
      real(c_double), value, intent(in) :: dt
      
      double precision :: timestep
      integer :: nstep
      integer :: ierr
      integer :: i
      
      if (dt<= 0.00) then
         nstep = 1
      else
         call Flow1DModel_get_time_step(timestep)
         nstep = nint(dt/timeStep)
      endif   
      ierr = 0 
      do i = 1, nstep
         call compute1d2d_user_timestep()
      enddo
   end subroutine update

   subroutine finalize() bind(C, name="finalize")
      !DEC$ ATTRIBUTES DLLEXPORT::finalize
      use iterative_coupler_1d2d
      
      implicit none
      
      call finalize_iterative_coupler()

   end subroutine finalize
 
   subroutine get_start_time(time) bind(C, name="get_start_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
 
      real(c_double), intent(out) :: time
      time = 0.0D+0 ! startTimeAsMJD
 
   end subroutine get_start_time

   subroutine get_end_time(time) bind(C, name="get_end_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
       
      real(c_double), intent(out) :: time
      time = 0.0D+0 ! startTimeAsMJD
 
   end subroutine get_end_time

   subroutine get_time_step(time) bind(C, name="get_time_step")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step

      real(c_double), intent(out) :: time
      time = 0.0D+0 ! startTimeAsMJD
   end subroutine get_time_step
   
   subroutine get_current_time(time) bind(C, name="get_current_time")
      !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
      
      real(c_double), intent(out) :: time
      time = 0.0D+0 ! startTimeAsMJD
 
   end subroutine get_current_time

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
   end subroutine get_var
 

   subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
      !DEC$ ATTRIBUTES DLLEXPORT :: set_var

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
      
   end subroutine set_var

end module flow1d2d_bmi
   