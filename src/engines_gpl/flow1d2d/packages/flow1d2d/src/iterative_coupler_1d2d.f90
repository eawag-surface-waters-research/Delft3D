module iterative_coupler_1d2d

   implicit none
   private

   public compute1d2d_user_timestep
   public init_iterative_coupler
   public finalize_iterative_coupler
   
   type t_1d2d_data
      integer                             :: iteration
      integer                             :: max_iteration
      double precision                    :: max_error
      integer                             :: connections_count
      integer, allocatable, dimension(:)  :: index2d
      integer, allocatable, dimension(:)  :: index1d
      double precision, allocatable, dimension(:)  :: q2d
      double precision, allocatable, dimension(:)  :: s1sobek
      double precision, allocatable, dimension(:)  :: width1d
      double precision, allocatable, dimension(:)  :: q1d
      double precision, allocatable, dimension(:)  :: qzetafm
      double precision, allocatable, dimension(:)  :: qlatfm
      double precision, allocatable, dimension(:)  :: qzetasbk
      double precision, allocatable, dimension(:)  :: qlatsbk
      double precision, allocatable, dimension(:)  :: qsobek
   end type t_1d2d_data
   
   type(t_1d2d_data), target :: d_1d2d
   contains
   
   subroutine init_iterative_coupler()
   end subroutine init_iterative_coupler

   subroutine finalize_iterative_coupler()
   end subroutine finalize_iterative_coupler

   subroutine compute1d2d_user_timestep()
      use dummy_api
      use messageHandling
      
      implicit none
   
      ! local variables
      double precision  :: dt_flow1D
      double precision  :: dt_flow2D
      double precision  :: current_time 
      double precision  :: new_time 
      double precision  :: user_time_step
      double precision  :: dt
      logical           :: success
      integer           :: iteration
      double precision, pointer :: q2d(:)
      double precision, pointer :: s1sobek(:)
      
      q2d => d_1d2d%q2d
      s1sobek => d_1d2d%s1sobek
      
      call Flow1DModel_get_current_time(current_time)
      call Flow1DModel_get_time_step(user_time_step)
      new_time = current_time + user_time_step

      call Flow1DModel_InitializeUserTimeStep()
      call Flow2DModel_InitializeUserTimeStep()
            
      ! loop over computational time steps
      do while (new_time - current_time > 1d-5)
         success = Flow1DModel_InitializeComputationalTimeStep(dt_flow1D)
         if (.not. success) then
            call setmessage(LEVEL_FATAL, 'Initialization of 1d computational timestep failed')
         endif
         
         success = Flow2DModel_InitializeComputationalTimeStep(dt_flow2D)
         if (.not. success) then
            call setmessage(LEVEL_FATAL, 'Initialization of 2d computational timestep failed')
         endif
      
         iteration = 0
            
         call Flow2DModel_get_var('qtotal_1d2d', q2d)

         ! iteration loop, might fail in case time step is too small
         success = .false.
         do while (.not. success)
            dt = min(dt_Flow1D, dt_Flow2D)

            ! Make sure the last step fits exactly:
            if (current_time + 1.1*dt > new_time) then
               dt = new_time - current_time
            endif

            dt_flow1D = dt
            dt_flow2D = dt

            call MapDataFrom1Dto2D()
            success = Flow2DModel_RunComputationalTimeStep(dt_flow2D)
            call Flow2DModel_get_var('total_1d2d', q2d)
            if (.not. success) then
               cycle
            endif
            
            call MapDataFrom2Dto1D()
            success = Flow1DModel_RunComputationalTimeStep(dt_flow1D)
            call get_from_sobek('s1', s1Sobek)
            call CalcQtotal_2d1d()

            if (.not. success) then
               cycle
            endif
            
            success = CheckConvergence()
         enddo
         
         ! ! mass conservation:
         ! MapDataFrom2Dto1DWithMassConvervation()
         ! successful = Flow1DModel.RunComputationalTimeStep(ref dt_flow1D)
         ! qSobek = GetFromSobek('qtotal_2d1d')

         success = success .and. Flow1DModel_FinalizeComputationalTimeStep()
         success = success .and. Flow2DModel_FinalizeComputationalTimeStep()

         if (.not. success) then
            write(msgbuf, '(''1D-2D coupled model run failed Current time: '', g10.3, '', Computational timestep 1D: '', f8.3, ''s, Computational timestep 2D: '', f8.3, ''s.'')') &
                        current_time, dt_flow1d, dt_flow2d
            call setmessage(LEVEL_FATAL, msgbuf)
         endif

         current_time = current_time + dt_flow1D

      enddo
      
      call Flow1DModel_FinalizeUserTimeStep()
      call Flow2DModel_FinalizeUserTimeStep()

   end
   
   logical function CheckConvergence()
      use messageHandling
      
      double precision :: highest_error
      double precision, pointer :: q2d(:)
      double precision, pointer :: q1d(:)
      double precision          :: maxdiff, qfm, qsobek, maxq
      integer :: i
      
      highest_error = 0d0
      
      q2d => d_1d2d%q2d
      q1d => d_1d2d%q1d

      do i = 1, d_1d2d%connections_count
         Qfm = q2d(i)
         QSobek = q1d(i)
         maxQ = max(abs(QSobek), abs(Qfm), 1.0)
         maxdiff = abs(QSobek + Qfm)/maxQ 
         if (highest_error < maxdiff) then
            highest_error = maxdiff
         endif
      enddo
      if (d_1d2d%iteration > d_1d2d%max_iteration) then
         CheckConvergence = .true.
         return
      endif
      d_1d2d%iteration = d_1d2d%iteration + 1
      if ((highest_error > d_1d2d%max_error) .and. (d_1d2d%iteration < d_1d2d%max_Iteration)) then
         CheckConvergence = .false. 
         return 
      endif
      
      call setmessage(LEVEL_WARN, '1d2d coupling did not converge')
      CheckConvergence = .true. 
   end function CheckConvergence

   subroutine MapDataFrom1Dto2D()
      use dummy_api
   
      call get_from_sobek('s1', d_1d2d%s1sobek)
      call get_from_sobek('width_1d', d_1d2d%width1D)

      call Flow2DModel_set_var('zbnd1d2d1', d_1d2d%s1Sobek)
      call Flow2DModel_set_var('width_1d', d_1d2d%width1D)
   end subroutine MapDataFrom1Dto2D
   

   subroutine get_from_sobek(name, arr)
      use dummy_api
      
      character(len=*) :: name
      double precision, dimension(:), intent(inout) :: arr
      double precision, pointer, dimension(:) :: arr_sobek
      integer, pointer, dimension(:) :: ind
      integer :: i
      
      call Flow1DModel_get_var(name, arr_sobek)
      ind => d_1d2d%index1d
      
      do i = 1, d_1d2d%connections_count
         arr(i) = arr_sobek(ind(i))
      enddo   
      
   end subroutine get_from_sobek

   subroutine MapDataFrom2Dto1D()
      use dummy_api
      
      double precision, pointer :: qzetafm(:)
      double precision, pointer :: qlatfm(:)
      double precision, pointer :: qzetasbk(:)
      double precision, pointer :: qlatsbk(:)
      integer, pointer, dimension(:) :: ind
      integer :: i
      
      qzetafm => d_1d2d%qzetafm
      qlatfm  => d_1d2d%qlatfm
      qzetasbk => d_1d2d%qzetasbk
      qlatsbk  => d_1d2d%qlatsbk
      ind => d_1d2d%index1d
      ! Qtot = Qzeta * s1 + Qlat
      call Flow2DModel_get_var('qzeta_1d2d', qZetaFM)
      call Flow2DModel_get_var('qlat_1d2d', qLatFM)

      qzetasbk = 0d0
      qlatsbk  = 0d0

      do i = 1, d_1d2d%connections_count
         qZetaSbk(ind(i)) = qZetaSbk(ind(i)) + qZetaFM(i)
         qLatSbk (ind(i)) = qLatSbk (ind(i)) + qLatFM(i)
      enddo

      call Flow1DModel_set_var('qzeta_2d1d', qZetaSbk)
      call Flow1DModel_set_var('qlat_2d1d', qLatSbk)
      
   end subroutine MapDataFrom2Dto1D
  
   subroutine CalcQtotal_2d1d()
      double precision, pointer :: qzetafm(:)
      double precision, pointer :: qlatfm(:)
      double precision, pointer :: qzetasbk(:)
      double precision, pointer :: qlatsbk(:)
      double precision, pointer :: qsobek(:)
      double precision, pointer :: s1sobek(:)
      integer, pointer, dimension(:) :: ind
      integer :: i
      
      qzetafm => d_1d2d%qzetafm
      qlatfm  => d_1d2d%qlatfm
      qsobek  => d_1d2d%qsobek
      s1sobek => d_1d2d%s1sobek 
     
      do i = 1, d_1d2d%connections_count
         qSobek(i) = -qZetaFM(i) * s1Sobek(i) - qLatFM(i)
      enddo
      
   end subroutine CalcQtotal_2d1d
   

   end module iterative_coupler_1d2d


!        protected override void OnFinish()
!        {
!            base.OnFinish()
!
!            if (Activities.Any(a => a.Status == ActivityStatus.Failed))
!            {
!                Status = ActivityStatus.Failed
!                return
!            }
!
!            Generate1D2DLinkCoverages()
!
!            if (!LogDebugMessages || HydroModelApplicationPlugin.IterativeCouplerAppender.Messages.Count == 0) return
!
!            AddLogFile(string.Join('\r\n', HydroModelApplicationPlugin.IterativeCouplerAppender.Messages))
!        }
