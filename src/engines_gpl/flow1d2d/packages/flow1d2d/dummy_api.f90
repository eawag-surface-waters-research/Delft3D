module dummy_api
   public
   
   contains
   
      subroutine Flow1DModel_get_current_time(time)
         double precision time
      end subroutine Flow1DModel_get_current_time
      
      subroutine Flow1DModel_get_time_step(time)
         double precision time
      end subroutine Flow1DModel_get_time_step
      
      subroutine Flow1DModel_InitializeUserTimeStep()
      end subroutine Flow1DModel_InitializeUserTimeStep
      
      subroutine Flow2DModel_InitializeUserTimeStep()
      end subroutine Flow2DModel_InitializeUserTimeStep
            
      logical function Flow2DModel_InitializeComputationalTimeStep(dt)
         double precision dt
         Flow2DModel_InitializeComputationalTimeStep = .true.
      end function Flow2DModel_InitializeComputationalTimeStep
      
      logical function Flow1DModel_InitializeComputationalTimeStep(dt)
         double precision dt
         Flow1DModel_InitializeComputationalTimeStep = .true.
      end function Flow1DModel_InitializeComputationalTimeStep

      subroutine Flow2DModel_get_var(string, q2d)
         character(len=*) :: string
         double precision, dimension(:) :: q2d
      end subroutine Flow2DModel_get_var
      
      subroutine Flow2DModel_set_var(string, q2d)
         character(len=*) :: string
         double precision, dimension(:) :: q2d
      end subroutine Flow2DModel_set_var
      
      subroutine Flow1DModel_get_var(string, q2d)
         character(len=*) :: string
         double precision, dimension(:) :: q2d
      end subroutine Flow1DModel_get_var
      
      subroutine Flow1DModel_set_var(string, q2d)
         character(len=*) :: string
         double precision, dimension(:) :: q2d
      end subroutine Flow1DModel_set_var

      logical function Flow2DModel_RunComputationalTimeStep(dt)
         double precision dt
         Flow2DModel_RunComputationalTimeStep = .true.
      end function Flow2DModel_RunComputationalTimeStep
      
      logical function Flow1DModel_RunComputationalTimeStep(dt)
         double precision dt
         Flow1DModel_RunComputationalTimeStep = .true.
      end function Flow1DModel_RunComputationalTimeStep
         
      logical function Flow1DModel_FinalizeComputationalTimeStep()
         Flow1DModel_FinalizeComputationalTimeStep = .true.
      end function Flow1DModel_FinalizeComputationalTimeStep
      
      logical function Flow2DModel_FinalizeComputationalTimeStep()
         Flow2DModel_FinalizeComputationalTimeStep = .true.
      end function Flow2DModel_FinalizeComputationalTimeStep
      
      subroutine Flow1DModel_FinalizeUserTimeStep()
      end subroutine Flow1DModel_FinalizeUserTimeStep
      
      subroutine Flow2DModel_FinalizeUserTimeStep()
      end subroutine Flow2DModel_FinalizeUserTimeStep

end module dummy_api