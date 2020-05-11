module m_forcinglist

   use messagehandling
   implicit none
   private
   
   public realloc
   public dealloc

   interface realloc
      module procedure reallocForcingList
   end interface

   interface dealloc
      module procedure deallocForcingList
   end interface dealloc

  !> Data type to store user input for structure forcings, to be processed later by a kernel.
  !! For example, a pump's capacity may be prescribed by a time series in a .bc file.
  !! The flow1d structure reader only reads all user-supplied input, and later it is up to
  !! the calling kernel to initialize that forcing provider.
  type, public :: t_forcing
     character(IdLen)                 :: object_id      !< The character Id of the object (e.g. a structure id or a timeseries id).
     character(IdLen)                 :: param_name     !< Name of the parameter that this forcing data is for.
     character(IdLen)                 :: quantity_id    !< The name of the quantity id in the *.bc file
     double precision, pointer        :: targetptr => null() !< Pointer to scalar variable in which the provided
                                                        !< parameter value(s) can later be stored.
                                                        !< For example => pump%capacity.
    character(Charln)                :: filename        !< Name of file that contains the forcing data (e.g., a time series file).
  end type

  !> An ordered list of structure forcing items.
  type, public :: t_forcingList
     integer                                :: Size     = 0  !< Current maximum size of the forcing list.
     integer                                :: growsBy  = 20 !< Increment upon each realloc call.
     integer                                :: Count    = 0  !< Current actual number of items in the forcing list.
     type(t_forcing), pointer, dimension(:) :: forcing => null() !< Actual forcing list.
  end type

  contains


   !> Deallocates a forcing list and sets all counters to zero.
   subroutine deallocForcingList(fs)
      ! Modules
  
      implicit none
  
      ! Input/output parameters
      type(t_forcingList), intent(inout) :: fs !< The forcing list.
  
      ! Local variables
  
      ! Program code
      if (associated(fs%forcing)) then
      deallocate(fs%forcing)
      endif
      
      fs%forcing => null()
      fs%size  = 0
      fs%count = 0
  
   end subroutine
   !
   !
   
   subroutine reallocForcingList(fs)
      ! Modules
  
      implicit none
  
      ! Input/output parameters
      type(t_forcingList), intent(inout)          :: fs
  
      ! Local variables
      type(t_forcing), pointer, dimension(:)      :: oldforcing
  
      ! Program code
  
      if (fs%Size > 0) then
      oldforcing=>fs%forcing
      endif
  
      if (fs%growsBy <=0) then
      fs%growsBy = 200
      endif
      allocate(fs%forcing(fs%Size+fs%growsBy))
  
      if (fs%Size > 0) then
      fs%forcing(1:fs%Size) = oldforcing(1:fs%Size)
      deallocate(oldforcing)
      endif
      fs%Size = fs%Size+fs%growsBy
   end subroutine
  
end module m_forcinglist