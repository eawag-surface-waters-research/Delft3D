module m_debug
   !
   ! Module with arrays to write debug quantities to nc map files
   !
   public
      integer                                         :: jawritedebug
      double precision, allocatable, dimension(:)     :: debugarr1d
      double precision, allocatable, dimension(:,:)   :: debugarr2d
      double precision, allocatable, dimension(:,:,:) :: debugarr3d

   contains

      subroutine init_debugarr(dim1, dim2, dim3)
         use m_alloc
         use m_missing

         implicit none

         integer, intent(in)               :: dim1
         integer, intent(in), optional     :: dim2
         integer, intent(in), optional     :: dim3

         integer                           :: ierr
         integer                           :: dim2_
         integer                           :: dim3_

         dim2_ = 0
         dim3_ = 0

         if (present(dim2)) then
            dim2_ = dim2
         endif

         if (present(dim3)) then
            dim3_ = dim3
         endif

         if (allocated(debugarr1d)) then
            deallocate(debugarr1d)
         endif

         if (allocated(debugarr2d)) then
            deallocate(debugarr2d)
         endif

         if (allocated(debugarr3d)) then
            deallocate(debugarr3d)
         endif

         call realloc(debugarr1d,dim1, keepExisting=.false., fill = dmiss)
         if (dim2_>0) then
            call realloc(debugarr2d,(/dim1, dim2_/), keepExisting=.false., fill = dmiss)
         endif
         if (dim2_>0 .and. dim3_>0) then
            call realloc(debugarr3d,(/dim1, dim2_, dim3_/), keepExisting=.false., fill = dmiss)
         endif
      end subroutine init_debugarr


end module m_debug
