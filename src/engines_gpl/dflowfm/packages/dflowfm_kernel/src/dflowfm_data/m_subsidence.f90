   module m_subsidence
      logical                                              :: sdu_first     !< Flag indicating whether this is the first call to obtain the 'bedrock_surface_elevation'
      integer                                              :: sdu_update_s1 !< Flag indicating whether water levels at wet point should be updated (0 = no, 1 = yes)
      integer                                              :: jasubsupl     !< Flag indicating whether subsidence and uplift is included in the simulation (0 = no, 1 = yes)
      double precision, dimension(:), allocatable, target  :: subsupl       !< Latest field of 'bedrock_surface_elevation' interpolated onto the mesh at the location at which the initial bed levels are prescribed
      double precision, dimension(:), allocatable          :: subsupl_t0    !< Initial field of 'bedrock_surface_elevation'
      double precision, dimension(:), allocatable          :: subsupl_tp    !< Previous field of 'bedrock_surface_elevation'
      double precision, dimension(:), allocatable          :: subsout       !< Output field of subsidence/uplift: latest field - initial field
      double precision, dimension(:), allocatable          :: sdu_blp       !< Previous field of bed level values at cell centres (temporary copy of bl(:))

   contains

      subroutine default_subsupl()
         jasubsupl     = 0
         sdu_first     = .true.
         sdu_update_s1 = 0
      end subroutine

   end module m_subsidence
