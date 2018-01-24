module m_oned_funcions
   private
   
   public set_1d_roughnesses
   
   contains
   
   subroutine set_1d_roughnesses
      use m_flow, only: frcu, ifrcutp
      use unstruc_channel_flow
      use m_spatial_data
      use m_branch
      
      integer :: L
      integer :: ibr
      type(t_branch), pointer                 :: pbr
      double precision, dimension(:), pointer :: cpar
      integer,          dimension(:), pointer :: rgh_type
      integer,          dimension(:), pointer :: fun_type
      integer,          dimension(9)          :: rgh_mapping
            
      if (network%brs%Count > 0) then
         ! RGH_TYPE is similar to IFRCUTP, only with different type numbers
         ! Dflow1D also supports water level or discharge dependent roughness parameters (FUN_TYPE )
         rgh_mapping = -1
         rgh_mapping(R_Chezy         ) = 0
         rgh_mapping(R_Manning       ) = 1
         rgh_mapping(R_WhiteColebrook) = 3
         
         rgh_type => network%rgs%rough(1)%rgh_type_pos
         fun_type => network%rgs%rough(1)%fun_type_pos
         cpar     => network%spData%quant(network%rgs%rough(1)%spd_pos_idx)%values
         do ibr = 1, network%brs%Count
            pbr => network%brs%branch(ibr)
            do i = 1, pbr%uPointsCount
               L = pbr%lin(i)
               k = pbr%points(1) -1 + i
               ifrcutp(L) = rgh_mapping(rgh_type(ibr))
               if ( (fun_type(ibr) == R_FunctionConstant) .and. (ifrcutp(L) >=0) ) then
                  frcu(L) = cpar(k)
               else
                  call setmessage(LEVEL_FATAL, '1D roughness type on branch '// trim(pbr%name) //', '//pbr%id//' is not available in D-FlowFM')
                  ifrcutp(L) = 0
                  frcu(L)    = 45d0
               endif
            enddo
         enddo
      endif
      
   end subroutine set_1d_roughnesses
   
end module m_oned_funcions