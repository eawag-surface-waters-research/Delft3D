!> perform actions in batch
subroutine refine_from_commandline()
   use network_data
   use m_partitioninfo
   use unstruc_netcdf, only: unc_write_net
   use m_samples_refine
   implicit none

   integer           :: MPOL

   character(len=10) :: snum
   character(len=128) :: filnam

   !call generate_partitioning_from_pol()
   !write(snum, "(I4.4)") ndomains
   !call partition_write_domains('par'//trim(snum)//'_net.nc')

   filnam = 'out_net.nc'
   CALL REFINECELLSANDFACES2()
   call unc_write_net(trim(filnam))
end subroutine refine_from_commandline
