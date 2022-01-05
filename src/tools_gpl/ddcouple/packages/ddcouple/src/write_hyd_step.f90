      subroutine write_hyd_step(hyd, itime)

      use hydmod
      implicit none

      type(t_hyd)         :: hyd           ! description of the hydrodynamics
      integer             :: itime         ! relative time in file

      ! local

      character(len=20)   :: valnam(1)     ! name of value to be written
      integer             :: notim         ! number of output steps

      notim = 2 ! in fact we do not know but not constant so set to 2

      valnam(1) = 'volume'
      call write_data ( hyd%file_vol, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%volume,2)

      valnam(1) = 'area'
      call write_data ( hyd%file_are, itime, notim, hyd%noq1, hyd%noq2, hyd%noq3, 1, 1, 0, valnam, hyd%area,1)

      valnam(1) = 'flow'
      call write_data ( hyd%file_flo, itime, notim, hyd%noq1, hyd%noq2, hyd%noq3, 1, 1, 0, valnam, hyd%flow,1)

      if ( hyd%sal_present ) then
         valnam(1) = 'salinity'
         call write_data ( hyd%file_sal, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%sal,1)
      endif

      if ( hyd%tem_present ) then
         valnam(1) = 'temperature'
         call write_data ( hyd%file_tem, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%tem,1)
      endif

      if ( hyd%tau_present ) then
         valnam(1) = 'tau'
         call write_data ( hyd%file_tau, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%tau,1)
      endif

      if ( hyd%vdf_present ) then
         valnam(1) = 'vdf'
         call write_data ( hyd%file_vdf, itime, notim, hyd%noseg, 0, 0, 1, 1, 0, valnam, hyd%vdf,1)
      endif

      return
      end
