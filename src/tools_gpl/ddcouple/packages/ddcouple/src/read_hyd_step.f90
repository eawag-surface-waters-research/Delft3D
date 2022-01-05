      subroutine read_hyd_step(hyd, itime, iend)

      ! global declarations

      use hydmod
      implicit none

      ! decalration of arguments

      type(t_hyd)          :: hyd           ! description of the hydrodynamics
      integer              :: itime         ! relative time in file
      integer              :: iend          ! end of file indicator

      ! local decalrations

      integer              :: ierr          ! error indicator
      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      ! for volume check on end of file

      if (hyd%file_vol%status .eq. 0) then
         write(*,*)'Reading volumes file: ', trim(hyd%file_vol%name)
         write(lunrep,*)'Reading volumes file: ', trim(hyd%file_vol%name)
         call dlwqfile_open(hyd%file_vol)
      endif
      read(hyd%file_vol%unit_nr,iostat=iend) itime,(hyd%volume(i),i=1,hyd%noseg)
      if ( iend .ne. 0 ) return

      ! for the rest read

      if (hyd%file_are%status .eq. 0) then
         write(*,*)'Reading exchange areas file: ', trim(hyd%file_are%name)
         write(lunrep,*)'Reading exchange areas file: ', trim(hyd%file_are%name)
         call dlwqfile_open(hyd%file_are)
      endif
      read(hyd%file_are%unit_nr,iostat=ierr) itime,(hyd%area(i),i=1,hyd%noq)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) 'ERROR: reading are file: ',trim(hyd%file_are%name)
         call srstop(1)
      endif

      if (hyd%file_flo%status .eq. 0) then
         write(*,*)'Reading discharges file: ', trim(hyd%file_flo%name)
         write(lunrep,*)'Reading discharges file: ', trim(hyd%file_flo%name)
         call dlwqfile_open(hyd%file_flo)
      endif
      read(hyd%file_flo%unit_nr,iostat=ierr) itime,(hyd%flow(i),i=1,hyd%noq)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) 'ERROR: reading flo file: ',trim(hyd%file_flo%name)
         call srstop(1)
      endif

      if ( hyd%sal_present ) then
         if (hyd%file_sal%status .eq. 0) then
            write(*,*)'Reading salinity file: ', trim(hyd%file_sal%name)
            write(lunrep,*)'Reading salinity file: ', trim(hyd%file_sal%name)
            call dlwqfile_open(hyd%file_sal)
         endif
         read(hyd%file_sal%unit_nr,iostat=ierr) itime,(hyd%sal(i),i=1,hyd%noseg)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) 'ERROR: reading sal file: ',trim(hyd%file_sal%name)
            call srstop(1)
         endif
      endif

      if ( hyd%tem_present ) then
         if (hyd%file_tem%status .eq. 0) then
            write(*,*)'Reading temperature file: ', trim(hyd%file_tem%name)
            write(lunrep,*)'Reading temperature file: ', trim(hyd%file_tem%name)
            call dlwqfile_open(hyd%file_tem)
         endif
         read(hyd%file_tem%unit_nr,iostat=ierr) itime,(hyd%tem(i),i=1,hyd%noseg)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) 'ERROR: reading tem file: ',trim(hyd%file_tem%name)
            call srstop(1)
         endif
      endif

      if ( hyd%tau_present ) then
         if (hyd%file_tau%status .eq. 0) then
            write(*,*)'Reading bottom shear stress file: ', trim(hyd%file_tau%name)
            write(lunrep,*)'Reading bottom shear stress file: ', trim(hyd%file_tau%name)
            call dlwqfile_open(hyd%file_tau)
         endif
         read(hyd%file_tau%unit_nr,iostat=ierr) itime,(hyd%tau(i),i=1,hyd%noseg)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) 'ERROR: reading tau file: ',trim(hyd%file_tau%name)
            call srstop(1)
         endif
      endif

      if ( hyd%vdf_present ) then
         if (hyd%file_vdf%status .eq. 0) then
            write(*,*)'Reading vertical dispersion file: ', trim(hyd%file_vdf%name)
            write(lunrep,*)'Reading vertical dispersion file: ', trim(hyd%file_vdf%name)
            call dlwqfile_open(hyd%file_vdf)
         endif
         read(hyd%file_vdf%unit_nr,iostat=ierr) itime,(hyd%vdf(i),i=1,hyd%noseg)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) 'ERROR: reading vdf file: ',trim(hyd%file_vdf%name)
            call srstop(1)
         endif
      endif

      return
      end
