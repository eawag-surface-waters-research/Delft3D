      subroutine write_cco(file_cco, mmax  , nmax  , xdepth, ydepth, nolay)

      ! function : write a cco file

      ! (c) Deltares

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_cco               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      real                                   :: xdepth(nmax,mmax)      ! x coordinate depth points
      real                                   :: ydepth(nmax,mmax)      ! y coordinate depth points
      integer                                :: nolay                  ! nolay

      ! local declarations

      real                                   :: x0                     ! x coordinate origin
      real                                   :: y0                     ! y coordinate origin
      real                                   :: alpha                  ! alpha
      integer                                :: npart                  ! npart
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      real                                   :: rdum                   ! dummy

      x0    = 0.0
      y0    = 0.0
      alpha = 0.0
      npart = 0
      rdum  = 0.0

      call dlwqfile_open(file_cco)
      write(file_cco%unit_nr,iostat=ioerr) mmax, nmax, x0, y0, alpha, npart, nolay
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error writing cco file header record'
         call srstop(1)
      endif

      do i=1 , 2*npart+9
         write(file_cco%unit_nr,iostat=ioerr) rdum
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file dummy records'
            call srstop(1)
         endif
      enddo

      write(file_cco%unit_nr,iostat=ioerr) xdepth
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error writing cco file xdepth'
         call srstop(1)
      endif
      write(file_cco%unit_nr,iostat=ioerr) ydepth
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error writing cco file ydepth'
         call srstop(1)
      endif

      close(file_cco%unit_nr)
      file_cco%status = FILE_STAT_UNOPENED

      return
      end
