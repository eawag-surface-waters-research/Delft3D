      subroutine read_cco(file_cco, mmax  , nmax  , xdepth, ydepth)

      ! function : read a cco file and check dimensions

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

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from cco file
      integer                                :: nmaxd                  ! grid cells n direction from cco file
      real                                   :: x0                     ! x coordinate origin
      real                                   :: y0                     ! y coordinate origin
      real                                   :: alpha                  ! alpha
      integer                                :: npart                  ! npart
      integer                                :: nolay                  ! nolay
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      integer                                :: m                      ! loop counter
      integer                                :: n                      ! loop counter
      real                                   :: rdum                   ! dummy
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_cco)
      read(file_cco%unit_nr,iostat=ioerr) mmaxd, nmaxd, x0, y0, alpha, npart, nolay
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading cco file header record'
         call srstop(1)
      endif

      if ( nmaxd.ne.nmax .or. mmaxd.ne.mmax ) then
         write(lunrep,*) ' dimensions cco file differ from input hydrodynamics'
         call srstop(1)
      endif

      do i=1 , 2*npart+9
         read(file_cco%unit_nr,iostat=ioerr) rdum
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading cco file dummy records'
            call srstop(1)
         endif
      enddo

      read(file_cco%unit_nr,iostat=ioerr) ((xdepth(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading cco file xdepth'
         call srstop(1)
      endif
      read(file_cco%unit_nr,iostat=ioerr) ((ydepth(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading cco file ydepth'
         call srstop(1)
      endif

      close(file_cco%unit_nr)
      file_cco%status = FILE_STAT_UNOPENED

      return
      end
