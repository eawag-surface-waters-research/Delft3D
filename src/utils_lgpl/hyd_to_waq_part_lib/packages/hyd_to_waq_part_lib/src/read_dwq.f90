      subroutine read_dwq(file_dwq, mmax  , nmax, ipnt )

      ! function : read a dwq file and check dimensions

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_dwq               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: ipnt(nmax,mmax)        ! aggregation pointer

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from dwq file
      integer                                :: nmaxd                  ! grid cells n direction from dwq file
      integer                                :: nmd                    ! total number of grid cells from dwq file
      integer                                :: ioptdd                 ! dido option ?
      integer                                :: idum                   ! dummy
      integer                                :: n,m                    ! loop counters
      integer                                :: ioerr                  ! error on file

      call dlwqfile_open(file_dwq)
      read(file_dwq%unit_nr,*,iostat=ioerr) nmaxd, mmaxd, nmd, ioptdd, idum
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error reading dwq file'
         call srstop(1)
      endif

      if (nmaxd.ne.nmax.or.mmaxd.ne.mmax.or.nmd.ne.nmax*mmax) then
         write(*,*) ' dimensions grid on dido file differ from input hydrodynamics'
         call srstop(1)
      endif

      read(file_dwq%unit_nr,*,iostat=ioerr) ((ipnt(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(*,*) ' error reading dwq file'
         call srstop(1)
      endif

      return
      end
