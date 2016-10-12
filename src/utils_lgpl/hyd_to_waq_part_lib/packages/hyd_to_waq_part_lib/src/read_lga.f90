      subroutine read_lga(file_lga, mmax  , nmax  , nolay , nosegl, &
                          noq1    , noq2  , noq3  , lgrid )

      ! function : read a lga file and check dimensions

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_lga               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nolay                  ! nolay
      integer                                :: nosegl                 ! nosegl
      integer                                :: noq1                   ! noq1
      integer                                :: noq2                   ! noq2
      integer                                :: noq3                   ! noq3
      integer                                :: lgrid(nmax,mmax)       ! active grid table

      ! local declarations

      integer                                :: mmaxd                  ! grid cells m direction from lga file
      integer                                :: nmaxd                  ! grid cells n direction from lga file
      integer                                :: ioerr                  ! error on file
      integer                                :: m                      ! loop counter
      integer                                :: n                      ! loop counter
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_lga)
      read(file_lga%unit_nr,iostat=ioerr) nmaxd, mmaxd, nosegl, nolay, noq1, noq2, noq3
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call srstop(1)
      endif

      if ( nmaxd.ne.nmax .or. mmaxd.ne.mmax ) then
         write(lunrep,*) ' dimensions lga file differ from input hydrodynamics'
         write(lunrep,*) ' mmax hyd file:',mmax
         write(lunrep,*) ' nmax hyd file:',nmax
         write(lunrep,*) ' mmax lga file:',mmaxd
         write(lunrep,*) ' nmax lga file:',nmaxd
         call srstop(1)
      endif

      read(file_lga%unit_nr,iostat=ioerr) ((lgrid(n,m),n=1,nmax),m=1,mmax)
      if ( ioerr .ne. 0 ) then
         write(lunrep,*) ' error reading lga file'
         call srstop(1)
      endif

      return
      end
