      subroutine read_poi(file_poi, noq   , noq1    , noq2  , noq3  , &
                          ipoint  )

      ! function : read a poi file and check dimensions

      ! (c) Deltares

      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_poi               ! pointer-file
      integer                                :: noq                    ! noq
      integer                                :: noq1                   ! noq1
      integer                                :: noq2                   ! noq2
      integer                                :: noq3                   ! noq3
      integer                                :: ipoint(4,noq)          ! pointer table

      ! local declarations

      integer                                :: i,j,ip1,ip2            ! indxes in pointer table
      integer                                :: ioerr                  ! error on file
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_poi)

      if ( noq1 .gt. 0 ) then
         read(file_poi%unit_nr,iostat=ioerr) ((ipoint(i,j),i=1,4),j=1,noq1)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call srstop(1)
         endif
      endif
      if ( noq2 .gt. 0 ) then
         ip1 = noq1 + 1
         ip2 = noq1 + noq2
         read(file_poi%unit_nr,iostat=ioerr) ((ipoint(i,j),i=1,4),j=ip1,ip2)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call srstop(1)
         endif
      endif
      if ( noq3 .gt. 0 ) then
         ip1 = noq1 + noq2 + 1
         ip2 = noq1 + noq2 + noq3
         read(file_poi%unit_nr,iostat=ioerr) ((ipoint(i,j),i=1,4),j=ip1,ip2)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call srstop(1)
         endif
      endif

      close(file_poi%unit_nr)
      file_poi%status = FILE_STAT_UNOPENED

      return
      end
