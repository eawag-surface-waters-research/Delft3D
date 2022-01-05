      subroutine write_srf ( file_srf, mmax  , nmax  , nosegl, surf  )
!
!     Deltares
!
!     created             : jan van beek
!
!     function            : writes horizontal surface file.
!
!     subroutines called  : jbputi, puts an integer value to a dos binary file.
!                           jbputa, puts a real array to a dos binary file.
!                         : uxputi, puts an integer value to a unix binary file.
!                         : uxputa, puts an real array to a unix binary file.
!                         : dlwq_platform, return platform type
!
      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of arguments

      type(t_dlwqfile)                       :: file_srf               ! surfaces-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      integer                                :: nosegl                 ! number of segments per layer
      real                                   :: surf(nosegl)           ! surf

      ! local declarations

      integer       lun
      integer       i
      integer       idummy
      integer       irlen
      integer       plform
      integer       filtyp
      integer       filsta

      plform = dlwq_platform()
      idummy = 0

      ! initialise file

      call dlwqfile_open(file_srf)
      lun    = file_srf%unit_nr
      filtyp = file_srf%type

      ! write surfaces file

      if ( filtyp .eq. FT_UNF .and. plform .eq. PL_UNX .or. &
           filtyp .eq. FT_BIN .and. plform .eq. PL_DOS) then
         write (lun) nmax,mmax,nosegl,nosegl,nosegl,idummy
         write (lun) (surf(i),i=1,nosegl)
      elseif ( filtyp .eq. FT_BIN .and. plform .eq. PL_UNX) then
         call jbputi(lun,nmax)
         call jbputi(lun,mmax)
         call jbputi(lun,nosegl)
         call jbputi(lun,nosegl)
         call jbputi(lun,nosegl)
         call jbputi(lun,idummy)
         call jbputa(lun,surf,nosegl)
      elseif ( filtyp .eq. FT_UNF .and. plform .eq. PL_DOS) then
         irlen = 6*4
         call uxputi(lun,irlen)
         call uxputi(lun,nmax)
         call uxputi(lun,mmax)
         call uxputi(lun,nosegl)
         call uxputi(lun,nosegl)
         call uxputi(lun,nosegl)
         call uxputi(lun,idummy)
         call uxputi(lun,irlen)
         irlen = nosegl*4
         call uxputi(lun,irlen)
         call uxputa(lun,surf,nosegl)
         call uxputi(lun,irlen)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') nmax,mmax,nosegl,nosegl,nosegl,idummy
         write (lun,'(e12.6)') (surf(i),i=1,nosegl)
      endif

      close(file_srf%unit_nr)
      file_srf%status = FILE_STAT_UNOPENED

      return
      end
