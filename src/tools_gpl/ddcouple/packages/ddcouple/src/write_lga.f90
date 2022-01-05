      subroutine write_lga ( file_lga, mmax  , nmax  , nolay , nosegl, &
                             noq1    , noq2  , noq3  , lgrid )
!
!     Deltares
!
!     created             : jan van beek
!
!     function            : writes active grid to file.
!
!     subroutines called  : jbputi, puts an integer value to a dos binary file.
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     filnr   integer  1          input   index file in file administr.
!     mmax    integer  1          input   x,u direction, second in lgrid
!     nmax    integer  1          input   y,v direction, first in lgrid
!     nolay   integer  1          input   number of layers
!     nosegl  integer  1          input   number of delwaq cells per layer aggregated
!     noq1    integer  1          input   number of exchanges 1st direction aggregated
!     noq2    integer  1          input   number of exchanges 2st direction aggregated
!     noq3    integer  1          input   number of exchanges 3d direction aggregated
!     lgrid   integer  nmax,mmax  input   grid table
!
      ! global declarations

      use filmod                   ! module contains everything for the files
      implicit none

!     declaration of arguments

      type(t_dlwqfile)                       :: file_lga               ! aggregation-file
      integer       mmax  , nmax  , nolay , nosegl, &
                    noq1  , noq2  , noq3
      integer       lgrid(nmax,mmax)
!
!     local declarations
!
      integer       lun
      integer       n
      integer       m
      integer       irlen
      integer       plform
      character*256 filnam
      integer       filtyp
      integer       filsta

      plform = dlwq_platform()
!
!     initialise file
!
      call dlwqfile_open(file_lga)
      lun    = file_lga%unit_nr
      filtyp = file_lga%type
!
!     write table
!
      if ( filtyp .eq. FT_UNF .and. plform .eq. PL_UNX .or. &
           filtyp .eq. FT_BIN .and. plform .eq. PL_DOS) then
         write (lun) nmax,mmax,nosegl,nolay,noq1,noq2,noq3
         write (lun) ((lgrid(n,m),n=1,nmax),m=1,mmax)
      elseif ( filtyp .eq. FT_BIN .and. plform .eq. PL_UNX) then
         call jbputi(lun,nmax)
         call jbputi(lun,mmax)
         call jbputi(lun,nosegl)
         call jbputi(lun,nolay)
         call jbputi(lun,noq1)
         call jbputi(lun,noq2)
         call jbputi(lun,noq3)
         do m = 1 , mmax
            do n = 1 , nmax
               call jbputi(lun,lgrid(n,m))
            enddo
         enddo
      elseif ( filtyp .eq. FT_UNF .and. plform .eq. PL_DOS) then
         irlen = 7*4
         call uxputi(lun,irlen)
         call uxputi(lun,nmax)
         call uxputi(lun,mmax)
         call uxputi(lun,nosegl)
         call uxputi(lun,nolay)
         call uxputi(lun,noq1)
         call uxputi(lun,noq2)
         call uxputi(lun,noq3)
         call uxputi(lun,irlen)
         irlen = mmax*nmax*4
         call uxputi(lun,irlen)
         do m = 1 , mmax
            do n = 1 , nmax
               call uxputi(lun,lgrid(n,m))
            enddo
         enddo
         call uxputi(lun,irlen)
      elseif ( filtyp .eq. FT_ASC ) then
         write (lun,'(4i8)') nmax,mmax,nosegl,nolay,noq1,noq2,noq3
         write (lun,'(i7)') ((lgrid(n,m),n=1,nmax),m=1,mmax)
      endif

      close(file_lga%unit_nr)
      file_lga%status = FILE_STAT_UNOPENED

      return
      end
