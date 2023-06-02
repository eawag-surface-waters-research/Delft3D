!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

      subroutine write_data ( afile , itime , notim , noq1  , noq2  , &
                              noq3  , noval , nosca , flagsf, valnam, &
                              ardata, funtyp)

      use filmod
      use m_dhctim

      implicit none
!
!     Deltares
!
!     created             : jan van beek
!
!     function            : writes data to delwaq auxiliary input file.
!
!     subroutines called  : dhctim, conversion of an integer variable in seconds to dd:hh:mm:ss or yy:ddd:hh format.
!                           jbputa, puts a real array to a dos binary file.
!                           jbputi, puts an integer value to a dos binary file.
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     filnr   integer  1          input   index file in file administr.
!     itime   integer  1          input   actual time in seconds
!     notim   integer  1          input   number of times in file (total)
!     noq1    integer  1          input   number of items 1 in file
!     noq2    integer  1          input   number of items 2 in file
!     noq3    integer  1          input   number of items 3 in file
!     noval   integer  1          input   number of values per item
!     nosca   integer  1          input   number of scale factors
!     flagsf  integer  1          input   flag indocating segment function
!     valnam  char*20  *          input   names of functions
!     ardata  real     *          input   data array to be written
!     funtyp  integer  1          input   function type
!
!     declaration of arguments
!
      type(t_dlwqfile) :: afile
      integer               itime , notim , noq1  , noq2  , &
                    noq3  , noval , nosca , flagsf, funtyp
      real          ardata(*)
      character*20  valnam(*)
!
!     local declarations
!
      integer       ioerr , lun   , noloc , nodata, filtyp, &
                    plform, i     , il    , itime2, &
                    irlen
      character*256 filnam

      plform = dlwq_platform()
!
!     initialise file
!
      noloc  = noq1 + noq2 + noq3
      call dlwqfile_open(afile)
      lun    = afile%unit_nr
      filtyp = afile%type
      filnam = afile%name

      if ( afile%status .eq. 1 ) then
         if ( filtyp .eq. FT_ASC ) then

            ! write header

            if ( notim .eq. 1 ) then
               if ( flagsf .eq. 1 ) then
                  do  i = 1 , noloc
                     write ( lun , 2090 ) valnam(i)
                  enddo
               endif
               write ( lun , 2080 )
            else
               if ( flagsf .eq. 1 ) then
                  do i = 1 , noloc
                     write ( lun , 2090 ) valnam(i)
                  enddo
               else
                  write ( lun , 2000 )
               endif
               if ( funtyp .eq. 1 ) then
                  write ( lun , 2100 )
               else
                  write ( lun , 2010 )
               endif
               write ( lun , 2020 ) noloc
               write ( lun , 2030 ) (i,i=1,noloc)
               write ( lun , 2040 ) notim
               write ( lun , 2050 ) (1.0,i=1,nosca)
            endif
         endif
         afile%status = 2
      endif
!
!     write timestep
!
      nodata = noloc*noval
      if ( filtyp .eq. FT_UNF .or. filtyp .eq. FT_BIN) then
         write ( lun ) itime , (ardata(i),i=1,nodata)
      elseif ( filtyp .eq. FT_ASC ) then
         if ( notim .eq. 1 ) then
            if ( noq1 .gt. 0 ) then
               write ( lun , 2050 ) (1.0,i=1,nosca)
               do 20 il = 1 , noq1
                  write ( lun , 2070 ) (ardata(i+(il-1)*noval), i=1,noval)
   20          continue
            endif
            if ( noq2 .gt. 0 ) then
               write ( lun , 2050 ) (1.0,i=1,nosca)
               do 30 il = noq1+1 , noq1+noq2
                  write ( lun , 2070 ) (ardata(i+(il-1)*noval), i=1,noval)
   30          continue
            endif
            if ( noq3 .gt. 0 ) then
               write ( lun , 2050 ) (1.0,i=1,nosca)
               do 40 il = noq1+noq2+1 , noq1+noq2+noq3
                  write ( lun , 2070 ) (ardata(i+(il-1)*noval), i=1,noval)
   40          continue
            endif
         else
            call dhctim(itime,itime2,.true.,.false.)
            write ( lun , 2060 ) itime2
            do 50 il = 1 , noloc
               write ( lun , 2070 ) (ardata(i+(il-1)*noval),i=1,noval)
   50       continue
         endif
      endif


      return
!
 2000 format ( '   3 ; time dependent data')
 2010 format ( '   1 ; block data')
 2020 format ( i6, ' ; number of series in this block')
 2030 format ( i6, ' ; series number')
 2040 format ( i4, ' ; number of breakpoints')
 2050 format ( 5(e13.6,1x) )
 2060 format ( i8, ' ; time')
 2070 format ( 5(e13.6,1x) )
 2080 format ( '   1 ; constant values without defaults')
 2090 format ( '''',a20,''' ; function name')
 2100 format ( '   2 ; linear interpolation')
      end
