!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine opt1 ( iopt1  , lun    , is     , lchar  , filtype,
     &                  dtflg1 , dtflg3 , nitem  , ierr   , iwar   ,
     &                  dont_read                                  )

!     Deltares Software Centre

!>\File
!>        Processing of first input file option
!>
!>        - Get the file name
!>        - Open the file
!>        - If ASCII, push file-info on include stack

!     CREATED            : April '88  BY M.E. Sileon / L. Postma

!     MODIFIED           : July     2002 by Leo Postma  : File option -4 allowed for ASCII description
!                                                         Subroutine FFFIND added
!                          February 2011 by Leo Postma  : Fortran-95 look and feel, streamlining,
!                                                         addition of filtype array for big endian files

!     SUBROUTINES CALLED : STRIP   user input file
!                          DHOPNF  open file

!     LOGICAL UNITS      : LUN(33) = working unit for opening binary files

      use m_zoek
      use m_fffind
      use m_dhopnf
      use timers       !   performance timers
      use rd_token
      use m_cnvtim
      use m_dhfext

      implicit none

!     Parameters    :
!     type     kind  function         name             description

      integer   (4) , intent(in   ) :: iopt1          !< Input option
      integer   (4) , intent(inout) :: lun  (*)       !< DELWAQ Unit number array
      integer   (4) , intent(in   ) :: is             !< entry in LUN for item
      character*(*) , intent(inout) :: lchar(*)       !< IN/OUT  Filenames
      logical       , intent(in   ) :: dtflg1         !< 'date'-format 1st time scale
      logical       , intent(in   ) :: dtflg3         !< 'date'-format (F;ddmmhhss,T;yydddhh)
      integer   (4) , intent(in   ) :: nitem          !< nr of input items expected
      integer   (4) , intent(inout) :: filtype(*)     !< type of binary file
      integer   (4) , intent(inout) :: ierr           !< Local error flag
      integer   (4) , intent(inout) :: iwar           !< Cumulative warning count
      logical       , intent(in)    :: dont_read      !< do not actually read tokens, if true, the information is already provided

!     local

      integer         extpos, extlen
      character(255)  cdummy   ! Work string
      character(255)  sfile , filext
      character( 25)  sstring
      integer   (4)   ifl      ! help variable for stack size
      integer   (4)   ierr2    ! help variable for error handling
      integer   (4)   lunin    ! help variable for opening external ASCII file
      integer   (4)   nfil     ! nr of files in hydrodynamics steering file
      integer   (4)   intopt   ! interpolation option in hydrodynamics steering file
      integer   (4)   ifil     ! loop counter for number of files
      real      (4)   fact     ! interpolation factor steering file
      integer   (4)   it1 , it2 , it3   ! timer variables
      integer   (4)   it1a, it2a, it3a  ! timer variables
      integer   (4)   itype    ! returned type of input from gettoken
      integer   (4)   ihyd     ! location of 'hyd' in the file name string
      integer   (4)   k        ! implicit loop counter
      real      (4)   adummy   ! dummy to read data from file
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "opt1", ithndl )

!           See what type of file it is
!           -4 the file is steering file for a series of binary files
!           -2 the file is a binary file interpolated when needed
!           -1 the file is an external ASCII file (superfluous because INCLUDE)
!            0 the file is a binary file one record per time step
!            1 the file is this input file

      select case ( iopt1 )

         case ( -1 )                      !    External ASCII file
            do ifil = 1 , lstack
               if ( ilun(ifil) .ne. 0 ) ifl = ifil
            enddo
            if ( ifl .eq. lstack ) then   !    No space on the stack
               write ( lunut , 2010 ) lstack
               ierr2 = 1
               goto 30
            endif
            if ( gettoken( cdummy, ierr2 ) .gt. 0 ) goto 30     !   Get file name
            write ( lunut , 2020 )  cdummy
            ifl       = ifl + 1
            lunin     = 800 + ifl
            call dhopnf  ( lunin , cdummy , 33 , 1 , ierr2 )    !   Open the file
            if ( ierr2 .gt. 0 ) then
               ifl = ifl - 1
               write ( lunut , 2030 )
            else
               lch (ifl) = cdummy
               ilun(ifl) = lunin
            endif

         case ( -2, 0 )                   !    External binairy intermediate file
            if ( dont_read ) then
               cdummy = lchar(is)
            else
   10          if ( gettoken( cdummy, ierr2 ) .gt. 0 ) goto 30     !   Get file name
               if ( cdummy .eq. 'UNFORMATTED' ) then
                  filtype(is) = filtype(is) + 10
                  write ( lunut, * ) 'UNFORMATTED file detected'
                  goto 10
               endif
               if ( cdummy .eq. 'BIG_ENDIAN' ) then
                  filtype(is) = filtype(is) + 20
                  write ( lunut, * ) 'BIG_ENDIAN  file detected'
                  goto 10
               endif
            endif
            lchar(is) = cdummy
            write ( lunut , 2040 ) cdummy
!                   Check if file exists
            call dhopnf  ( lun(33) , cdummy    , 33     , 2     , ierr2 )
            if ( ierr2 .gt. 0 ) then
               ierr2 = -2
            else
               close ( lun(33) )
            endif

         case ( -4 )                      !    ASCII steering file taylored to read .hyd files
            if ( nitem .eq. 0 ) then
               write ( lunut , 2000 )
               ierr2 = 1
               goto 30
            endif
            itype = 1
            do while ( itype .eq. 1 )
               if ( gettoken( cdummy , nfil  , itype, ierr2 ) .gt. 0 ) goto 30  !   Get number of files
               if ( itype .eq. 1 ) then
                  if ( cdummy .eq. 'UNFORMATTED' ) then
                     filtype(is) = filtype(is) + 10
                     write ( lunut, * ) 'UNFORMATTED file detected'
                  else if ( cdummy .eq. 'BIG_ENDIAN' ) then
                     filtype(is) = filtype(is) + 20
                     write ( lunut, * ) 'BIG_ENDIAN  file detected'
                  else
                     write ( lunut, 2150 ) cdummy
                     ierr2 = 1
                     goto 30
                  endif
               endif
            enddo
            if ( gettoken( intopt , ierr2 ) .gt. 0 ) goto 30     !   Get interpolation option
            if ( gettoken( sstring, ierr2 ) .gt. 0 ) goto 30     !   Get file string
!                 Open the binary intermediate file for output
            call dhfext(lchar(27),filext,extpos,extlen)
            lchar(is) = lchar(27)(1:max(1,(extpos-1)))//'-'//sstring
            call dhfext(lchar(is),filext,extpos,extlen)
            lchar(is)(extpos:) = '.wrk'
            call dhopnf  ( lun(is), lchar(is),  1 , 1   , ierr2 )
            if ( ierr2 .gt. 0 ) then
               ierr2 = -2
               goto 30
            endif
            write(lunut,2120)  lchar(is),intopt
            write(lun(is)   )  'Steering file '
            write(lun(is)   )   nfil,intopt
            write ( lunut , 2080 )
            do  20 ifil = 1,nfil
               if ( gettoken( fact   , ierr2 ) .gt. 0 ) goto 30  !   Get multiplication factor
               if ( gettoken( cdummy , it1   , itype, ierr2 ) .gt. 0 ) goto 30    ! 'from' time
               if ( itype .eq. 1 ) then
                  call dlwq0t(cdummy, it1    , .false., .false., ierr2  )
                  if ( ierr2 .gt. 0 ) then
                     write ( lunut , 2130 ) trim(cdummy)
                     goto 30
                  endif
                  if ( it1   .eq. -999 ) then
                     write ( lunut , 2140 ) trim(cdummy)
                     ierr2 = 1
                     goto 30
                  endif
               else
                  call cnvtim ( it1    , 1     , dtflg1 , dtflg3 )
               endif
               if ( gettoken( cdummy , it2   , itype, ierr2 ) .gt. 0 ) goto 30    ! 'to' time
               if ( itype .eq. 1 ) then
                  call dlwq0t(cdummy, it2    , .false., .false., ierr2  )
                  if ( ierr2 .gt. 0 ) then
                     write ( lunut , 2130 ) trim(cdummy)
                     goto 30
                  endif
                  if ( it2   .eq. -999 ) then
                     write ( lunut , 2140 ) trim(cdummy)
                     ierr2 = 1
                     goto 30
                  endif
               else
                  call cnvtim ( it2    , 1     , dtflg1 , dtflg3 )
               endif
               if ( gettoken( cdummy , it3   , itype, ierr2 ) .gt. 0 ) goto 30    ! 'step'
               if ( itype .eq. 1 ) then
                  call dlwq0t(cdummy, it3    , .false., .false., ierr2  )
                  if ( ierr2 .gt. 0 ) then
                     write ( lunut , 2130 ) trim(cdummy)
                     goto 30
                  endif
                  if ( it3   .eq. -999 ) then
                     write ( lunut , 2140 ) trim(cdummy)
                     ierr2 = 1
                     goto 30
                  endif
               else
                  call cnvtim ( it3    , 1     , dtflg1 , dtflg3 )
               endif
               if ( gettoken( sfile , ierr2 ) .gt. 0 ) then       !     Get file string
                  ierr2 = -1
                  goto 30
               endif
               call dhfext(sfile,filext,extpos,extlen)
               call zoek ( 'hyd ', 1, filext, 4, ihyd )
               if ( ihyd .eq. 1 ) then                            !     hyd file processing
                  call fffind ( lunut, sstring, sfile , cdummy, it3   ,
     &                          it1a   , it2a  , it3a , nitem , ierr  )
               else                                               !     other file processing
                  cdummy = sfile
                  it2a   = 0
                  call dhopnf  ( lun(33), cdummy   , 33    , 2    , ierr2 )
                  if ( ierr2 .gt. 0 ) then
                     ierr2 = -2
                     goto 30
                  endif
                  read ( lun(33) ) it1a,(adummy,k=1,nitem)
                  read ( lun(33) ) it3a
                  it3a = it3a-it1a
                  close ( lun(33) )
               endif
               write ( lun(is)    ) fact,it1,it2,it3,cdummy,it1a,it2a,it3a
               write ( lunut, 2090) ifil, fact,
     &                              it1/31536000       , mod(it1,31536000)/86400,
     &                              mod(it1,86400)/3600, mod(it1,3600)/60       ,
     &                              mod(it1,60)        ,
     &                              it2/31536000       , mod(it2,31536000)/86400,
     &                              mod(it2,86400)/3600, mod(it2,3600)/60       ,
     &                              mod(it2,60),
     &                              it3/31536000       , mod(it3,31536000)/86400,
     &                              mod(it3,86400)/3600, mod(it3,3600)/60       ,
     &                              mod(it3,60), cdummy
               if ( ierr .ne. 0 ) then
                  ierr2 = -1
                  exit
               endif
   20       continue
            close ( lun(is) )

         case ( 1 )            !   continue reading from current file
            write ( lunut , 2050 )
            ierr2 = 0

         case default
            write ( lunut , 2000 )
            ierr2 = 1

   30 end select

      select case ( ierr2 )

         case ( -2 )
            write ( lunut , 2060 ) cdummy
            iwar = iwar + 1
            ierr = 0

         case ( -1 )
            write ( lunut , 2100 ) cdummy
            ierr = 1

         case (  0 )
            ierr = 0

         case ( 1: )
            write ( lunut , 2070 )
            ierr = 1

      end select

      if (timon) call timstop( ithndl )
      return

!       output formats

 2000 format (/' ERROR: option not implemented !!!' )
 2010 format (/' ERROR: nr of include stack levels (',I2,') exceeded !')
 2020 format (/' Including file: ',A )
 2030 format (/' ERROR: Include file does not exist !' )
 2040 format (/' Information from unformatted intermediate file.' ,
     &        /' Filename is: ',A )
 2050 format ( ' Information from the standard input file.' )
 2060 format ( ' WARNING file does not exist.'/' Filename: ',A )
 2070 format ( ' ERROR reading input!' )
 2080 format (/' Nr: Interpolation        From                  ',
     &         ' To                  Start with      Hydro-dynamic',
     &        /'        factor                                  ',
     &         '                                     description file:')
 2090 format (   I3,F10.3,6X,I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S. ',
     &           I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S. ',
     &           I2,'Y-',I3,'D-',I2,'H-',I2,'M-',I2,'S. ',A )
 2100 format (/' ERROR: Reading ASCII description file ',A)
 2110 format (/' ERROR: This option is not supported here !')
 2120 format ( ' Work file = ',A,' Interpolation option:',I2)
 2130 format (/' ERROR: String is not a valid absolute timer:',A)
 2140 format (/' ERROR: Absolute timer does not fit in timer format :',A,/
     &          ' Is your T0 setting in block #1 correct?'/,
     &          ' Allowed difference with T0 is usually ca. 68 years.' )
 2150 format (/' ERROR: Not a valid token at this position: ',A)

      end
