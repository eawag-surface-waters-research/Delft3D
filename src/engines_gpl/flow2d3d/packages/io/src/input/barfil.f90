subroutine barfil(lundia    ,filbar    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,kmax      ,kspu      ,kspv      ,nsluv     , &
                & mnbar     ,nambar    ,cbuv      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!!--description-----------------------------------------------------------------
!
!    Function: Reads the barrier location definitions and
!              coefficients from the attribute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Local parameters
! 
    integer, parameter :: MAXFLD = 10 
!
! Global variables
!
    integer                                                                   , intent(in)  :: kmax   !  Description and declaration in iidim.f90
    integer                                                                                 :: lundia !  Description and declaration in inout.igs
    integer                                                                   , intent(in)  :: mmax   !  Description and declaration in iidim.f90
    integer                                                                                 :: nmax   !  Description and declaration in iidim.f90
    integer                                                                   , intent(in)  :: nmaxus !  Description and declaration in iidim.f90
    integer                                                                   , intent(in)  :: nsluv  !  Description and declaration in dimens.igs
    integer      , dimension(5, nsluv)                                        , intent(out) :: mnbar  !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspu   !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspv   !  Description and declaration in iidim.f90
    logical                                                                                 :: error  !!  Flag=TRUE if an error is encountered 
    real(fp)     , dimension(4, nsluv)                                        , intent(out) :: cbuv   !  Description and declaration in rjdim.f90
    character(*)                                                                            :: filbar !!  File name for Barriers/contr.gates
    character(20), dimension(nsluv)                                                         :: nambar !  Description and declaration in ckdim.f90
!
!
! Local variables
!
    integer                          :: ibar                 ! Loop variable 
    integer                          :: ibeg                 ! Begin position in the RECORD from where the search for data/record is started 
    integer                          :: idef                 ! Help var. containing default va- lue(s) for integer variable 
    integer                          :: iend                 ! Last position in the RECORD when the searched data/record is finished 
    integer                          :: ier                  ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                          :: inc                  ! loop counter 
    integer                          :: incx                 ! Increment between M1,M2 
    integer                          :: incy                 ! Increment between N1,N2 
    integer                          :: iocond               ! IO status for reading 
    integer                          :: lenc                 ! Number of char. to read in string 
    integer                          :: lfile                ! Number of non blank characters of file name 
    integer                          :: luntmp               ! Temporary file unit 
    integer                          :: m                    ! loop counter 
    integer                          :: m1                   ! First m-index for barrier 
    integer                          :: m2                   ! Last m-index for barrier 
    integer                          :: maxinc               ! Maximum of (increment between M1,M2 & increment between N1,N2) 
    integer                          :: n                    ! loop counter 
    integer                          :: n1                   ! First n-index for barrier 
    integer                          :: n2                   ! Last n-index for barrier 
    integer, external                :: newlun
    integer                          :: nlook                ! Nr. of values to look for in a record 
    integer                          :: nrflds
    integer, dimension(4)            :: ival                 ! Help array (integer) where the data, recently read from the MD-file, are stored temporarily 
    integer, dimension(maxfld)       :: ifield
    integer, dimension(maxfld)       :: itype
    integer, dimension(maxfld)       :: lenchr 
    logical, external                :: exifil
    real(fp)                         :: brlosc               ! Barrier energy loss coefficient 
    real(fp)                         :: gate                 ! Initial gate height for barrier 
    real(fp)                         :: rdef                 ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(maxfld)      :: rfield
    character(10), dimension(maxfld) :: cfield 
    character(1)                     :: direct               ! Help string for reading direction 
    character(132)                   :: rec132               ! Standard rec. length in an attribute file (132) 
    character(20)                    :: btype                ! Barrier type
    character(40)                    :: errmsg               ! Text string error messages 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----initialize local parameters
    !
    direct = ' '
    idef = 0
    rdef = 0.0
    lenc = 1
    errmsg = 'barrier '
    !
    !-----test file existance
    !
    call noextspaces(filbar    ,lfile     )
    error = .not.exifil(filbar(:lfile), lundia, 'G004', gdp)
    if (error) goto 9999
    !
    !-----open formatted file, if not formatted IOCOND <> 0
    !
    luntmp = newlun(gdp)
    open (luntmp, file = filbar(:lfile), form = 'formatted', status = 'old',    &
        & iostat = iocond)
    if (iocond/=0) then
       error = .true.
       call prterr(lundia    ,'G007'    ,filbar(:lfile)       )
       !
       goto 9999
    endif
    !
    !-----freeformatted file, skip lines starting with a '*'
    !
    call skipstarlines(luntmp    )
    !
    !-----freeformatted file, read input and test iocond
    !
    do ibar = 1, nsluv
    ! --->
  100  continue
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filbar(:lfile)       )
          !
          else
             call prterr(lundia    ,'G007'    ,filbar(:lfile)       )
          !
          endif
          error = .true.
          goto 300
       endif
       !
       !------- Ignore empty lines
       !
       if (rec132==' ') goto 100
       ! <---
       !
       !--------define barrier location name
       !
       nambar(ibar) = rec132(:20)
       call small(nambar(ibar)         ,20        )
       !
       !--------there must be a name defined !!
       !
       if (nambar(ibar)=='') then
          errmsg(12:) = ': no name defined'
          error = .true.
          call prterr(lundia    ,'U021'    ,errmsg    )
          !
          goto 300
       endif
       !
       !--------barrier direction is indicated by U or V string
       !
       ibeg = 21
       iend = 132
       call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
                 & ifield    ,rfield    ,cfield    ,lenchr    ,maxfld    , &
                 & .true.    ,.false.   ,.false.   )
       !
       if (nrflds<7 .or. itype(1)/=3) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
          !
          goto 300
       endif
       direct = cfield(1)(1:1)
       !
       !--------barrier location is defined in M,N coordinates
       !
       if (itype(2)/=1 .or. itype(3)/=1 .or. itype(4)/=1 .or. itype(5)/=1) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
          !
          goto 300
       endif
       !
       m1 = ifield(2)
       n1 = ifield(3)
       m2 = ifield(4)
       n2 = ifield(5)
       mnbar(1, ibar) = m1
       mnbar(2, ibar) = n1
       mnbar(3, ibar) = m2
       mnbar(4, ibar) = n2
       !
       !--------Test contents of read coordinate pairs
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       !
       !--------If error then no multiple of 45 degrees
       !
       if (error) then
          errmsg(9:) = nambar(ibar)
          call prterr(lundia    ,'V231'    ,errmsg(:32)          )
          !
          error = .true.
          goto 300
       endif
       !
       !--------Inside computational domain ?
       !
       if (m1>mmax .or. m1<1 .or. m2>mmax .or. m2<1) then
          call prterr(lundia    ,'V232'    ,errmsg(:22)          )
          !
          error = .true.
          goto 300
       endif
       if (n1>nmaxus .or. n1<1 .or. n2>nmaxus .or. n2<1) then
          call prterr(lundia    ,'V232'    ,errmsg(:22)          )
          !
          error = .true.
          goto 300
       endif
       !
       !--------test for barrier type 'U' or 'V' and fill mask array
       !
       call small(direct    ,1         )
       m = m1 - incx
       n = n1 - incy
       !
       !-----------First U-barriers
       !
       if (direct=='u') then
          mnbar(5, ibar) = 0
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             !
             !-----------------Check for different structures defined (not allowed)
             !
             if (abs(kspu(n, m, 0))>2) then
                errmsg(1:) = nambar(ibar) // ' on other structure'
                call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                !
                error = .true.
                goto 300
             endif
             !
             !-----------------Define structure point masks for barrier as 3D-gate
             !
             kspu(n, m, 0) = 4
          enddo
       !
       !-----------V-barriers
       !
       elseif (direct=='v') then
          mnbar(5, ibar) = 1
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             !
             !-----------------Check for different structures defined (not allowed)
             !
             if (abs(kspv(n, m, 0))>2) then
                errmsg(1:) = nambar(ibar) // ' on other structure'
                call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                !
                error = .true.
                goto 300
             endif
             !
             !-----------------Define structure point masks for barrier as 3D-gate
             !
             kspv(n, m, 0) = 4
          enddo
       else
          errmsg(23:26) = ' (' // direct // ')'
          call prterr(lundia    ,'V233'    ,errmsg(:11)          )
          !
          error = .true.
          goto 300
       endif
       !
       !--------switch between old barriers with BRLOSC and GATEHEIGHT versus
       !        new barriers that continue with barrier formulation type
       !
       if (itype(7)<3) then
          !
          !--> old barrier format:
          !--------read BRLOSC from record, default value not allowed
          !
          if (itype(6)>2 .or. itype(7)>2) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
             !
             goto 300
          endif
          !
          brlosc = rfield(6)
          gate   = rfield(7)
          !
          !--------Value for loss coefficient permitted ?
          !
          if (brlosc<0.0) then
             call prterr(lundia    ,'V234'    ,errmsg(:22)          )
             !
             error = .true.
          endif
          cbuv(1, ibar) = gate
          cbuv(2, ibar) = 1.0_fp
          cbuv(3, ibar) = brlosc
       else
          !
          !--> new barrier format:
          !
          if (itype(6)>2) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
             !
             goto 300
          endif
          cbuv(1, ibar) = rfield(6) ! gate height
          !
          btype = cfield(7)
          call small(btype ,20 )
          select case(btype)
          case('a')
             if (nrflds<8 .or. itype(8)>2) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
                !
                goto 300
             endif
             !
             cbuv(2, ibar) = 1.0_fp
             cbuv(3, ibar) = rfield(8) ! brlosc
             !
             !--------Value for loss coefficient permitted ?
             !
             if (rfield(8)<0.0) then
                error = .true.
                call prterr(lundia    ,'V234'    ,errmsg(:22)          )
                !
                goto 300
             endif
          case('b')
             if (nrflds<9 .or. itype(8)>2 .or. itype(9)>2) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
                !
                goto 300
             endif
             !
             cbuv(2, ibar) = 2.0_fp
             cbuv(3, ibar) = rfield(8)
             cbuv(4, ibar) = rfield(9)
             !
             !--------Value for loss coefficient permitted ?
             !
             if (rfield(8)<0.0 .or. (rfield(8)+rfield(9))<0.0) then
                error = .true.
                call prterr(lundia    ,'V234'    ,errmsg(:22)          )
                !
                goto 300
             endif
          end select
       endif
    enddo
    !
    !-----Not twice the same barrier name
    !
    do ibar = 1, nsluv
       do n = 1, ibar - 1
          if (nambar(n)==nambar(ibar)) then
             error = .true.
             call prterr(lundia    ,'U021'    ,'Barrier name already used: ' // nambar(ibar)         )
          !
          endif
       enddo
    enddo
    !
    !-----close file
    !
  300 continue
    close (luntmp)
    !
 9999 continue
end subroutine barfil
