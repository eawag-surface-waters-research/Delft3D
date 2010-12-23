subroutine rdscour(error, nrrec, mdfrec, gdp)
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
! Manage User Input from file = scour.inp
! Requests: LOAD  open and read file, save data
! TAKE  store data
! GIVE  return data
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
    ! They replace the  include igd / include igp lines
    !
    integer                , pointer :: nmax
    integer                , pointer :: ddbound
    integer                , pointer :: nmaxd
    integer                , pointer :: nof
    integer, dimension(:)  , pointer :: nmapp
    integer, dimension(:)  , pointer :: nmref
    logical                , pointer :: scour
    real(fp), dimension(:) , pointer :: factor
    real(fp)               , pointer :: slope
    type (gd_scour)        , pointer :: gdscour
    integer                , pointer :: lunmd
    integer                , pointer :: lundia
    integer                , pointer :: itis
!
! Global variables
!
    integer              :: nrrec
    logical, intent(out) :: error
    character(*)         :: mdfrec
!
! Local variables
!
    integer                              :: ddb
    integer                              :: dummy
    integer                              :: i
    integer                              :: i1
    integer                              :: i2
    integer                              :: idx
    integer                              :: inp
    integer                              :: iost    ! IO-errorcodes
    integer                              :: lenc
    integer                              :: lfile
    integer                              :: lkw
    integer                              :: m
    integer                              :: n
    integer, dimension(:,:), allocatable :: nmappin
    integer, dimension(:,:), allocatable :: nmrefin
    integer                              :: nmaxddb
    integer                              :: ntrec
    integer                              :: nlook
    integer, external                    :: newlun
    logical                              :: found
    logical                              :: lex
    logical                              :: newkw
    character                            :: fildef
    character(6)                         :: keyw
    character(256)                       :: errmsg
    character(256)                       :: flname
!
!! executable statements -------------------------------------------------------
!
    nmax       => gdp%d%nmax
    ddbound    => gdp%d%ddbound
    nmaxd      => gdp%d%nmaxd
    nof        => gdp%gdscour%nof
    nmapp      => gdp%gdscour%nmapp
    nmref      => gdp%gdscour%nmref
    scour      => gdp%gdscour%scour
    factor     => gdp%gdscour%factor
    slope      => gdp%gdscour%slope
    gdscour    => gdp%gdscour
    lunmd      => gdp%gdinout%lunmd
    lundia     => gdp%gdinout%lundia
    itis       => gdp%gdrdpara%itis
    !
    error  = .false.
    newkw  = .true.
    found  = .false.
    fildef = ' '
    nlook  = 1
    !
    nof = 0
    !
    ! locate 'Scour' record for attribute file containing parameters
    ! for transport formulation
    !
    keyw = 'Scour'
    ntrec = nrrec
    lkw = 6
    call search(lunmd     ,error     ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    !
    ! read when found
    !
    if (found) then
       lenc = 256
       call read2c(lunmd     ,error     ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,flname    ,fildef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (error) then
          errmsg = 'Not able to read keyword Scour'
          call prterr(lundia    ,'U021'    ,errmsg    ,gdp       )
          goto 9999
       endif
       call noextspaces(flname, lfile)
       !
       inquire (file = flname(1:lfile), exist = lex)
       if (lex) then
          inp = newlun(gdp)
          open (inp, file = flname(1:lfile),status = 'old', iostat = iost)
          if (iost/=0) then
             call prterr(lundia, 'G004', flname(1:lfile), gdp)
             call d3stop(1, gdp)
          endif
          read (inp, *, iostat = iost) i
          if (iost/=0 .or. i==0) then
             goto 9999
          else
             scour   = .true.
             errmsg  = 'Scouring flag = ON'
             call prterr(lundia    ,'G051'    ,errmsg    ,gdp       )
          endif
          !
          ! read slope
          !
          read (inp, *) slope
          !
          ! Dummy read to get nof records ( = points)
          !
          read (inp, *, iostat = iost) m
          do while (iost==0)
             nof = nof + 1
             read (inp, *, iostat = iost) m
          enddo
          !
          ! End of file found
          !
          ! Now the required memory is known
          ! Allocate using the gdp structure itself instead of the local pointers
          !
                       allocate (nmappin(2, nof), stat = iost)
          if (iost==0) allocate (nmrefin(2, nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%nmapp(nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%nmref(nof), stat = iost)
          if (iost==0) allocate (gdp%gdscour%factor(nof), stat = iost)
          if (iost/=0) then
             call prterr(lundia, 'U021', 'Rdscour: memory alloc error', gdp)
             call d3stop(1, gdp)
          endif
          !
          ! include .igp again to be sure that the local pointers
          ! point to the allocated memory
          !
    nof        => gdp%gdscour%nof
    nmapp      => gdp%gdscour%nmapp
    nmref      => gdp%gdscour%nmref
    scour      => gdp%gdscour%scour
    factor     => gdp%gdscour%factor
    slope      => gdp%gdscour%slope
    gdscour    => gdp%gdscour
          rewind (inp)
          read (inp, *) dummy
          read (inp, *) dummy
          do n = 1, nof
             read (inp, *) (nmrefin(i, n), i = 1, 2), (nmappin(i, n), i = 1, 2),       &
                         & factor(n)
          enddo
          close (inp)
          !
          ! convert nmappin(n,nof), nmappin(m,nof) to nmapp(nof) nm point
          ! convert nmrefin(n,nof), nmrefin(m,nof) to nmref(nof) nm point
          !
          ddb     = gdp%d%ddbound
          nmaxddb = nmax + 2*ddb
          idx = 0
          do n = 1, nof
             idx = idx + 1
             nmapp(idx) = nmappin(2, n) + ddb + (nmappin(1, n) + ddb - 1)*nmaxddb
             nmref(idx) = nmrefin(2, n) + ddb + (nmrefin(1, n) + ddb - 1)*nmaxddb
          enddo
          deallocate (nmappin)
          deallocate (nmrefin)
       else
          errmsg = 'Scour file '//flname(1:lfile)//' does not exist'
          call prterr(lundia    ,'U021'    ,errmsg    ,gdp       )
          goto 9999
       endif
    else
       ! no scour
    endif
 9999 continue
end subroutine rdscour
