subroutine incdis(lundia    ,sferic    ,grdang    ,timnow    ,nsrc      , &
                & lstsc     ,lstsci    ,j         ,nmmaxj    ,kmax      , &
                & icx       ,icy       ,kfsmin    ,kfsmx0    , &
                & disint    ,dismmt    ,itdis     ,kcu       ,kcv       , &
                & kfs       ,iwrk      ,mnksrc    ,alfas     ,xcor      , &
                & ycor      ,dp        ,disch     , &
                & disch0    ,disch1    ,rint      ,rint0     ,rint1     , &
                & umdis     ,umdis0    ,umdis1    ,vmdis     ,vmdis0    , &
                & vmdis1    ,bubble    ,r0        ,thick     ,relthk    , &
                & dzs0      ,dps       ,s0        ,gdp       )
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
!    Function: Determine increments and updates the current time
!              dependent value for discharge flows and conc.
!              (if fldis = TRUE)
! Method used: At each time step (if DISINT=TRUE) 
!              New values with linear interpoaltion are determined
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer               , pointer :: itstop
    integer               , pointer :: lundis
    integer               , pointer :: ltem
    real(fp)              , pointer :: tstop
    real(fp)              , pointer :: dt
    real(fp)              , pointer :: cp
    real(fp)              , pointer :: rhow
    real(fp)              , pointer :: maxTOutlet
    real(fp), dimension(:), pointer :: capacity
    real(fp)              , pointer :: eps
    real(fp)              , pointer :: scalef
    logical               , pointer :: zmodel
    logical , dimension(:), pointer :: flbub
!
! Global variables
!
    integer                                                                   :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                        !!  then computation proceeds in the X-
                                                                                        !!  dir. If icx=1 then computation pro-
                                                                                        !!  ceeds in the Y-dir.
    integer                                                                   :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                   :: j      !!  Begin pointer for arrays which have
                                                                                        !!  been transformed into 1D arrays.
                                                                                        !!  Due to the shift in the 2nd (M-)
                                                                                        !!  index, J = -2*NMAX + 1
    integer                                                     , intent(in)  :: kmax
    integer                                                                   :: lstsc  !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lstsci ! Description and declaration in iidim.f90
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                                   :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                                   :: nsrc   !  Description and declaration in iidim.f90
    integer     , dimension(5, nsrc)                                          :: itdis  !  Description and declaration in iidim.f90
    integer     , dimension(7, nsrc)                                          :: mnksrc !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: iwrk
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcu    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcv    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfs    !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmx0 !  Description and declaration in iidim.f90
    logical                                                     , intent(in)  :: bubble !  Description and declaration in procs.igs
    logical                                                                   :: sferic !  Description and declaration in tricom.igs
    real(fp)                                                                  :: grdang !  Description and declaration in tricom.igs
    real(fp)                                                                  :: timnow !!  Current timestep (multiples of dt)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: alfas  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dp     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: xcor   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: ycor   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     ! Description and declaration in rjdim.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in rjdim.f90
    real(fp)    , dimension(lstsc, nsrc)                                      :: rint   !  Description and declaration in rjdim.f90
    real(fp)    , dimension(lstsc, nsrc)                                      :: rint0  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(lstsc, nsrc)                                      :: rint1  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: disch  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: disch0 !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: disch1 !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: umdis  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: umdis0 !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: umdis1 !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: vmdis  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: vmdis0 !  Description and declaration in rjdim.f90
    real(fp)    , dimension(nsrc)                                             :: vmdis1 !  Description and declaration in rjdim.f90
    real(fp)    , dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in rjdim.f90
    real(fp)    , dimension(kmax)                                             :: relthk
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs0   !  Description and declaration in rjdim.f90
    real(prec)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in rjdim.f90
    character(1), dimension(nsrc)                                             :: disint !  Description and declaration in ckdim.f90
    character(1), dimension(nsrc)                                             :: dismmt !  Description and declaration in ckdim.f90
!
!> Local variables
!
    integer,external  :: newlun
    integer           :: dfil
    integer           :: ddb
    integer           :: i
    integer           :: icxy    !< MAX value of ICX and ICY 
    integer           :: isrc    !< Index number of discharge location 
    integer           :: jsrc
    integer           :: k
    integer           :: kk
    integer           :: l
    integer           :: nm      !< N,M index for discharge location 
    real(fp)          :: hCap    !< heat capacity that is going to be given to 1 m^3 cooling water
    real(fp)          :: timscl  !< Multiple factor to create minutes from read times
    real(fp)          :: tInlet
    real(fp)          :: alpha   !< linear interpolation factor
    real(fp)          :: rtdis0  !< Previous  read time for discharge time-series 
    real(fp)          :: rtdis1  !< Following read time for discharge time-series
    character(200)    :: filename
!
!! executable statements -------------------------------------------------------
!
    tstop      => gdp%gdexttim%tstop
    dt         => gdp%gdexttim%dt
    itstop     => gdp%gdinttim%itstop
    lundis     => gdp%gdluntmp%lundis
    ltem       => gdp%d%ltem
    cp         => gdp%gdheat%cp
    rhow       => gdp%gdphysco%rhow
    maxTOutlet => gdp%gddischarge%maxTOutlet
    capacity   => gdp%gddischarge%capacity
    eps        => gdp%gdconst%eps
    scalef     => gdp%gdupddis%scalef
    zmodel     => gdp%gdprocs%zmodel
    flbub      => gdp%gdbubble%flbub
    !
    timscl = 1.0
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    !
    ! change the location in case of walking discharge
    !
    do i = 1, nsrc
       if (mnksrc(7, i) == 1) then
          !
          ! it is a walking discharge
          ! Always start searching wet point at original location
          !
          isrc = mnksrc(1, i)
          jsrc = mnksrc(2, i)
          nm = jsrc + ddb + ((isrc + ddb) - 1)*icxy
          if (kfs(nm)==0) then
             call wetdis(i         ,isrc      ,jsrc      ,dp        ,xcor      , &
                       & ycor      ,kcu       ,kcv       ,kfs       ,iwrk      , &
                       & j         ,nmmaxj    ,icx       ,icy       ,gdp       )
          endif
          mnksrc(4, i) = isrc
          mnksrc(5, i) = jsrc
       endif
       if (mnksrc(7,i) == 6) then
          !
          ! Q-type power station
          ! Not the discharge, but the capacity (= amount of heat to be added to the water (per second)) is given in the input
          !
          if (comparereal(capacity(i),eps)==1) then
             !
             ! Reset disch to the capacity values stored
             ! Subroutine incdis is the only one where disch/capacity are mixed
             !
             disch(i) = capacity(i)
          endif
       endif
    enddo
    !
    ! Loop over all discharge locations NSRC
    !
    do isrc = 1, nsrc
       !
       !  Skip this point when it is outside this partition
       !
       if (mnksrc(6,isrc) == -1) cycle

       if (bubble) then
          flbub(isrc) = .false.
       endif
       if (timnow >= real(itdis(2,isrc),fp)) then
          !
          ! Define discharge location
          ! Test over KCS is not necessary. Discharge location always
          ! in point with KCS = 1 (see CHKDIS)
          !
          nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
          !
          ! Read new time step and discharge data
          !
          call upddis(lundis    ,lundia    ,sferic    ,itdis     , &
                    & isrc      ,nm        ,grdang    ,timnow    ,dt        , &
                    & itstop    ,timscl    ,nsrc      ,lstsc     ,j         , &
                    & nmmaxj    ,dismmt    ,alfas     , &
                    & disch0    ,disch1    ,rint0     ,rint1     , &
                    & umdis0    ,umdis1    ,vmdis0    ,vmdis1    ,gdp       )
          if (bubble) then
             flbub(isrc) = .true.
          endif
          if (disint(isrc) /= 'Y') then
               disch(isrc) = disch0(isrc)
               umdis(isrc) = umdis0(isrc)
               vmdis(isrc) = vmdis0(isrc)
               do l = 1, lstsc
                   rint(l, isrc) = rint0(l, isrc)
               enddo
          endif
       endif
       !
       if (disint(isrc) == 'Y') then
            if (itdis(1,isrc) == itdis(2,isrc)) then
               alpha = 0.0_fp
            else
               rtdis0 = real(itdis(1,isrc),fp)
               rtdis1 = real(itdis(2,isrc),fp)
               alpha  = (timnow-rtdis0) / (rtdis1-rtdis0)
            endif
            disch(isrc) = (1.0_fp-alpha)*disch0(isrc) + alpha*disch1(isrc)
            umdis(isrc) = (1.0_fp-alpha)*umdis0(isrc) + alpha*umdis1(isrc)
            vmdis(isrc) = (1.0_fp-alpha)*vmdis0(isrc) + alpha*vmdis1(isrc)
            do l = 1, lstsc
                rint(l, isrc) = (1.0_fp-alpha)*rint0(l, isrc) + alpha*rint1(l, isrc)
            enddo
       endif
       if (mnksrc(7,isrc) == 6) then
          !
          ! Q-type power station:
          ! - Store the new capacity in the capacity array
          ! - Calculate the new discharge
          !
          capacity(isrc) = disch(isrc)
          !
          ! get intake location
          !
          if (disch(isrc) >= 0.0_fp ) then
             nm = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
             kk = mnksrc(3, isrc)
          else
             nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
             kk = mnksrc(6, isrc)
          endif
          !
          ! get intake temperature
          !
          tInlet = 0.0_fp
          if (zmodel) then
             if (kk == 0) then
                do k = 1, kmax
                   relthk(k) = dzs0(nm,k) / (real(dps(nm),fp)+s0(nm))
                   tInlet    = tInlet + r0(nm,k,ltem)*relthk(k)
                enddo
             else
                if (kk > kfsmx0(nm)) kk = max(kfsmx0(nm), kfsmin(nm))
                if (kk < kfsmin(nm)) kk = kfsmin(nm)
                tInlet = r0(nm, kk, ltem)
             endif
          else
             !
             ! sigma model
             !
             if (kk == 0) then
                do k = 1, kmax
                   tInlet = tInlet + r0(nm, k, ltem)*thick(k)
                enddo
             else
                tInlet = r0(nm, kk, ltem)
             endif
          endif
          !
          ! calculate discharge
          !
          hCap = rhow * cp * (min(tInlet+rint(ltem,isrc),maxTOutlet)-tInlet)
          disch(isrc) = capacity(isrc) / hCap
       endif
    enddo
    !
    ! cancel where mnksrc(6,.)=-1 (out of domain)    
    !
    do isrc = 1, nsrc
       if (mnksrc(6, isrc) == -1 ) disch(isrc) = 0.0
    enddo
end subroutine incdis
