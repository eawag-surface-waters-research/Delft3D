module waqmor_module
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------

use waqsim_module

contains

subroutine morini2waqdef(gdmorpar, gdsedpar, gdtrapar, waqmor)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module
    use sediment_basics_module
    !
    implicit none
!
! Call variables
!
    type (gd_morpar)                         , pointer     :: gdmorpar
    type (gd_sedpar)                         , pointer     :: gdsedpar
    type (gd_trapar)                         , pointer     :: gdtrapar
    type (waqsimtype)                        , target      :: waqmor
!
! Local variables
!
    integer, parameter          :: MAXNPAR_PER_FRACTION = 100
    integer, parameter          :: MAXNPAR_MOR          = 100
    !
    integer                     :: i
    integer                     :: ipar
    integer                     :: j
    integer                     :: iout
    integer                     :: iproc
    integer                     :: istat ! error flag during allocation
    integer                     :: nIBS  ! number of bedload/total load fractions
    integer                     :: nISS  ! number of suspended fractions
    character(7)                :: num      ! number string for fraction
    character(6)                :: trapar   ! name of transport parameter
    character(1024)             :: errmsg
!
!! executable statements -------------------------------------------------------
!
    nISS = 0
    nIBS = 0
    do i = 1, size(gdsedpar%sedtyp,1)
       if (gdsedpar%sedtyp(i)==SEDTYP_NONCOHESIVE_TOTALLOAD) then
          nIBS = nIBS+1
       else
          nISS = nISS+1
       endif
    enddo
    !
    call addpar(waqmor, 'RhoWater', RVAL=1000.0_fp)
    call addprocess(waqmor, 'DynDepth')  ! determine Depth
    call addprocess(waqmor, 'TotDepth')  ! determine LocalDepth
    call addprocess(waqmor, 'kfSed')     ! determine morphological active/inactive segments
    call addoutput(waqmor, 'LocalDepth')
    ! zcoordwaterlevel
    !
    call addprocess(waqmor, 'VelocM')
    call addprocess(waqmor, 'BedDiam')
    call addprocess(waqmor, 'BedFrac')
    if (nISS>0) then
       call addsubs(waqmor, 'ISS', .true., nISS)
       call addprocess(waqmor, 'TraFrmISS')
       call addprocess(waqmor, 'ExchTraISS')
    endif
    if (nIBS>0) then
       call addsubs(waqmor, 'IBS', .false., nIBS)
       call addprocess(waqmor, 'TraFrmIBS')
       call addprocess(waqmor, 'ExchTraIBS')
    endif
    !
    call addsubs(waqmor, 'ZB', .false., 1)
    call addprocess(waqmor, 'BedSlope')
    !
    do j = 1,nISS+nIBS
       if (j<=nISS) then
          num = '*ISS'
          i = j
          if (nISS==1) i = 0
       else
          num = '*IBS'
          i = j-nISS
          if (nIBS==1) i = 0
       endif
       if (i==0) then
          ! don't add a number if there is only one fraction
       elseif (i<100) then
          write(num(5:6),'(I2.2)') i
       else
          write(num(5:7),'(I3.3)') i
       endif
       !
       if (j<=nISS) call addpar(waqmor, 'type'  //num, IVAL=gdsedpar%sedtyp(j))
       call addpar(waqmor, 'rho'   //num, RVAL=gdsedpar%rhosol(j))
       call addpar(waqmor, 'rhobed'//num, RVAL=gdsedpar%cdryb(j))
       if (gdsedpar%sedtyp(j) /= SEDTYP_COHESIVE) then
          call addpar(waqmor, 'dmin'  //num, RVAL=exp(gdsedpar%logseddia(2,1,j)))
          call addpar(waqmor, 'dmax'  //num, RVAL=exp(gdsedpar%logseddia(2,gdsedpar%nseddia(j),j)))
          call addpar(waqmor, 'd50'   //num, RVAL=gdsedpar%sedd50(j))
          call addpar(waqmor, 'd90'   //num, RVAL=gdsedpar%sedd90(j))
          call addpar(waqmor, 'logstd'//num, RVAL=gdsedpar%logsedsig(j))
       endif
       call addpar(waqmor, 'IniThk'//num, RVAL=gdsedpar%sdbuni(j))
       call addpar(waqmor, 'TraFrm'//num, IVAL=gdtrapar%iform(j))
       !
       trapar = 'TrPa'
       do ipar = 1,10
          write(trapar(5:6),'(I2.2)') ipar
          if (gdtrapar%iparfld(ipar+10,j) /= 0) then
             ! spatially varying field
             call addfld(waqmor, trapar//num, RVAL=gdtrapar%parfld(:,gdtrapar%iparfld(ipar+10,j)))
          elseif (gdtrapar%par(ipar+10,j) /= 0.0) then
             call addpar(waqmor, trapar//num, RVAL=gdtrapar%par(ipar+10,j))
          endif
       enddo
       !
       gdmorpar%moroutput%sbcuv = .true.
       if (gdmorpar%moroutput%sbcuv) then
          call addoutput(waqmor, 'SBCx'//num)
          call addoutput(waqmor, 'SBCy'//num)
       endif
       !if (gdmorpar%moroutput%sbcuuvv)  call addoutput(waqmor, '???')
       if (gdmorpar%moroutput%sbwuv) then
          call addoutput(waqmor, 'SBWx'//num)
          call addoutput(waqmor, 'SBWy'//num)
       endif
       !if (gdmorpar%moroutput%sbwuuvv)  call addoutput(waqmor, '???')
       if (gdmorpar%moroutput%sswuv) then
          call addoutput(waqmor, 'SSWx'//num)
          call addoutput(waqmor, 'SSWy'//num)
       endif
       !if (gdmorpar%moroutput%sswuuvv)  call addoutput(waqmor, '???')
       !if (gdmorpar%moroutput%suvcor)  call addoutput(waqmor, '???')
       if (gdmorpar%moroutput%sourcesink .and. j<=nISS) then
          call addoutput(waqmor, 'SourSe'//num)
          call addoutput(waqmor, 'SinkSe'//num)
       endif
       if (gdmorpar%moroutput%frac)  call addoutput(waqmor, 'frac'//num)
    enddo
    !
    if (gdmorpar%moroutput%AKS) call addoutput(waqmor, 'aks')
    !if (gdmorpar%moroutput%cumavg)  call addoutput(waqmor, '???')
    if (gdmorpar%moroutput%dg)  call addoutput(waqmor, 'DG_mix')
    if (gdmorpar%moroutput%dm)  call addoutput(waqmor, 'DM_mix')
    !if (gdmorpar%moroutput%dzduuvv)  call addoutput(waqmor, '???')
    if (gdmorpar%moroutput%fixfac)  call addoutput(waqmor, 'fixfac')
    !if (gdmorpar%moroutput%hidexp)  call addoutput(waqmor, '???')
    if (gdmorpar%moroutput%mudfrac)  call addoutput(waqmor, 'fracMud')
    !if (gdmorpar%moroutput%percentiles)  call addoutput(waqmor, '???')
    !if (gdmorpar%moroutput%taurat)  call addoutput(waqmor, '???')
    if (gdmorpar%moroutput%umod)  call addoutput(waqmor, 'UMor')
    if (gdmorpar%moroutput%zumod) call addoutput(waqmor, 'zUMor')
    if (gdmorpar%moroutput%ustar) call addoutput(waqmor, 'UStar')
    if (gdmorpar%moroutput%uuuvvv) then
       call addoutput(waqmor, 'UxMor')
       call addoutput(waqmor, 'UyMor')
    endif
    !
    call addpar(waqmor, 'Tau', RVAL=0.01_fp) ! Use Tau process?
    call addpar(waqmor, 'ksRip', RVAL=0.01_fp) ! probably need to add process to compute this ...
    call addprocess(waqmor, 'Z0') ! z0cur and z0rou
    !
    call addpar(waqmor, 'ThrNonErod', RVAL=gdmorpar%thresh)
    !
    if (gdmorpar%bedupd) then
       call addprocess(waqmor, 'BedUpdate')
       call addoutput(waqmor, 'dZB')
    endif
end subroutine morini2waqdef


subroutine addpar(waqmor,newpar,rval,ival)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    implicit none
!
! Call variables
!
    type (waqsimtype)                          :: waqmor
    character(*)               , intent(in)    :: newpar
    integer          , optional, intent(in)    :: ival
    real(fp)         , optional, intent(in)    :: rval
!
! Local variables
!
    integer                                    :: ipar
!
!! executable statements -------------------------------------------------------
!
   ipar = waqmor%npar+1
   call realloc(waqmor%parname,ipar)
   call realloc(waqmor%parval,ipar)
   waqmor%parname(ipar) = newpar
   if (present(ival)) then
      waqmor%parval(ipar) = ival
   elseif (present(rval)) then
      waqmor%parval(ipar) = rval
   endif
   waqmor%npar = ipar
end subroutine addpar


subroutine addfldex(waqmor,newfld,cval)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    implicit none
!
! Call variables
!
    type (waqsimtype)                          :: waqmor
    character(*)               , intent(in)    :: newfld
    real(fp)         , optional, intent(in)    :: cval
!
! Local variables
!
    integer                                    :: ifld
    integer, dimension(2)                      :: newsize
!
!! executable statements -------------------------------------------------------
!
   ifld = waqmor%nfldex+1
   call realloc(waqmor%fldexname,ifld)
   newsize(1) = ifld
   newsize(2) = sum(waqmor%nexchange)
   call realloc(waqmor%fldexval, newsize )
   waqmor%fldexname(ifld) = newfld
   if (present(cval)) then
      waqmor%fldexval(ifld,:) = cval
   else
      waqmor%fldexval(ifld,:) = 0.0
   endif
   waqmor%nfldex = ifld
end subroutine addfldex


subroutine addfld(waqmor,newfld,rval)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    implicit none
!
! Call variables
!
    type (waqsimtype)                            :: waqmor
    character(*)                    , intent(in) :: newfld
!    integer , dimension(:), optional, intent(in) :: ival
    real(fp), dimension(:), optional, intent(in) :: rval
!
! Local variables
!
    integer                                    :: i
    integer                                    :: j
    integer                                    :: ifld
    integer                                    :: nsegments2D
    integer, dimension(2)                      :: newsize
!
!! executable statements -------------------------------------------------------
!
   ifld = waqmor%nfld+1
   call realloc(waqmor%fldname,ifld)
   newsize(1) = ifld
   newsize(2) = waqmor%nsegments
   call realloc(waqmor%fldval, newsize )
   waqmor%fldname(ifld) = newfld
   if (present(rval)) then
      j = 0
      nsegments2D = size(rval)
      do i = 1,waqmor%nsegments
         j = j+1
         if (j>nsegments2D) j = 1
         waqmor%fldval(ifld,i) = rval(j)
      enddo
   else
      waqmor%fldval(ifld,:) = 0.0
   endif
   waqmor%nfld = ifld
end subroutine addfld


subroutine addsubs(waqmor,newsubs,transp,subsmult)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    implicit none
!
! Call variables
!
    type (waqsimtype)                          :: waqmor
    character(*)               , intent(in)    :: newsubs
    logical                    , intent(in)    :: transp
    integer                    , intent(in)    :: subsmult
!    integer          , optional, intent(in)    :: ival
!    real(fp)         , optional, intent(in)    :: rval
!
! Local variables
!
    integer                                    :: isubs
!
!! executable statements -------------------------------------------------------
!
   isubs = waqmor%nsubs+1
   call realloc(waqmor%subsname,isubs)
   call realloc(waqmor%substran,isubs)
   call realloc(waqmor%subsmult,isubs)
   call realloc(waqmor%subsval,isubs)
   waqmor%subsname(isubs) = newsubs
   waqmor%substran(isubs) = transp
   waqmor%subsmult(isubs) = subsmult
   waqmor%subsval(isubs) = 0.0
   waqmor%nsubs = isubs
   if (transp) waqmor%nsubs_transp = waqmor%nsubs_transp + 1
end subroutine addsubs


subroutine addprocess(waqmor,newproc)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    implicit none
!
! Call variables
!
    type (waqsimtype)                          :: waqmor
    character(*)               , intent(in)    :: newproc
!    integer          , optional, intent(in)    :: ival
!    real(fp)         , optional, intent(in)    :: rval
!
! Local variables
!
    integer                                    :: iproc
!
!! executable statements -------------------------------------------------------
!
   iproc = waqmor%nprocess+1
   call realloc(waqmor%process,iproc)
   waqmor%process(iproc) = newproc
   waqmor%nprocess = iproc
end subroutine addprocess


subroutine addoutput(waqmor,newoutput)
!!--description-----------------------------------------------------------------
!
! Convert morphology ini properties to waq process definition
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use m_alloc
    implicit none
!
! Call variables
!
    type (waqsimtype)                          :: waqmor
    character(*)               , intent(in)    :: newoutput
!    integer          , optional, intent(in)    :: ival
!    real(fp)         , optional, intent(in)    :: rval
!
! Local variables
!
    integer                                    :: iout
!
!! executable statements -------------------------------------------------------
!
   iout = waqmor%noutput+1
   call realloc(waqmor%output,iout)
   waqmor%output(iout) = newoutput
   waqmor%noutput = iout
end subroutine addoutput

end module waqmor_module