#include "global_config.inc"
module m_depfil_stm
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
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
!!--description----------------------------------------------------------------- 
! 
!    Function: (Wrapper) Reads the depth values from the attribute file 
!    Either calls the familiar depfil routine, or an interpolator from the
!    EC-module for extended file type and unstructured grid support.
! 
!-------------------------------------------------------------------------------

contains
subroutine depfil_stm(lundia    ,error     ,fildep    ,fmttmp    , &
                    & array     ,nfld      ,ifld      ,dims      , &
                    & errmsg    )
    use precision
    use grid_dimens_module
#if MOR_USE_ECMODULE
    use m_ec_module ! NOTE: AvD: ds trunk cannot use this yet, as m_ec_module is still in branches/research/Deltares/20130912_12819_EC-module/
    use m_ec_filereader_read, only: ecSampleReadAll
    use m_ec_basic_interpolation, only: triinterp2
#endif
    ! 
    implicit none 
! 
! Global variables 
! 
    type(griddimtype), target                                          , intent(in)  :: dims   !  grid dimensions
    integer                                                            , intent(in)  :: ifld   !  index of field to be read
    integer                                                                          :: lundia !  unit number for diagnostic file
    integer                                                            , intent(in)  :: nfld   !  number of fields
    logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    real(fp), dimension(nfld, dims%nlb:dims%nub, dims%mlb:dims%mub)    , intent(out) :: array  !  data array to fill
    character(*)                                             , optional, intent(out) :: errmsg !  Error message in case of error
    character(*)                                                       , intent(in)  :: fildep !  Name of the relevant file 
    character(11)                                                      , intent(in)  :: fmttmp !  Format switch for the attribute file 
! 
! Local variables 
! 
!!#if MOR_USE_ECMODULE
 real(hp), allocatable :: xs(:), ys(:), zs(:,:)
 real(hp) :: xpl(1), ypl(1), zpl(1)
 real(hp) :: transformcoef(25)
 integer  :: minp0, jdla
 integer  :: ibnd
 integer  :: nm
 integer  :: nm2
 logical  :: success
 real(hp) :: dmiss    = -999.0_hp
 integer  :: ns, kx
 integer  :: npl
 integer  :: jsferic, jasfer3D, jins
 transformcoef = 0.0_hp
!!#endif
! 
!! executable statements ------------------------------------------------------- 
! 
   error = .false.
   if (present(errmsg)) errmsg = ' '
! MOR_USE_ECMODULE macro used from global_config.h to enable/disable EC-module for space-varying input in sed/mor.
#if MOR_USE_ECMODULE
   !
   ! This part only works if fp==hp !!
   !
   ! some call to timespaceinitialfield in EC module
   ! TODO: AvD: test code below now works via EC module, but still needs some inconvenient additional 'dummy' arguments. Consider further refactoring.
   open (newunit=minp0, file = fildep, form = fmttmp, status = 'old') 
   success = ecSampleReadAll(minp0, fildep, xs, ys, zs, ns, kx)

   jdla = 1
   jsferic = 0
   jasfer3D = 0
   jins = 1
   NPL = 0 ! Dummies, since STM is not aware of these yet.

   array(ifld, :, :) = dmiss

   CALL triinterp2(dims%xz, dims%yz, array(ifld, :, :), dims%nmmax, jdla, & 
                   XS, YS, ZS(1,:), NS, dmiss, jsferic, jins, jasfer3D, NPL, 0, 0, XPL, YPL, ZPL, transformcoef)

   ! mirror boundary cells if undefined if equal to dmiss
   do ibnd = 1, size(dims%nmbnd,1)  ! loop over boundary flow links (TO DO: what about 3D?)
      nm  = dims%nmbnd(ibnd,1)      ! point outside net
      nm2 = dims%nmbnd(ibnd,2)      ! point inside net
      if (array(ifld, nm, 1) == dmiss) then
          array(ifld, nm, 1) = array(ifld, nm2, 1)
      endif 
   enddo   
   
   ! if sample still equal to dmiss (values are not defined on flow nodes) - throw error
   do nm = 1, size(array,1)  ! loop over flow nodes
      if (array(ifld, nm, 1) == dmiss) then
          error = .true.
          if (present(errmsg)) errmsg = 'Error reading samples (not covering full grid) ' // trim(fildep) //' .'
      endif
   enddo    
   close(minp0)

   ! success = timespaceinitialfield(dims%xz, dims%yz, array(ifld, :, :), dims%nmmax, fildep, 7, 5,  'O', transformcoef, 1) ! zie meteo module
   continue
#else
    call depfil(lundia    ,error     ,fildep    ,fmttmp    , &
              & array     ,nfld      ,ifld      ,dims      )
    if (present(errmsg)) errmsg = 'Error reading QUICKIN file '//trim(fildep)
#endif
end subroutine depfil_stm


subroutine depfil_stm_double(lundia    ,error     ,fildep    ,fmttmp    , &
                           & array     ,nfld      ,ifld      ,dims      , &
                           & errmsg    )
    use precision 
    use grid_dimens_module
#if MOR_USE_ECMODULE
    use m_ec_module ! NOTE: AvD: ds trunk cannot use this yet, as m_ec_module is still in branches/research/Deltares/20130912_12819_EC-module/
    use m_ec_basic_interpolation, only: triinterp2
    use m_ec_filereader_read, only: ecSampleReadAll
#endif
!    use timespace ! TODO: AvD: replace by EC location and remove current temp extra include path
    ! 
    implicit none 
! 
! Global variables 
! 
    type(griddimtype), target                                          , intent(in)  :: dims   !  grid dimensions
    integer                                                            , intent(in)  :: ifld   !  index of field to be read
    integer                                                                          :: lundia !  unit number for diagnostic file
    integer                                                            , intent(in)  :: nfld   !  number of fields
    logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    real(hp), dimension(nfld, dims%nlb:dims%nub, dims%mlb:dims%mub)    , intent(out) :: array  !  data array to fill
    character(*)                                             , optional, intent(out) :: errmsg !  Error message in case of error
    character(*)                                                       , intent(in)  :: fildep !  Name of the relevant file 
    character(11)                                                      , intent(in)  :: fmttmp !  Format switch for the attribute file 
! 
! Local variables 
! 
!!#if MOR_USE_ECMODULE
 real(hp), allocatable :: xs(:), ys(:), zs(:,:)
 real(hp) :: xpl(1), ypl(1), zpl(1)
 real(hp) :: transformcoef(25)
 integer  :: minp0, jdla
 integer  :: ibnd
 integer  :: nm
 integer  :: nm2
 logical  :: success
 real(hp) :: dmiss    = -999.0_hp
 integer  :: ns, kx
 integer  :: npl
 integer  :: jsferic, jasfer3D, jins
 transformcoef = 0.0_hp
!!#endif
! 
!! executable statements ------------------------------------------------------- 
! 
   error = .false.
   if (present(errmsg)) errmsg = ' '
! MOR_USE_ECMODULE macro used from global_config.h to enable/disable EC-module for space-varying input in sed/mor.
#if MOR_USE_ECMODULE
   ! some call to timespaceinitialfield in EC module
   ! TODO: AvD: test code below now works via EC module, but still needs some inconvenient additional 'dummy' arguments. Consider further refactoring.
   open (newunit=minp0, file = fildep, form = fmttmp, status = 'old') 
   success = ecSampleReadAll(minp0, fildep, xs, ys, zs, ns, kx)

   jdla = 1
   jsferic = 0
   jasfer3D = 0
   jins = 1
   NPL = 0 ! Dummies, since STM is not aware of these yet.

   array(ifld, :, :) = dmiss

   CALL triinterp2(dims%xz, dims%yz, array(ifld, :, :), dims%nmmax, jdla, & 
                   XS, YS, ZS(1,:), NS, dmiss, jsferic, jins, jasfer3D, NPL, 0, 0, XPL, YPL, ZPL, transformcoef)

   ! mirror boundary cells if undefined if equal to dmiss
   do ibnd = 1, size(dims%nmbnd,1)  ! loop over boundary flow links (TO DO: what about 3D?)
      nm  = dims%nmbnd(ibnd,1)      ! point outside net
      nm2 = dims%nmbnd(ibnd,2)      ! point inside net
      if (array(ifld, nm, 1) == dmiss) then
          array(ifld, nm, 1) = array(ifld, nm2, 1)
      endif 
   enddo
   
   ! if sample still equal to dmiss (values are not defined on flow nodes) - throw error
   do nm = 1, size(array,1)  ! loop over flow nodes
      if (array(ifld, nm, 1) == dmiss) then
          error = .true.
          if (present(errmsg)) errmsg = 'Error reading samples (not covering full grid) ' // trim(fildep) //' .'
      endif    
   enddo    
   close(minp0)

   ! success = timespaceinitialfield(dims%xz, dims%yz, array(ifld, :, :), dims%nmmax, fildep, 7, 5,  'O', transformcoef, 1) ! zie meteo module
   continue
#else
    call depfil_double(lundia    ,error     ,fildep    ,fmttmp    , &
                     & array     ,nfld      ,ifld      ,dims      )
    if (present(errmsg)) errmsg = 'Error reading QUICKIN file '//trim(fildep)
#endif
end subroutine depfil_stm_double

end module m_depfil_stm
