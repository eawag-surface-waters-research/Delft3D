module wave_data
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
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
! Module parameters
!
!
! Mode options:
integer, parameter :: stand_alone     = 0
integer, parameter :: flow_online     = 1
integer, parameter :: flow_mud_online = 2
!
! FlowVelocityType options:
integer, parameter :: FVT_SURFACE_LAYER  = 1
integer, parameter :: FVT_DEPTH_AVERAGED = 2
integer, parameter :: FVT_WAVE_DEPENDENT = 3
!
! Whitecapping options:
integer, parameter :: WC_OFF        = 0
integer, parameter :: WC_KOMEN      = 1
integer, parameter :: WC_WESTHUYSEN = 2
!
! Module types
!
type wave_time_type
   integer  :: refdate         ! [yyyymmdd] reference date, reference time is 0:00 h
   integer  :: timtscale       ! [tscale]   Current time of simulation since reference date (0:00h)
   real     :: tscale          ! [sec]      Basic time unit: default = 60.0,
                               ! when running online with FLOW tscale = FLOW_time_step
   real     :: timsec          ! [sec]      Current time of simulation since reference date (0:00h)
   real     :: timmin          ! [min]      Current time of simulation since reference date (0:00h)
end type wave_time_type
!
type wave_output_type
   integer  :: count           ! [-]        Counts the number of generated output fields
   real     :: nexttim         ! [sec]      Next time to write to wavm-file
   real     :: nextint         ! [sec]      Next time to write to wavm-file   
   logical  :: write_wavm      ! [y/n]      True when writing to wavm file
   logical  :: keephotfile     ! [y/n]      True when writing to wavm file   
end type wave_output_type
!
type wave_data_type
   integer                :: mode
   type(wave_time_type)   :: time
   type(wave_output_type) :: output
end type wave_data_type
!
! Module parameters
!
! arch is currently 'win32' or 'linux'
!
character(10) :: arch
!
!
!
contains
!
!
!===============================================================================
subroutine initialize_wavedata(wavedata)
   type(wave_data_type) :: wavedata
   character(30)        :: txthlp

   wavedata%mode                   =  0
   wavedata%time%refdate           =  0
   wavedata%time%timtscale         =  0
   wavedata%time%tscale            = 60.0
   wavedata%time%timsec            =  0.0
   wavedata%time%timmin            =  0.0
   wavedata%output%count           =  0
   wavedata%output%nexttim         =  0.0
   wavedata%output%write_wavm      =  .false.
   wavedata%output%keephotfile     =  .false.   
   wavedata%output%nextint         =  0.0   
   !
   ! platform definition
   !
   call util_getenv('ARCH',txthlp)
   call small(txthlp,999)
   if (txthlp == 'win32' .or. txthlp == 'w32') then
      arch = 'win32'
   else
      arch = 'linux'
   endif
end subroutine initialize_wavedata
!
!
!===============================================================================
subroutine setmode(wavedata, mode_in)
   integer :: mode_in
   type(wave_data_type) :: wavedata

   wavedata%mode = mode_in
end subroutine setmode
!
!
!===============================================================================
subroutine setrefdate(wavetime, refdate_in)
   integer :: refdate_in
   type(wave_time_type) :: wavetime

   wavetime%refdate = refdate_in
end subroutine setrefdate
!
!
!===============================================================================
subroutine settimtscale(wavetime, timtscale_in)
   integer :: timtscale_in
   type(wave_time_type) :: wavetime

   wavetime%timtscale = timtscale_in
   wavetime%timsec    = real(wavetime%timtscale) * wavetime%tscale
   wavetime%timmin    = wavetime%timsec / 60.0
end subroutine settimtscale
!
!
!===============================================================================
subroutine settscale(wavetime, tscale_in)
   real :: tscale_in
   type(wave_time_type) :: wavetime

   wavetime%tscale    = tscale_in
   wavetime%timsec    = real(wavetime%timtscale) * wavetime%tscale
   wavetime%timmin    = wavetime%timsec / 60.0
end subroutine settscale
!
!
!===============================================================================
subroutine settimsec(wavetime, timsec_in)
   real :: timsec_in
   type(wave_time_type) :: wavetime

   wavetime%timsec    = timsec_in
   wavetime%timmin    = wavetime%timsec / 60.0
   wavetime%timtscale = nint(wavetime%timsec / wavetime%tscale)
end subroutine settimsec
!
!
!===============================================================================
subroutine settimmin(wavetime, timmin_in)
   real :: timmin_in
   type(wave_time_type) :: wavetime

   wavetime%timmin    = timmin_in
   wavetime%timsec    = wavetime%timmin * 60.0
   wavetime%timtscale = nint(wavetime%timsec / wavetime%tscale)
end subroutine settimmin
!
!
!===============================================================================
subroutine setoutputcount(waveoutput, count_in)
   integer :: count_in
   type(wave_output_type) :: waveoutput

   waveoutput%count = count_in
end subroutine setoutputcount
!
!
!===============================================================================
subroutine setnexttim(waveoutput, nexttim_in)
   real :: nexttim_in
   type(wave_output_type) :: waveoutput

   waveoutput%nexttim = nexttim_in
end subroutine setnexttim
!
!
!===============================================================================
subroutine setwrite_wavm(waveoutput, write_in)
   logical :: write_in
   type(wave_output_type) :: waveoutput

   waveoutput%write_wavm = write_in
end subroutine setwrite_wavm
!
!
!===============================================================================
subroutine setkeep_hotfile(waveoutput, keep_in)
   logical :: keep_in
   type(wave_output_type) :: waveoutput

   waveoutput%keephotfile = keep_in
end subroutine setkeep_hotfile
!
!
!===============================================================================
subroutine setnextint(waveoutput, nextint_in)
   real :: nextint_in
   type(wave_output_type) :: waveoutput

   waveoutput%nextint = nextint_in
end subroutine setnextint



end module wave_data
