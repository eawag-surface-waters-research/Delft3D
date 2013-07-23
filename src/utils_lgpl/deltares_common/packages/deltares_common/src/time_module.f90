module time_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!!--description-----------------------------------------------------------------
!
!    Function: - Various time processing routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

private

!
! functions and subroutines
!
public time_module_info
public datetime2sec
public sec2ddhhmmss
public ymd2jul
public jul2ymd

contains



! ------------------------------------------------------------------------------
!   Subroutine: time_module_info
!   Purpose:    Add info about this time module to the messages stack
!   Summary:    Add id string and URL
!   Arguments:
!   messages    Stack of messages to add the info to
! ------------------------------------------------------------------------------
subroutine time_module_info(messages)
    use message_module
    !
    ! Call variables
    !
    type(message_stack), pointer :: messages
    !
    !! executable statements ---------------------------------------------------
    !
    call addmessage(messages,'$Id $')
    call addmessage(messages,'$URL$')
end subroutine time_module_info



! ------------------------------------------------------------------------------
!   Subroutine: datetime2sec
!   Purpose:    Convert a 6 integer datetime vector to a number of seconds
!   Arguments:
!   datetime    Integer array of length 6 containing date yr,mo,dy and time hr,mn,sc
!   refdatetime Optional integer array of length 6 containing reference date yr,mo,dy and time hr,mn,sc
!   sec         If refdatetime is specified: time difference in seconds
!               Otherwise: time in seconds since julday = 0 (only to be used for time steps)
! ------------------------------------------------------------------------------
function datetime2sec(datetime, refdatetime) result (sec)
    implicit none
    !
    ! Call variables
    !
    integer, dimension(6)           , intent(in)    :: datetime
    integer, dimension(6) , optional, intent(in)    :: refdatetime
    integer                                         :: sec
    !
    ! Local variables
    !
    integer :: jd    ! Julian date
    integer :: refjd ! Julian refernce date
    !
    !! executable statements ---------------------------------------------------
    !
    jd    = ymd2jul(datetime(1), datetime(2), datetime(3))
    !
    if (present(refdatetime)) then
       refjd = ymd2jul(refdatetime(1), refdatetime(2), refdatetime(3))
       ! difference in days
       sec   = jd - refjd
       ! difference in hours
       sec   = sec*24 + datetime(4) - refdatetime(4)
       ! difference in minutes
       sec   = sec*60 + datetime(5) - refdatetime(5)
       ! difference in seconds
       sec   = sec*60 + datetime(6) - refdatetime(6)
    else
       ! time in hours
       sec   = jd*24 + datetime(4)
       ! time in minutes
       sec   = sec*60 + datetime(5)
       ! time in seconds
       sec   = sec*60 + datetime(6)
    endif
end function datetime2sec



! ------------------------------------------------------------------------------
!   Subroutine: sec2ddhhmmss
!   Purpose:    Convert a number of seconds to ddhhmmss integer
!   Arguments:
!   sec         Number of seconds
!   ddhhmmss    Integer with days, hours, minutes and seconds formatted as ddhhmmss 
! ------------------------------------------------------------------------------
function sec2ddhhmmss(sec) result (ddhhmmss)
    implicit none
    !
    ! Call variables
    !
    integer                         , intent(in)    :: sec
    integer                                         :: ddhhmmss
    !
    ! Local variables
    !
    integer :: dd ! days
    integer :: hh ! hours
    integer :: mm ! minutes
    integer :: ss ! seconds
    !
    !! executable statements ---------------------------------------------------
    !
    ! time in seconds
    dd = sec
    ss = mod(dd,60)
    ! time in minutes
    dd = (dd - ss)/60
    mm = mod(dd,60)
    ! time in hours
    dd = (dd - mm)/60
    hh = mod(dd,24)
    ! time in days
    dd = (dd - hh)/24
    !
    ddhhmmss = ss + 100*mm + 10000*hh + 1000000*dd
end function sec2ddhhmmss



! ------------------------------------------------------------------------------
!   Subroutine: ymd2jul
!   Purpose:    Convert a year, month, day triplet into a Julian date number
!   Arguments:
!   year        Year
!   month       Month number
!   day         Day
!   julday      Julian date
! ------------------------------------------------------------------------------
function ymd2jul(year, month, day) result (julday)
    implicit none
!
! Global variables
!
    integer , intent(in)             :: year   !  Year or YYYYMMDD integer
    integer , intent(in) , optional  :: month  !  Month
    integer , intent(in) , optional  :: day    !  Day
    integer                          :: julday !  Julian day number
!
!
! Local variables
!
    integer               :: d      ! Help var. to check calculation 
    integer               :: m      ! Help var. to check calculation 
    integer               :: y      ! Help var. to check calculation 
    integer               :: lday   ! Local day
    integer               :: lmonth ! Local month number
    integer               :: lyear  ! Local year
    integer               :: month1 ! Help variable
!
!! executable statements -------------------------------------------------------
!
    !
    if (present(month) .and. present(day)) then
       lyear  = year
       lmonth = month
       lday   = day
    elseif (.not.present(month) .and. .not.present(day)) then
       lyear  = year/10000
       lmonth = year/100 - lyear*100
       lday   = year - lmonth*100 - lyear*10000
    else
       stop 'Improper use of YMD2JUL'
    endif
    !
    ! Calculate Julian day assuming the given month is correct
    !
    month1 = (lmonth - 14)/12
    julday = lday - 32075 + 1461*(lyear + 4800 + month1)/4 + 367*(lmonth - 2 - month1*12) &
           & /12 - 3*((lyear + 4900 + month1)/100)/4
    !
    ! Calculate backwards to test if the assumption is correct
    !
    call jul2ymd(julday, y, m, d)
    !
    ! Test if calculating is correct
    !
    if ((y /= lyear) .or. (m /= lmonth) .or. (d /= lday)) then
       julday = 0
    endif
end function ymd2jul



! ------------------------------------------------------------------------------
!   Subroutine: jul2ymd
!   Purpose:    Convert a Julian date number in a year, month, day triplet
!   Arguments:
!   julday      Julian date
!   year        Year or YYYYMMDD integer
!   month       Month number
!   day         Day
! ------------------------------------------------------------------------------
subroutine jul2ymd(julday, year, month, day)
    implicit none
!
! Global variables
!
    integer , intent(in)             :: julday !  Julian day number
    integer , intent(out)            :: year   !  Year
    integer , intent(out) , optional :: month  !  Month
    integer , intent(out) , optional :: day    !  Day
!
! Local variables
!
    integer               :: j      ! Help variable
    integer               :: l      ! Help variable
    integer               :: m      ! Help variable
    integer               :: n      ! Help variable
    integer               :: lday   ! Local day
    integer               :: lmonth ! Local month number
    integer               :: lyear  ! Local year
!
!! executable statements -------------------------------------------------------
!
    l      = julday + 68569
    n      = 4 * l / 146097
    l      = l - (146097*n + 3)/4
    j      = 4000 * (l + 1) / 1461001
    l      = l - 1461*j/4 + 31
    m      = 80 * l / 2447
    lday   = l - 2447*m/80
    l      = m / 11
    lmonth = m + 2 - 12*l
    lyear  = 100*(n-49) + j + l
    !
    if (present(month) .and. present(day)) then
       year  = lyear
       month = lmonth
       day   = lday
    elseif (.not.present(month) .and. .not.present(day)) then
       year  = lyear*10000 + lmonth*100 + lday
    else
       stop 'Improper use of JUL2YMD'
    endif
end subroutine jul2ymd


end module time_module
