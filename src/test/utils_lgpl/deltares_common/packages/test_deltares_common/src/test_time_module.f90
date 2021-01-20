module test_time_module
   !----- LGPL --------------------------------------------------------------------
   !                                                                               
   !  Copyright (C)  Stichting Deltares, 2011-2021.                                
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
   !    Function: - Tests for various time and date processing routines
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision_basics, only : hp
   use time_module
   use ftnunit
   implicit none

   private

   real(kind=hp), parameter :: tol = 1d-9

   public :: tests_time_module

   contains

      subroutine tests_time_module
         call test( testconversion1,  'Test CalendarYearMonthDayToJulianDateNumber' )
         call test( testconversion2,  'Test julian' )
         call test( test_split_date_time, 'Test split_date_time' )
         call test( test_split_date_time_invalid, 'Test split_date_time with invalid input')
         call test( test_parse_time_valid, 'Test parse_time with valid input')
         call test( test_parse_time_invalid, 'Test parse_time with invalid input')
      end subroutine tests_time_module

      !> test CalendarYearMonthDayToJulianDateNumber
      subroutine testConversion1()

         integer :: jdn1, jdn2, jdn3, jdn4, yyyymmdd, yyyymmdd2, istat
         real(kind=hp) :: mjd, mjd2

         jdn1 = CalendarYearMonthDayToJulianDateNumber(1, 1, 1)
         jdn2 = CalendarYearMonthDayToJulianDateNumber(1582, 10, 4)
         jdn3 = CalendarYearMonthDayToJulianDateNumber(1582, 10, 15)
         jdn4 = CalendarYearMonthDayToJulianDateNumber(2001, 1, 1)

         call assert_equal(jdn1, 1721424, 'error for 1-1-1')
         call assert_equal(jdn2, 2299160, 'error for 4-10-1582')
         call assert_equal(jdn3, 2299161, 'error for 15-10-1582')
         call assert_equal(jdn4, 2451911, 'error for 1-1-2001')

         mjd = 55833.5_hp
         istat = mjd2date(mjd, yyyymmdd)
         mjd2 = date2mjd(yyyymmdd)
         call assert_comparable (mjd, mjd2, tol, 'error for mjd=55833.5')

         yyyymmdd = 15821015
         mjd = date2mjd(yyyymmdd)
         istat = mjd2date(mjd, yyyymmdd2)
         call assert_equal (yyyymmdd, yyyymmdd2, 'error for yyyymmdd = 15821015')

         yyyymmdd = 15821004
         mjd2 = date2mjd(yyyymmdd)
         istat = mjd2date(mjd2, yyyymmdd2)
         call assert_equal (istat, 1, 'mjd2date failed for yyyymmdd = 15821004')
         call assert_equal (yyyymmdd, yyyymmdd2, 'error for yyyymmdd = 15821004')
         call assert_false (abs(mjd - mjd2 - 1d0) > 1d-4, 'error for jump 15821004 -> 15821015')

      end subroutine testConversion1

      !> test julian
      subroutine testConversion2()

         real(kind=hp) :: jdn1, jdn2, jdn3, jdn4

         jdn1 = julian(00010101, 0)
         jdn2 = julian(15821004, 0)
         jdn3 = julian(15821015, 0)
         jdn4 = julian(20010101, 0)

         call assert_comparable(jdn1, -678577.0_hp, tol, 'error for 1-1-1')
         call assert_comparable(jdn2, -100841.0_hp, tol, 'error for 4-10-1582')
         call assert_comparable(jdn3, -100840.0_hp, tol, 'error for 15-10-1582')
         call assert_comparable(jdn4, 51910.0_hp, tol, 'error for 1-1-2001')

      end subroutine testConversion2

      !> test split_date_time with valid input
      subroutine test_split_date_time
         character(len=16) :: date, time, tz
         character(len=26) :: date_time
         logical           :: success
         character(len=26), parameter :: date_times (9) = (/ &
            "2010-04-04 00:07:02       ", &
            "2011-05-05T01:08:03       ", &
            "2012-06-06 02:09:04 +02:00", &
            "2013-07-07T03:10:05 +01:00", &
            "2014-08-08 04:11:06 -01:00", &
            "2015-09-09T05:12:07 -02:00", &
            "2016-10-10 06:13:08Z      ", &
            "2017-11-11T07:14:09Z      ", &
            "2018-12-12                " /)
         integer :: i

         do i = 1, size(date_times)
            date_time = date_times(i)
            success = split_date_time(date_time, date, time, tz)
            call assert_true(success, 'success for ' // date_time)
            call assert_equal(date, date_time(1:10), 'diff. in date for ' // date_time)
            call assert_equal(time, date_time(12:19), 'diff. in time for ' // date_time)
            call assert_equal(tz, adjustl(date_time(20:)), 'diff. in time zone for ' // date_time)
         end do

         date_time = "2014-08-08 04:11:06.123"  ! longer time: has a fractial number of seconds
         success = split_date_time(date_time, date, time, tz)
         call assert_true(success, 'success for ' // date_time)
         call assert_equal(date, date_time(1:10), 'diff. in date for ' // date_time)
         call assert_equal(time, date_time(12:), 'diff. in time for ' // date_time)
         call assert_equal(tz, ' ', 'diff. in time zone for ' // date_time)

      end subroutine test_split_date_time

      !> test split_date_time with invalid input
      subroutine test_split_date_time_invalid
         character(len=16) :: date, time, tz
         character(len=26) :: date_time
         logical           :: success
         character(len=26), parameter :: date_times (3) = (/ &
            "2010-04                   ", &  ! date to short
            "2011-05-05t01:08:03       ", &  ! t as separator (must be upper case)
            "2012-06-06 02:09:04  02:00" /)  ! no sign in time zone
         integer :: i

         do i = 1, size(date_times)
            date_time = date_times(i)
            success = split_date_time(date_time, date, time, tz)
            call assert_false(success, 'unexpected success for ' // date_time)
         end do

      end subroutine test_split_date_time_invalid

      !> test parse time with valid input
      subroutine test_parse_time_valid
         integer, parameter           :: nr_cases = 7
         character(len=16), parameter :: times(nr_cases) = (/ &
            "01:02:03        ", &
            "04:05:06.7      ", &
            "07:47:33        ", &
            "8:10:20.3       ", &    ! one digit for hour and with fractional seconds (is allowed)
            "8:10:20         ", &    ! one digit for hour (is allowed)
            "123456          ", &    ! no splitters
            "014554.44       "/)     ! no splitter, but with fractional seconds
         real(kind=hp), parameter :: fraction_expected(nr_cases) = (/ 0.0430902777778_hp, 0.170216435185_hp, 0.3246875_hp, &
                                                                      0.340512731481_hp, 0.340509259259_hp, 0.524259259259_hp, &
                                                                      0.073546759259_hp /)
         integer                  :: i
         logical                  :: ok
         real(kind=hp)            :: fraction

         do i = 1, nr_cases
            fraction = parse_time(times(i), ok)
            call assert_true( ok, "unexpected success status (must succeed)")
            call assert_comparable(fraction, fraction_expected(i), tol, "difference in fraction")
         end do
      end subroutine test_parse_time_valid

      !> test parse time
      !! both valid and invalid input
      subroutine test_parse_time_invalid
         character(len=20), parameter :: times(9) = (/ &
            "                    ", &    ! no time at all
            "25:00:00            ", &    ! invalid hh
            "23:65:28            ", &    ! invalid mm
            "12:00:99            ", &    ! invalid ss
            "12:22:206577.31415  ", &    ! invalid ss combined with fractional seconds
            "250000              ", &    ! invalid hh, without ':'
            "236528              ", &    ! invalid mm, without ':'
            "120099              ", &    ! invalid ss, without ':'
            "1222206577.31415    " /)    ! invalid ss, without ':', combined with fractional seconds
         integer                  :: i
         logical                  :: ok
         real(kind=hp)            :: fraction

         do i = 1, size(times)
            fraction = parse_time(times(i), ok)
            call assert_false( ok, "unexpected success status (must fail)")
         end do
      end subroutine test_parse_time_invalid

end module test_time_module
