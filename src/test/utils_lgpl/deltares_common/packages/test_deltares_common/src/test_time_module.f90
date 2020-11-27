module test_time_module
   !----- LGPL --------------------------------------------------------------------
   !                                                                               
   !  Copyright (C)  Stichting Deltares, 2011-2020.                                
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
   use precision_basics, only : hp, comparereal
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
      end subroutine tests_time_module

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

end module test_time_module
