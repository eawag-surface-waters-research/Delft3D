!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

!> Given time in seconds from refdat, fill dateandtime string
 !! NOTE: maketime and maketimeinverse are not compatible, because of minutes versus seconds, and different format string.
 subroutine maketime(dateandtime,tim)
 use m_flowtimes
 implicit none

 character,        intent(out) :: dateandtime*(*) !< Output datetime string, format '20000101_000000', note: includes seconds.
 double precision, intent(in)  :: tim             !< Input time in seconds since refdat.

 integer          :: iday, imonth, iyear, ihour, imin, isec

 dateandtime = '20000101_000000'
 ! TODO: AvD: maketime and maketimeinverse are now inconsistent since the addition of this '_'

 call datetime_from_refdat(tim, iyear, imonth, iday, ihour, imin, isec)

 write(dateandtime( 1:4 ),'(i4)')   iyear
 write(dateandtime( 5:6 ),'(i2.2)') imonth
 write(dateandtime( 7:8 ),'(i2.2)') iday
 write(dateandtime(10:11),'(i2.2)') ihour
 write(dateandtime(12:13),'(i2.2)') imin
 write(dateandtime(14:15),'(i2.2)') isec

 return
 end subroutine maketime
