!!  Copyright (C)  Stichting Deltares, 2012-2019.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!
!  *********************************************************************
!  *    SUBROUTINE TO CHECK FEASIBILITY OF NUTRIENT CONSTRAINTS        *
!  *********************************************************************
!
      subroutine nutfea(infeas)
      
      use bloom_data_dim
      use bloom_data_io  
      use bloom_data_matrix  
      use bloom_data_phyt    

      implicit none

!      include 'blmdim.inc'
!      include 'phyt1.inc'
!      include 'phyt2.inc'
!      include 'matri.inc'
!      include 'ioblck.inc'

      real(8)   :: x(mx), slack = 1.d-12
      integer   :: i, j, infeas
!
!  If a negative righthand side is determined, check whther all species
!  have a positive A-coefficient or not; introduce slack to avoid
!  negative concentrations.
!
      infeas=0
      do 40 i=1,nunuco
      if (b(i) .ge. 0.0) go to 40
      do 10 j=1,nuspec
      if (aa(i,j) .lt. 1.0d-6) go to 20
   10 continue
      if (idump .eq. 1) write (iou(6),99999) cstra(i),b(i)
      infeas=1
      go to 30
   20 continue
      if (idump .eq. 1) write (iou(6),99990) cstra(i),b(i)
   30 b(i)=slack
   40 continue
99999 format (2X,'The nutrient constraint',1X,A8,1X,'has a negative',
     1        ' right hand side =',2X,F8.3,/,1X,'and positive',
     2        ' A-coefficients for all species; problem is infeasible.')
99990 format (2X,'The nutrient constraint',1X,A8,1X,'has a negative',
     1        ' right hand side =',2X,F8.3,/,1X,'and zero',
     2        ' A-coefficients for at least one species;',
     3        ' slack introduced.')
      return
      end
