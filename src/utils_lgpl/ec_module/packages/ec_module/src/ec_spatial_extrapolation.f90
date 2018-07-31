!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2018.                                
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

!  $Id$
!  $HeadURL$

!> This module contains the spatial extrapolation methods for the EcConverter.
!! @author Sander.vanderPijl@deltares.nl
!! @author Edwin.Spee@deltares.nl
module m_ec_spatial_extrapolation
   use precision, only : hp
   use m_ec_typedefs, only : tEcField, tEcElementSet, tEcItem
   use m_ec_message, only : setECMessage
   use kdtree2Factory
   implicit none

   private

   public :: extrapolate_missing, set_max_search_radius, nearest_sample_wrapper

   real(kind=hp) :: max_search_radius = 3.0_hp   !< not used yet. always return nearest neighbor

   contains

   ! =======================================================================

       !> set the value of set_max_search_radius,
       !! only if given value is positive
       subroutine set_max_search_radius(value)
          real(kind=hp) :: value      !< new value for max_search_radius

          if (value > 0.0_hp) then
             max_search_radius = value
          endif
       end subroutine set_max_search_radius

!> extrapolate missing values
      subroutine extrapolate_missing(vals, Missing, jamissing)
         implicit none

         real(kind=hp), dimension(2,2,2,2), intent(inout) :: vals      !< values
         real(kind=hp),                     intent(in)    :: Missing   !< missing values
         integer,                           intent(out)   :: jamissing !< missing values left (1) or not (0)

         real(kind=hp)                                    :: value
         integer, dimension(4)                            :: imiss, jmiss
         integer                                          :: nummissing, n

         integer :: i, j, k, L
         integer :: i1, j1, i2, j2

         jamissing = 0

!        first two dimensions (horizontal)
         do L=1,2
            do k=1,2

!              count number of missing values
               nummissing = 0
               imiss = 0
               jmiss = 0
               n = 0
               do j=1,2
                  do i=1,2
                     if ( vals(i,j,k,L)==Missing ) then
                        nummissing = nummissing+1
                        imiss(nummissing) = i
                        jmiss(nummissing) = j
                     else
                        n = n+1
                        imiss(5-n) = i
                        jmiss(5-n) = j
                     end if
                  end do
               end do

               if ( nummissing == 1 ) then
!                 linear extrapolation
                  i = imiss(1)
                  j = jmiss(1)
                  i1 = 3-i
                  j1 = 3-j
                  vals(i,j,k,L) = vals(i,j1,k,L) + vals(i1,j,k,L) - vals(i1,j1,k,L)
               else if ( nummissing == 2 ) then
!                 linear extrapolation in one direction, constant in other
                  i1 = imiss(1)
                  j1 = jmiss(1)
                  i2 = imiss(2)
                  j2 = jmiss(2)
                  if ( i1 == i2 ) then
                     vals(i1,:,k,L) = vals(3-i1,:,k,L)
                  else if ( j1 == j2 ) then
                     vals(:,j1,k,L) = vals(:,3-j1,k,L)
                  else
                     vals(i1,j1,k,L) = 0.5d0*(vals(i2,j1,k,L)+vals(i1,j2,k,L))
                     vals(i2,j2,k,L) = vals(i1,j1,k,L)
                  end if
               else if ( nummissing == 3 ) then
                  i = imiss(4)
                  j = jmiss(4)
                  value = vals(i,j,k,L)
                  vals(:,:,k,L) = value
               else if ( nummissing == 4 ) then
!                 can not extrapolate
               end if
            end do
         end do
         
!        third dimension
         do L=1,2
            do j=1,2
               do i=1,2
                  if ( vals(i,j,1,L) == Missing .or. vals(i,j,2,L) == Missing ) then
                     if ( vals(i,j,1,L) == Missing .and. vals(i,j,2,L) /= Missing ) then
                        vals(i,j,1,L) = vals(i,j,2,L)
                     else if ( vals(i,j,2,L) == Missing .and. vals(i,j,1,L) /= Missing ) then
                        vals(i,j,2,L) = vals(i,j,1,L)
                     else
!                       can not extrapolate
                        jamissing = 1
                     end if
                  end if
               end do
            end do
         end do

      end subroutine extrapolate_missing

      !> search for nearest neighbor using a kdtree
      function nearest_sample_wrapper(kdtree, sourceItem, targetElementSet, targetElementID, field, jsferic, p, q) result(success)
         type(kdtree_instance), intent(inout) :: kdtree           !< kdtree instance
         type(tEcElementSet),   intent(in)    :: targetElementSet !< target element set
         type(tEcItem),         intent(in)    :: sourceItem       !< source item
         integer,               intent(in)    :: targetElementID  !< target element Id
         integer,               intent(in)    :: jsferic          !< flag is spherical (1) or not (0)
         real(kind=hp), target, intent(in)    :: field(:)         !< source field
         integer,               intent(out)   :: p                !< resulting m point
         integer,               intent(out)   :: q                !< resulting n point
         logical                              :: success          !< function result

         integer, parameter                 :: NN = 1 ! for the time being
         real(kind=hp)                      :: xk, yk, dmiss
         real(kind=hp), allocatable, target :: xs2d(:,:), ys2d(:,:)
         real(kind=hp), pointer             :: zs(:,:), xs(:), ys(:)
         integer                            :: Ns, Ndim, ierror, isam(NN), dim1, dim2, i, j, isam2(2,NN)

         xk = targetElementSet%x(targetElementID)
         yk = targetElementSet%y(targetElementID)
         dim1 = size(sourceItem%elementsetptr%x)
         dim2 = size(sourceItem%elementsetptr%y)
         Ns = dim1 * dim2
         Ndim = 1
         dmiss = sourceItem%sourcet0fieldptr%missingvalue
         zs(1:1, 1:Ns) => field
         if (kdtree%itreestat /= ITREE_OK) then
            allocate(xs2d(dim1,dim2), ys2d(dim1,dim2), stat=ierror)
            if (ierror /= 0) then
                call setECMessage("Allocate error in nearest_sample_wrapper with size ", 2*dim1*dim2)
                success = .false.
                return
            endif
            do i = 1, dim1
               do j = 1, dim2
                  xs2d(i,j) = sourceItem%elementsetPtr%x(i)
                  ys2d(i,j) = sourceItem%elementsetPtr%y(j)
               enddo
            enddo
            xs(1:Ns) => xs2d
            ys(1:Ns) => ys2d
         endif

         call find_nearest_sample_kdtree(kdtree, Ns, Ndim, xs, ys, zs, xk, yk, NN, isam, ierror, jsferic, dmiss)

         do i = 1, NN
             isam2(1,i) = mod(isam(i), dim1)
             isam2(2,i) = 1 + (isam(i)-1) / dim1
         enddo
         p = isam2(1,1)
         q = isam2(2,1)

         if (allocated(xs2d)) deallocate(xs2d)
         if (allocated(ys2d)) deallocate(ys2d)

         success = (ierror == 0)

      end function nearest_sample_wrapper

end module m_ec_spatial_extrapolation
