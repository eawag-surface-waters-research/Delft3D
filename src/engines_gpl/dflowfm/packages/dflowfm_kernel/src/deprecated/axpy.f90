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

 !> speed test: compute z = Ax+y
 !> note: no communcation included in parallel runs, only test speed
 subroutine axpy(Mglob,Nglob)
    use m_partitioninfo
    use unstruc_messages
    use m_timer

    implicit none

    integer,                          intent(in)  :: Mglob  !> global size of x
    integer,                          intent(in)  :: Nglob  !> global size of y

    double precision, dimension(:),   allocatable :: x, y, z
    double precision, dimension(:,:), allocatable :: A

    integer                                       :: irun
    integer                                       :: ierror
    integer                                       :: M, N, i, j

    ierror = 1

!   compute array sizes based on number of subdomains
    M = Mglob
    N = Nglob

!   allocate
    allocate(x(M))
    allocate(y(N))
    allocate(z(N))
    allocate(A(N,M))

!   fill matrix A and vector x
     do j=1,M
        do i=1,N
          call random_number(A(i,j))
        end do
        call random_number(x(j))
      end do

    ! fill vecor y
    do i=1,N
        call random_number(y(i))
    end do

    call starttimer(IAXPY)

    ! z = Ax + y
    do i=1,N
       z(i) = y(i)
       do j=1,M
          z(i) = z(i) + A(i,j)*x(j)
       end do
    end do

    call stoptimer(IAXPY)

    ierror = 0

1234 continue

!   deallocate
    if ( allocated(x) ) deallocate(x)
    if ( allocated(y) ) deallocate(y)
    if ( allocated(A) ) deallocate(A)

 end subroutine axpy
