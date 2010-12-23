subroutine dfupdi ( field, ks, ke, gdp )
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
!   Updates field array of type integer through exchanging halo values
!   between neighbouring subdomains
!
!!--pseudo code and references--------------------------------------------------
!
!   for all neighbouring subdomains do
!      get subdomain number, pointer and size
!      store data to be sent in array WORK
!      send array WORK
!
!   for all neighbouring subdomains do
!      get subdomain number, pointer and size
!      receive next array and store in WORK
!      store the received data
!
!   Marcel.Zijlema@wldelft.nl
!   01 mar 07
!
!!--declarations----------------------------------------------------------------
    use dfparall
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, intent(in)                                            :: ke    ! last index in vertical direction
    integer, intent(in)                                            :: ks    ! first index in vertical direction
!
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,ks:ke), intent(inout) :: field ! real array for which halo values must
                                                                            ! be copied from neighbouring subdomains
!
! Local variables
!
    integer, dimension(:), pointer      :: iblkad
    integer                            :: idom   ! subdomain number
    integer                            :: inb    ! neighbour counter
    integer                            :: istart ! pointer in array IBLKAD
    integer                            :: itag   ! message tag for sending and receiving
    integer                            :: j      ! loop counter
    integer                            :: k      ! loop counter in vertical direction
    integer                            :: ksiz   ! size in vertical direction (e.g. total number of sigma layers)
    integer                            :: nneigh ! number of neighbouring subdomains
    integer                            :: novlu  ! number of overlapping unknowns
    integer                             :: worksize  ! 
    integer                             :: request(4,2)  ! 
    integer, dimension(:,:,:), allocatable :: work   ! work array to store data to be sent to or received from neighbour
!
!! executable statements -------------------------------------------------------
!
    iblkad => gdp%gdparall%iblkad
    !
    ksiz = ke - ks + 1
    !
    nneigh = iblkad(1)
    !
    worksize = ksiz*max(ihalom,ihalon)*max(gdp%d%mmax,gdp%d%nmax)
    allocate(work(worksize, 4, 2))
    !
    ! for all neighbouring subdomains do
    !
    itag = 2
    call dfsendi ( field, work, worksize, ks, ke, request, itag, gdp )
    call dfwaiti ( field, work, worksize, ks, ke, request, itag, gdp )
    !
    deallocate(work)

end subroutine dfupdi
