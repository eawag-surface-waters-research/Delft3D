!!  Copyright (C)  Stichting Deltares, 2012-2021.
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

!!  *********************************************************************
!!  *    Modules containing all the functions for PROTIST                *
!!  *********************************************************************

!!   contains the derived types and associated functions:
!!    - protist_allocate
!!              allocate derived types 
!!    - protist_initialize
!!              initialize derived types 
!!    - protist_deallocate
!!              deallocate derived types 
  
  

!!    
    
module protist_types

    implicit none

    ! Protist arrays
    type :: protist_array
        ! prey state variables
        real, dimension(:), allocatable :: preyC, preyChl, preyN, preyP, preySi 
        ! other prey input parameters
        real, dimension(:), allocatable :: CcellPrey, rPrey, motPrey, PR
        ! food quantity
        real, dimension(:), allocatable :: preyFlag                       ! protection aginst small prey conc. 
        real, dimension(:), allocatable :: nrPrey                         ! prey abundance
        real, dimension(:), allocatable :: smallerVel, largerVel          ! velocities
        real, dimension(:), allocatable :: encPrey                        ! prey encounter
        real, dimension(:), allocatable :: capturedPrey                   ! prey capture
        real, dimension(:), allocatable :: propPrey, ingNC, ingPC         ! preyN and preyP proprtion in diet    
        ! ingestion of prey by predator fluxes
        real, dimension(:), allocatable :: dPreyC, dPreyChl, dPreyN, dPreyP, dPreySi
    end type
    
    
    contains
    
    ! allocate arrays
    subroutine allocate_prot_array(prot_array,maxNrPr)
        type(protist_array), intent(inout)  :: prot_array
        integer, intent(in)                 :: maxNrPr
        
        ! allocate statements
        allocate( prot_array%preyC(maxNrPr)        )    
        allocate( prot_array%preyChl(maxNrPr)      )  
        allocate( prot_array%preyN(maxNrPr)        )    
        allocate( prot_array%preyP(maxNrPr)        )    
        allocate( prot_array%preySi(maxNrPr)       )   
        allocate( prot_array%CcellPrey(maxNrPr)    )
        allocate( prot_array%rPrey(maxNrPr)        )    
        allocate( prot_array%motPrey(maxNrPr)      )  
        allocate( prot_array%PR(maxNrPr)           )       
    
        ! allocation of food quantity and quality arrays
        allocate( prot_array%nrPrey(maxNrPr)       )
        allocate( prot_array%preyFlag(maxNrPr)     )
        allocate( prot_array%smallerVel(maxNrPr)   )
        allocate( prot_array%largerVel(maxNrPr)    )
        allocate( prot_array%encPrey(maxNrPr)      )
        allocate( prot_array%capturedPrey(maxNrPr) )
        allocate( prot_array%propPrey(maxNrPr)     )
        allocate( prot_array%ingNC(maxNrPr)        )
        allocate( prot_array%ingPC(maxNrPr)        )
        allocate( prot_array%dPreyC(maxNrPr)       )  
        allocate( prot_array%dPreyChl(maxNrPr)     )
        allocate( prot_array%dPreyN(maxNrPr)       )  
        allocate( prot_array%dPreyP(maxNrPr)       )  
        allocate( prot_array%dPreySi(maxNrPr)      ) 
    
    end subroutine allocate_prot_array
   
    
    ! initialize arrays
    subroutine initialize_prot_array(prot_array,maxNrPr, PMSA, plen, ipnt, nrSpInd, maxNrSp, nrSpCon, iSpec, nrSp_par)
        type(protist_array), intent(inout)  :: prot_array
        integer, intent(in)                 :: maxNrPr
        integer, intent(in)                 :: ipnt(:)
        integer, intent(in)                 :: plen
        integer, intent(in)                 :: maxNrSp, nrSpCon, nrSpInd, iSpec, nrSp_par
        real(4)                             :: pmsa(plen)
        integer     iPrey


        do iPrey = 0, (maxNrPr - 1)
            !prey specific input
            ! independentItems + all input items of all zoo species + first prey item + current PreyNumber * total nr of prey specific items
            prot_array%preyC(iPrey + 1)         = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 1 + iPrey * nrSp_par))   !      C-biomass                                              (gC m-3)   
            prot_array%preyChl(iPrey + 1)       = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 2 + iPrey * nrSp_par))   !      Chl-biomass                                            (gC m-3)  
            prot_array%preyN(iPrey + 1)         = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 3 + iPrey * nrSp_par))   !      N-biomass                                              (gN m-3)  
            prot_array%preyP(iPrey + 1)         = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 4 + iPrey * nrSp_par))   !      P-biomass                                              (gP m-3)  
            prot_array%preySi(iPrey + 1)        = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 5 + iPrey * nrSp_par))   !      Si-biomass                                             (gP m-3)  
            prot_array%CcellPrey(iPrey + 1)     = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 6 + iPrey * nrSp_par))   !      C content of protist cell                              (pgC cell-1) 
            prot_array%rPrey(iPrey + 1)         = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 7 + iPrey * nrSp_par))   !      radius of nutrient repleted protist cell               (um)
            prot_array%motPrey(iPrey + 1)       = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 8 + iPrey * nrSp_par))   !      swimming velocity                                      (m s-1)
            prot_array%PR(iPrey + 1)            = PMSA(ipnt( nrSpInd + maxNrSp * nrSpCon + 9 + iSpec + iPrey * nrSp_par))   !      handling index of prey 1 by pred 1             (dl)  
            
            
            ! if loop to protect against small preys [dl]
            if (prot_array%preyC(iPrey + 1) >= 1.0E-5) then 
                prot_array%preyFlag(iPrey + 1) = 1.0
            else 
                prot_array%preyFlag(iPrey + 1) = 0.0
            end if 
                            
        end do
    end subroutine initialize_prot_array
    
    
    
    
    ! deallocate arrays
    subroutine deallocate_prot_array(prot_array)
        type(protist_array), intent(inout)  :: prot_array
        
        ! deallocate statements
        deallocate( prot_array%preyC        )    
        deallocate( prot_array%preyChl      )  
        deallocate( prot_array%preyN        )    
        deallocate( prot_array%preyP        )    
        deallocate( prot_array%preySi       )   
        deallocate( prot_array%CcellPrey    )
        deallocate( prot_array%rPrey        )    
        deallocate( prot_array%motPrey      )  
        deallocate( prot_array%PR           )       
    
        ! deallocation of food quantity and quality arrays
        deallocate( prot_array%nrPrey       )
        deallocate( prot_array%preyFlag     )
        deallocate( prot_array%smallerVel   )
        deallocate( prot_array%largerVel    )
        deallocate( prot_array%encPrey      )
        deallocate( prot_array%capturedPrey )
        deallocate( prot_array%propPrey     )
        deallocate( prot_array%ingNC        )
        deallocate( prot_array%ingPC        )
        deallocate( prot_array%dPreyC       )  
        deallocate( prot_array%dPreyChl     )
        deallocate( prot_array%dPreyN       )  
        deallocate( prot_array%dPreyP       )  
        deallocate( prot_array%dPreySi      ) 
    
    end subroutine deallocate_prot_array
    

end module protist_types