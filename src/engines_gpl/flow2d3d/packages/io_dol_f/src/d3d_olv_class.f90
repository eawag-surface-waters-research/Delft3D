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
!  $Id$
!  $HeadURL$
module d3d_olv_class

use precision

implicit none

type, public ::  olv_handle_t
    integer :: runningFlag = 0  ! 0=Flow not running (waiting); 1=Flow is iterating time steps
    integer :: currentStep = 0
    integer :: endTimeStep = 0
    integer :: endFlag     = 0  ! 0 = Flow simulation has not finished yet; 1 = Flow has finished
    integer :: timeStepInt = -1

    character(len=260)          :: runid
    integer, pointer            :: mmax
    integer, pointer            :: nmax
    integer, pointer            :: nlb, nub, mlb, mub, kmax
    integer, pointer            :: thick
    
    integer, pointer            :: ltur
    integer, pointer            :: lstsci
    integer, pointer            :: xcor
    integer, pointer            :: ycor
    integer, pointer            :: gvz
    integer, pointer            :: guz
    integer, pointer            :: xz
    integer, pointer            :: yz
    integer, pointer            :: kcs
    integer, pointer            :: kfs
    integer, pointer            :: kfu
    integer, pointer            :: kfv
    integer, pointer            :: kfsz1
    integer, pointer            :: alfas
    integer, pointer            :: s1
    integer, pointer            :: dp
    integer, pointer            :: dps
    integer, pointer            :: u1
    integer, pointer            :: v1
    integer, pointer            :: r1
    integer, pointer            :: rtur1
    integer, pointer            :: namcon
        
    real(fp), pointer           :: zbot
    real(fp), pointer           :: ztop
    logical                     :: zmodel
end type

type, public :: OLVHandle
    type(olv_handle_t), pointer :: fields => null()
end type

private

public new_olv
public free_olv
public isNull_olv

contains
!
!------------------------------------------------------------------------------
type(OLVHandle) function new_olv() result(res)

    type(olv_handle_t), pointer :: olv

    allocate( olv )

    olv%runid  = ''
    
    olv%mmax   => null()
    olv%nmax   => null()
    olv%nlb    => null()
    olv%nub    => null()
    olv%mlb    => null()
    olv%mub    => null()
    olv%kmax   => null()
    olv%thick  => null()
    
    olv%ltur   => null()
    olv%lstsci => null()
    olv%xcor   => null()
    olv%ycor   => null()
    olv%gvz    => null()
    olv%guz    => null()
    olv%xz     => null()
    olv%yz     => null()
    olv%kcs    => null()
    olv%kfs    => null()
    olv%kfu    => null()
    olv%kfv    => null()
    olv%kfsz1  => null()
    olv%alfas  => null()
    olv%s1     => null()
    olv%dp     => null()
    olv%dps    => null()
    olv%u1     => null()
    olv%v1     => null()
    olv%r1     => null()
    olv%rtur1  => null()
    olv%namcon => null()
    
    olv%zbot   => null()
    olv%ztop   => null()
    olv%zmodel = .false.

    res%fields => olv
end function
!
!------------------------------------------------------------------------------
subroutine free_olv(handle)
    type(OLVHandle) :: handle

    type(olv_handle_t), pointer :: olv

    if (isNull_olv(handle)) return

    olv => handle%fields

    if ( associated( olv%mmax   ) ) deallocate( olv%mmax   )
    if ( associated( olv%nmax   ) ) deallocate( olv%nmax   )
    if ( associated( olv%nlb    ) ) deallocate( olv%nlb    )
    if ( associated( olv%nub    ) ) deallocate( olv%nub    )
    if ( associated( olv%mlb    ) ) deallocate( olv%mlb    )
    if ( associated( olv%mub    ) ) deallocate( olv%mub    )
    if ( associated( olv%kmax   ) ) deallocate( olv%kmax   )
    if ( associated( olv%thick  ) ) deallocate( olv%thick  )

    if ( associated( olv%ltur   ) ) deallocate( olv%ltur   )
    if ( associated( olv%lstsci ) ) deallocate( olv%lstsci )
    if ( associated( olv%xcor   ) ) deallocate( olv%xcor   )
    if ( associated( olv%ycor   ) ) deallocate( olv%ycor   )
    if ( associated( olv%gvz    ) ) deallocate( olv%gvz    )
    if ( associated( olv%guz    ) ) deallocate( olv%guz    )
    if ( associated( olv%xz     ) ) deallocate( olv%xz     )
    if ( associated( olv%yz     ) ) deallocate( olv%yz     )
    if ( associated( olv%kcs    ) ) deallocate( olv%kcs    )
    if ( associated( olv%kfs    ) ) deallocate( olv%kfs    )
    if ( associated( olv%kfu    ) ) deallocate( olv%kfu    )
    if ( associated( olv%kfv    ) ) deallocate( olv%kfv    )
    if ( associated( olv%kfsz1  ) ) deallocate( olv%kfsz1  )
    if ( associated( olv%alfas  ) ) deallocate( olv%alfas  )
    if ( associated( olv%s1     ) ) deallocate( olv%s1     )
    if ( associated( olv%dp     ) ) deallocate( olv%dp     )
    if ( associated( olv%dps    ) ) deallocate( olv%dps    )
    if ( associated( olv%u1     ) ) deallocate( olv%u1     )
    if ( associated( olv%v1     ) ) deallocate( olv%v1     )
    if ( associated( olv%r1     ) ) deallocate( olv%r1     )
    if ( associated( olv%rtur1  ) ) deallocate( olv%rtur1  )
    if ( associated( olv%namcon ) ) deallocate( olv%namcon )
    
    if ( associated( olv%zbot   ) ) deallocate( olv%zbot   )
    if ( associated( olv%ztop   ) ) deallocate( olv%ztop   )

    olv%zmodel = .false.
    
    handle%fields => null()
end subroutine
!
!------------------------------------------------------------------------------
logical function isNull_olv(handle) result(res)
    type(OLVHandle) :: handle

    res = .NOT. associated(handle%fields)
end function
!
!------------------------------------------------------------------------------
end module
