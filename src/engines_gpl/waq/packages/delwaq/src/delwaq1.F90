!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

subroutine delwaq1(argc, argv, errorcode)

    !DEC$ ATTRIBUTES DLLEXPORT::delwaq1

!>\file
!>                    DELWAQ - INPUT PROGRAMME
!>
!>                    Reads the DELWAQ inputfiles and generates
!>                    a consistent set of binairy intermediate files.

!     INFORMATION   : Deltares
!                     L. Postma,
!                     Rotterdamse weg 185,
!                     P.O. Box 177,
!                     2600 MH Delft,
!                     Netherlands.
!                     telephone (31) 15-569353
!                     telefax   (31) 15-619674
!
!     LOGICAL UNITS : LUN(29), output, formatted report file
!                     LUN( 1), output, binary common-block file
!                     LUN( 2), output, binary system file
!
!     SUBROUTINES CALLED :*UNLOCK, unlocks user dependent data
!                         *UNISET, reads input filename
!                          DLWQ01, reads block 1 of user data
!                          DLWQ02, reads block 2 of user data
!                          DLWQ03, reads block 3 of user data
!                          DLWQ04, reads block 4 of user data
!                          DLWQ05, reads block 5 of user data
!                          DLWQ06, reads block 6 of user data
!                          DLWQ07, reads block 7 of user data
!                          DLWQ7A, reads block 7 of user data new style
!                          DLWQ08, reads block 8 of user data
!                          DLWQ09, reads block 9 of user data
!                          DLWQS1, reads block 10 , statistical definition
!                          DLWQP1, proces pre-processor
!                          SPACE , computes space needed
!                          DLWQDI, writes dimensions of arrays for DELWAQ2
!                         *DHOPNF, opens files ( if neccesary )
!                         *SRSTOP, stops execution
!
!                         *, these routines can contain sytem dependencies
!
!
    use Grids        !   for the storage of contraction grids
    use dlwq_data    !   for definition and storage of data
    use Output       !   for the output names and pointers
    use timers       !   performance timers
    use dhcommand
    use m_delwaq1_data

    use D00SUB
    use ProcesSet
    use Workspace
    use Rd_token


      
    implicit none

    integer, intent(in)                           :: argc
    character(len=*), dimension(argc), intent(in) :: argv
    integer, intent(out)                          :: errorcode


    call delwaq1_init(argc, argv, errorcode)
    call delwaq1_unlock_username(argc, argv, errorcode)
    call delwaq1_allocate_workspace(argc, argv, errorcode)
    call delwaq1_read_user_data(argc, argv, errorcode)
    call delwaq1_write_messages(argc, argv, errorcode)
    call delwaq1_close_lunfiles(argc, argv, errorcode)

    ! Delwaq1_lib should never use a stop, but must be modified to return an error code instead (0 = normal end)
    ! Currently a return from the delwaq1_lib assumes a normal end.
    errorcode = 0
    return
end subroutine delwaq1
