!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation; either                 
!  version 2.1 of the License.                                                  
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
!-------------------------------------------------------------------------------
!   Delft-FSM (Fortran Shared Memory)
!   Test Program
!
!   Reads lines for a file called "input" that specify an FSM operation.
!   There are four fields: operation, data type, length and key name.
!   The operations are carried out in the order of the lines, and for each
!   operation a result line is written to a file called "output".  When the
!   input is exhausted, the key table is printed (to stdout).
!
!   An input file and associated output file is provided ({input,output}.test)
!   in the test directory.  When comparing output to reference output, the
!   numerical values of the pointers may differ across machines.
!
!   Irv.Elshoff@deltares.nl
!   7 oct 05
!
!-------------------------------------------------------------------------------


program test
    implicit none
    include 'fsm.i'
    
    integer iostat
    integer line
    character operation*6
    integer type
    integer length
    character name*20
    integer status

    write (*,*) 'FSM Test Program #1 Starting (reading from "input")'

    !status = fsmini (0, FSM_SILENT)
    status = fsmini (0, FSM_TRACE)
    call fsmtrf ("trace.out")

    open (10, file='input',  iostat=iostat, status='old', form='formatted')
    if (iostat /= 0) then
        write (*,*) 'Cannot open input file'
        stop
    endif

    open (11, file='output', iostat=iostat, form='formatted')
    if (iostat /= 0) then
        write (*,*) 'Cannot open output file'
        stop
    endif

    line = 0
    do
        line = line + 1
        read (10, '(a6,1x,i8,1x,i8,1x,a8)', iostat=iostat) operation, type, length, name
        if (iostat < 0) then
            exit
        else if (iostat > 0) then
            write (*,*) 'Invalid record in input file on line ', line
            stop
        endif

        if (operation == 'makptr') then
            call makepointer (11, name, type, length)
        elseif (operation == 'getptr') then
            call getpointer (11, name)
        elseif (operation == 'relptr') then
            call releasepointer (11, name)
        else
            write (*,*) 'Error: Unknown operation in input:', operation, 'on line ', line
        endif
    enddo

    status = prtkey ()
    write (*,*) 'FSM Test Program #1 Finished (results in "output")'
end


!-------------------------------------------------------------------------------


subroutine makepointer (unit, keyname, type, length)
    implicit none
    integer unit
    character keyname*(*)
    integer type
    integer length

    include 'fsm.i'

    integer result
    character message*1000
    
    result = MAKPTR (keyname, type, length)
    if (result == 0) then
        result = FSMERR (message)
        write (unit,*) 'MAKPTR ', keyname, 'Error: ', message(1:len_trim(message))
    else
        write (unit,*) 'MAKPTR ', keyname, 'OK => ', result
    endif
end

subroutine getpointer (unit, keyname)
    implicit none
    integer unit
    character keyname*(*)

    include 'fsm.i'

    integer result
    character message*1000
    
    result = GETPTR (keyname)
    if (result == 0) then
        result = FSMERR (message)
        write (unit,*) 'GETPTR ', keyname, 'Error: ', message(1:len_trim(message))
    else
        write (unit,*) 'GETPTR ', keyname, 'OK => ', result
    endif
end

subroutine releasepointer (unit, keyname)
    implicit none
    integer unit
    character keyname*(*)

    include 'fsm.i'

    integer result
    character message*1000
    
    result = RELPTR (keyname)
    if (result == 0) then
        result = FSMERR (message)
        write (unit,*) 'RELPTR ', keyname, 'Error: ', message(1:len_trim(message))
    else
        write (unit,*) 'RELPTR ', keyname, 'OK => ', result
    endif
end
