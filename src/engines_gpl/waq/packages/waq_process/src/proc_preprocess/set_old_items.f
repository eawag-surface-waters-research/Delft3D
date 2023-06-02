!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!  
!  

      subroutine set_old_items( lurep , old_items, notot , nopa  , nofun    ,
     +                          nosfun, nodisp   , novelo, syname, paname   ,
     +                          funame, sfname   , diname, vename, constants)

!     Deltares Software Centre

!>/File
!>      rename items according the old_items table

      use m_zoek
      use m_srstop
      use timers         !< performance timers
      use dlwq_data      !< data definitions
      use processet      !< use processet definitions
      implicit none

      ! decalaration of arguments

      integer  ( 4), intent(in   ) :: lurep             !< unit number report file
      type(old_item_coll)          :: old_items         !< old_items table
      integer  ( 4), intent(in   ) :: notot             !< Number of systems
      integer  ( 4), intent(in   ) :: nopa              !< Number of parameters
      integer  ( 4), intent(in   ) :: nofun             !< Number of functions ( user )
      integer  ( 4), intent(in   ) :: nosfun            !< Number of segment functions
      integer  ( 4), intent(in   ) :: nodisp            !< Number of dispersion array's
      integer  ( 4), intent(in   ) :: novelo            !< Number of velocity array's
      character(20), intent(inout) :: syname(notot)     !< Systems names
      character(20), intent(inout) :: paname(nopa)      !< Parameter names
      character(20), intent(inout) :: funame(nofun )    !< Function names
      character(20), intent(inout) :: sfname(nosfun)    !< Segment function names
      character(20), intent(inout) :: diname(nodisp)    !< Dispersion array names
      character(20), intent(inout) :: vename(novelo)    !< Velocity array names
      type(t_dlwq_item)   , intent(inout) :: constants  !< delwaq constants list

      ! local declaration

      integer                   :: i                 ! loop counter old items
      integer                   :: n_old_items       ! number of records in old items table
      character(len=20)         :: name20            ! name
      integer                   :: ifound            ! index in array
      integer                   :: nocons            ! number of constants
      real                      :: range             ! range in range check
      integer                   :: ierr2             ! error indicator
      integer(4)                :: ithndl = 0        ! handle for performance timer
      if (timon) call timstrt( "set_old_items", ithndl )

      write (lurep,'(a)') '# scanning input for old process definitions'

      ! add processes based on old name

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_ADDPROC ) then
            name20 = 'active_'//old_items%old_items(i)%old_name
            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               nocons = constants%no_item + 1
               ierr2 = dlwq_resize(constants,nocons)
               if ( ierr2 .gt. 0 ) then
                  write(lurep,'(a,i10)') ' ERROR: set_old_items resize error constants size:',nocons
                  call srstop(1)
               endif
               constants%no_item          = nocons
               constants%name(nocons)     = 'active_'//old_items%old_items(i)%new_name
               constants%constant(nocons) = 1.0
               write(lurep,'(5a)') ' Added process [',old_items%old_items(i)%new_name,
     +                             '] based on activated process [',old_items%old_items(i)%old_name,']'
            endif
         endif
      enddo

      ! change default based on old process name, we do this later but we change the old_items record to ITEM_ACTION_DEFAULT

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_PROCDEF ) then
            name20 = 'active_'//old_items%old_items(i)%old_name
            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Replaced default [',old_items%old_items(i)%new_name,
     +                             '] based on activated process [',old_items%old_items(i)%old_name,']'
               old_items%old_items(i)%old_name    = old_items%old_items(i)%new_name
               old_items%old_items(i)%action_type = ITEM_ACTION_DEFAULT
               old_items%old_items(i)%serial      = 2100010101                           ! intoduce millenium bug
            endif
         endif
      enddo

      ! replace process names

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_PROCNAM ) then
            name20 = 'active_'//old_items%old_items(i)%old_name
            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               constants%name(ifound) = 'active_'//old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Process [',old_items%old_items(i)%new_name,
     +                             '] activated based on old name [',old_items%old_items(i)%old_name,']'
            endif
         endif
      enddo

      ! replace process parameter names ( including substance names ) with range check for constant value

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_RANGECHECK ) then

            name20 = old_items%old_items(i)%old_name
            range  = old_items%old_items(i)%old_default

            call zoek(name20, notot, syname, 20, ifound)
            if ( ifound .gt. 0 ) then
               syname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Substance name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
               write(lurep,'(a,g13.6)') ' WARNING no rangecheck possible for substance, range >',range
            endif

            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               if ( constants%constant(ifound) .ge. range ) then
                  constants%name(ifound) = old_items%old_items(i)%new_name
                  write(lurep,'(5a)') ' Constant name [',old_items%old_items(i)%old_name,
     +                                '] replace by new name [',old_items%old_items(i)%new_name,'] within range'
               else
                  write(lurep,'(5a)') ' Warning: constant name [',old_items%old_items(i)%old_name,
     +                                '] NOT replace by new name [',old_items%old_items(i)%new_name,'] range is not met'
               endif
            endif

            call zoek(name20, nopa  , paname, 20, ifound)
            if ( ifound .gt. 0 ) then
               paname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Parameter name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
               write(lurep,'(a,g13.6)') ' WARNING no rangecheck possible for parameter, range >',range
            endif

            call zoek(name20, nofun , funame, 20, ifound)
            if ( ifound .gt. 0 ) then
               funame(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Function name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
               write(lurep,'(a,g13.6)') ' WARNING no rangecheck possible for function, range >',range
            endif

            call zoek(name20, nosfun, sfname, 20, ifound)
            if ( ifound .gt. 0 ) then
               sfname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Segment function name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
               write(lurep,'(a,g13.6)') ' WARNING no rangecheck possible for segment function, range >',range
            endif

            call zoek(name20, nodisp, diname, 20, ifound)
            if ( ifound .gt. 0 ) then
               diname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Dispersion name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
               write(lurep,'(a,g13.6)') ' WARNING no rangecheck possible for dispersion, range >',range
            endif

            call zoek(name20, novelo, vename, 20, ifound)
            if ( ifound .gt. 0 ) then
               vename(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Velocity name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
               write(lurep,'(a,g13.6)') ' WARNING no rangecheck possible for velocity, range >',range
            endif

         endif
      enddo

      ! replace process parameter names ( including substance names )

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_PROCPAR ) then

            name20 = old_items%old_items(i)%old_name

            call zoek(name20, notot, syname, 20, ifound)
            if ( ifound .gt. 0 ) then
               syname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Substance name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               constants%name(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Constant name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

            call zoek(name20, nopa  , paname, 20, ifound)
            if ( ifound .gt. 0 ) then
               paname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Parameter name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

            call zoek(name20, nofun , funame, 20, ifound)
            if ( ifound .gt. 0 ) then
               funame(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Function name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

            call zoek(name20, nosfun, sfname, 20, ifound)
            if ( ifound .gt. 0 ) then
               sfname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Segment function name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

            call zoek(name20, nodisp, diname, 20, ifound)
            if ( ifound .gt. 0 ) then
               diname(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Dispersion name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

            call zoek(name20, novelo, vename, 20, ifound)
            if ( ifound .gt. 0 ) then
               vename(ifound) = old_items%old_items(i)%new_name
               write(lurep,'(5a)') ' Velocity name [',old_items%old_items(i)%old_name,
     +                             '] replace by new name [',old_items%old_items(i)%new_name,']'
            endif

         endif
      enddo

      ! point new name to the old name if new name not in input and old name is

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_PPEQUAL ) then

            name20 = old_items%old_items(i)%new_name

            call zoek(name20, notot, syname, 20, ifound)
            if ( ifound .le. 0 ) ifound = dlwq_find(constants,name20)
            if ( ifound .le. 0 ) call zoek(name20, nopa  , paname, 20, ifound)
            if ( ifound .le. 0 ) call zoek(name20, nofun , funame, 20, ifound)
            if ( ifound .le. 0 ) call zoek(name20, nosfun, sfname, 20, ifound)
            if ( ifound .le. 0 ) call zoek(name20, nodisp, diname, 20, ifound)
            if ( ifound .le. 0 ) call zoek(name20, novelo, vename, 20, ifound)

            if ( ifound .le. 0 ) then

               ! check if old name is in input

               name20 = old_items%old_items(i)%old_name

               call zoek(name20, notot, syname, 20, ifound)
               if ( ifound .le. 0 ) ifound = dlwq_find(constants,name20)
               if ( ifound .le. 0 ) call zoek(name20, nopa  , paname, 20, ifound)
               if ( ifound .le. 0 ) call zoek(name20, nofun , funame, 20, ifound)
               if ( ifound .le. 0 ) call zoek(name20, nosfun, sfname, 20, ifound)
               if ( ifound .le. 0 ) call zoek(name20, nodisp, diname, 20, ifound)
               if ( ifound .le. 0 ) call zoek(name20, novelo, vename, 20, ifound)

               ! set action to ITEM_ACTION_PPEQUAL2, the action itself is handled elsewhere

               if ( ifound .gt. 0 ) then
                  old_items%old_items(i)%action_type = ITEM_ACTION_PPEQUAL2
               endif

            endif

         endif
      enddo

      ! remarks, obsolete processes

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_REMARKPROC ) then

            name20 = 'active_'//old_items%old_items(i)%old_name
            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               write(lurep,'(3a,i10)') ' Activated process [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

         endif
      enddo

      ! remarks, obsolete process parameters

      n_old_items = old_items%cursize
      do i = 1, n_old_items
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_REMARKPAR ) then

            name20 = old_items%old_items(i)%old_name

            call zoek(name20, notot, syname, 20, ifound)
            if ( ifound .gt. 0 ) then
               write(lurep,'(3a,i10)') ' Substance name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

            ifound = dlwq_find(constants,name20)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Constant name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

            call zoek(name20, nopa  , paname, 20, ifound)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Parameter name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

            call zoek(name20, nofun , funame, 20, ifound)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Function name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

            call zoek(name20, nosfun, sfname, 20, ifound)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Segment function name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

            call zoek(name20, nodisp, diname, 20, ifound)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Dispersion name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

            call zoek(name20, novelo, vename, 20, ifound)
            if ( ifound .gt. 0 ) then
               write(lurep,'(5a)') ' Velocity name [',old_items%old_items(i)%old_name,
     +                             '] obsolete, see documentation remark no:',nint(old_items%old_items(i)%old_default)
            endif

         endif
      enddo

      if (timon) call timstop( ithndl )
      return
      end
