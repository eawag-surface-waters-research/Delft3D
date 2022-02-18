   subroutine run_swan (casl)
   !----- GPL ---------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2011-2022.
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
   !!--description-----------------------------------------------------------------
   !
   !     *** Run swan; produce output file swanout with values on     ***
   !     *** swan computational grid                                  ***
   !
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use swan_input
   use wave_mpi, only: numranks, wave_mpi_bcast, wave_mpi_barrier, engine_comm_world, MPI_SUCCESS, SWAN_GO
   use system_utils, only: SCRIPT_EXTENSION
   !
   implicit none
   !
   ! Global variables
   !
   integer                 :: ierr
   integer                 :: ind
   integer                 :: strlen
   integer                 :: ncasl
   integer                 :: nuerr
   integer                 :: count
   integer                 :: dia
   integer                 :: ic
   integer                 :: k
   integer                 :: ln1
   integer                 :: ln2
   integer                 :: uh
   integer                 :: iSize
   logical                 :: ex
   character(*)            :: casl
   character(4)            :: labl
   character(4)            :: copy
   character(9)            :: prints
   character(6)            :: append
   character(8)            :: swninp
   character(256)          :: wvsswn
   character(256)          :: string
   character(256)          :: fname
   character(1024)         :: swanCommand
   character(150)           :: line
   !
   !! executable statements -------------------------------------------------------
   !
   wvsswn = ' '
   labl   = ' '
   swninp = 'swan.inp'
   write (wvsswn, '(a)') trim(casl)
   ind    = index( wvsswn, ' ')
   ncasl  = ind - 1
   if (ncasl == 0) then
      ncasl = 3
   endif
   wvsswn(ind:) = '.swn'
   copy = 'copy'
   call cp_file( swninp, wvsswn, copy, nuerr)
   if (nuerr > 0) then
      write (*, '(3a,i3)') '*** ERROR: While copying swan.inp to ',trim(wvsswn),', errorcode:', nuerr
      call wavestop(1, '*** ERROR: While copying swan.inp to <case>.swn')
   endif
   !
   ! SWAN execution
   !
   if (swan_run%exemode == SWAN_MODE_LIB) then
      !
      ! As built-in function.
      !
      if (swan_run%scriptname /= ' ') then
         !
         ! The name of a preprocessing script has been specified. Call it.
         ! The script will generate 'INPUT' based on 'swan.inp'.
         !
         write (*,'(2a)') 'Preprocessing SWAN input file using: ', trim(swan_run%scriptname)
         write(swanCommand, '(3a)') trim(swan_run%scriptname), ' ', trim(casl)
         !
         ! In debug mode, util_system wants spaces at the end...
         !
         call util_system(swanCommand(1:len_trim(swanCommand)+5))
         !
         inquire (file = 'INPUT', exist = ex)
         if (.not. ex) then
            write (*,'(3a)') '*** ERROR: preprocessing script "', trim(swan_run%scriptname), '" ran without generating ''INPUT'' file'
            call wavestop(1, '*** ERROR: preprocessing script ran without generating ''INPUT'' file')
         endif
      else
         !
         ! No preprocessing script specified.
         ! Copy input file directly to INPUT.
         !
         call cp_file( swninp, 'INPUT', copy, nuerr)
         if (nuerr > 0) then
            write (*, '(a,i3)') '*** ERROR: While copying swan.inp to INPUT, errorcode:', nuerr
            call wavestop(1, '*** ERROR: While copying swan.inp to INPUT')
         endif
      endif
      !
      write(*,'(a)')'>>...Start SWAN run'
      if (numranks > 1) then
         call wave_mpi_bcast(SWAN_GO, ierr)
         if ( ierr == MPI_SUCCESS ) then
            call swan(engine_comm_world)
            call wave_mpi_barrier(ierr)
         endif
         if ( ierr /= MPI_SUCCESS ) then
            write (*,'(a,i5)') 'MPI produces some internal error - return code is ',ierr
            call wavestop(1, '*** ERROR: MPI produced an internal error')
         endif
      else
         call swan(engine_comm_world)
      endif
   else
      !
      ! As executable
      !
      write(*,'(3a)')'>>...Check file swan_',SCRIPT_EXTENSION(2:),'.log'
      write(swanCommand, '(4a)') 'swan', SCRIPT_EXTENSION, ' ', trim(casl)
      !
      ! In debug mode, util_system wants spaces at the end...
      !
      call util_system(swanCommand(1:len_trim(swanCommand)+5))
      !
      inquire (file = 'norm_end', exist = ex)
      if (.not. ex .and. SCRIPT_EXTENSION=='.sh') then
         write (*,'(a)') '*** WARNING: unable to run SWAN using "swan.sh". Trying with "swan.bat" ...'
         write(swanCommand,'(3a)') 'swan.bat', ' ', trim(casl)
         !
         ! In debug mode, util_system wants spaces at the end...
         !
         call util_system(swanCommand(1:len_trim(swanCommand)+5))
      endif
   endif
   write(*,'(a)')'>>...End of SWAN run'
   !
   ! Check SWAN output file norm_end
   !
   inquire (file = 'norm_end', exist = ex)
   if (.not. ex) then
      write (*,'(a)') '*** ERROR: file ''norm_end'' expected to signal a correct SWAN calculation'
      call wavestop(1, '*** ERROR: file ''norm_end'' expected to signal a correct SWAN calculation')
   endif
   !
   ! Check SWAN output file PRINT
   !
   string(1:9)  = 'swn-diag.'
   string(10:)  = casl
   ind = index(string, ' ')
   string(ind:) = labl
   append       = 'append'
   !
   ! Serial run
   inquire (file = 'PRINT', exist = ex, size=iSize)
   !
   ! size argument is needed because MPI runs create empty PRINT files
   if (ex .and. iSize>0) then
      call scan_fl(swan_run%checkVersionNumber, swan_run%versionNumberOK,'PRINT')
      prints        = 'PRINT'
      call cp_file(prints     ,string    ,append    ,nuerr             )
      if (nuerr > 0) then
         write (*, '(a,i3)') '*** ERROR: While appending PRINT to diag file: ', nuerr
      endif
   else
      ! MPI run
      count=0
      call scan_fl(swan_run%checkVersionNumber, swan_run%versionNumberOK, 'PRINT-001')
      do
         count=count+1
         write (fname,'(a,i3.3)') 'PRINT-', count
         inquire (file = trim(fname), exist = ex)
         if (ex) then
            if (count==1) then
               open (newunit=dia,file=trim(string),form = 'formatted', position = 'append')
               open (newunit = uh, file = trim(fname), form = 'FORMATTED')
               ln1=0
               do
                  ln1=ln1+1
                  read (uh, '(A)', end=100) line
                  k = index(line, 'HOTF') + index(line,'STOP')
                  if (k>0) then
                     exit
                  endif 
               enddo
               rewind(uh)
               do ic=1,ln1-3
                  read(uh,'(a)')line
                  write(dia,'(a)')trim(line)
               enddo   
100            continue 
               close (dia)
               close(uh)
            else
               ! Search for ** WARNING : Differences in wave height at the boundary
               ! end with HOTF or STOP
               ! Rest of the file is equal to PRINT-001
               open (newunit = uh, file = trim(fname), form = 'FORMATTED')
               ln1=0;ln2=0
               do
                  ln1=ln1+1
                  read (uh, '(A)', end=200) line
                  call small(line, 80)
                  k   = index(line, 'warning : differences')
                  if (k>0) then
                     exit
                  endif
               enddo
               if (k==0) then
                  goto 200        ! no spurious bnd results, nothing to add to PRINT file that is not contained in PRINT-001
               endif
               !
               rewind(uh)
               do
                  k=0
                  read (uh, '(A)',end=200) line
                  ln2=ln2+1
                  call small(line,80)
                  k   = index(line, 'hotf')+index(line, 'stop')
                  if (k>0) exit
               enddo
200            continue
               if (ln1>=ln2) then
                  goto 300
               endif
               !
               ! append to swn-diag file
               open(newunit=dia,file=trim(string),form = 'formatted', position = 'append')
               rewind(uh)
               do ic=1,ln1+4
                  read(uh,'(a)')line
               end do
               do ic=1,ln2-ln1-4
                  read(uh,'(a)')line
                  write(dia,'(a)')trim(line)
               end do
               close (dia)
300            continue
               close (uh)
            endif
         else
            ! went through all PRINT-xxx files
            exit
         endif
      enddo
   endif
   !
   ! Remove SWAN input/output/tmp files (except SWANOUT output data file)
   !
   call rm_del('norm_end')
   call rm_del('INPUT')
   call rm_del('PRINT')
   count=0
   do
      count=count+1
      write(fname,'(a,i3.3)')'PRINT-',count
      inquire(file=trim(fname),exist=ex)
      if (ex) then
         call rm_del(trim(fname))
      else
         exit
      endif
   enddo
   call rm_del('source')
   call rm_del('temp')
   call rm_del('swaninit')
   call rm_del('INSTR')
   call rm_del('INSTU')
   call rm_del('BOTNOW')
   call rm_del('CURNOW')
   call rm_del('AICENOW')
   string = casl(1:ncasl) // '.swn'
   call rm_del(string)
   string = casl(1:ncasl) // '.prt'
   call rm_del(string)
   string = 'fname'
   call rm_del(string)
   end subroutine run_swan
