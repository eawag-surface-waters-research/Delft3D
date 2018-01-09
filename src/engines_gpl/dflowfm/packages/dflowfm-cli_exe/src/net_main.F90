!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
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

! $Id: net_main.F90 52266 2017-09-02 11:24:11Z klecz_ml $
! $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/engines_gpl/dflowfm/packages/dflowfm-cli_exe/src/net_main.F90 $
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
!> @file net_main.F90
!! The main program 'network' and all net-related routines.
!! Flow-related routines are in unstruc.f90
!<

!> \mainpage Unstruc API docs
!! \li \b Main \b program: net.f90
!! \li \b Model \b setup: unstruc_model.f90
!! \li \b Network \b data: network.f90 + network_data.f90
!! \li \b Global \b data: modules.f90 (flow, geometry, times, parameters, ...)
!! \li \b GUI \b and \b network \b algorithms: net.f90
!! \li \b Unstructured \b flow \b solver: unstruc.f90
!! \li \b Matrix \b solver: solve_guus.f90
!! \li \b Stations \b and \b cross-sections: monitoring.f90
!! \li \b Various \b helpers: REST.F90 unstruc_ini.f90, unstruc_files.f90, unstruc_startup.f90, unstruc_display.f90, rcm.f90
!! \li \b RGFgrid \b routines: RGFSTUFF.f90
!! \li \b NetCDF-IO: unstruc_netcdf.f90
!! \li \b WAQ-output: waq.f90, wrwaq.f90
!! \li \b More: see file list
   PROGRAM unstruc
   use unstruc_startup
   use unstruc_model
   use unstruc_messages
   USE UNSTRUC_DISPLAY
   use unstruc_api
   use network_data, only: network_ini 
   use dfm_error   
   use m_partitioninfo
#ifdef HAVE_MPI
   use mpi
#endif
   
   implicit none

   integer :: MODE,NFLD, KEY
   integer :: JQN
   integer :: JDEMO
   integer :: finalizeProgram

   COMMON /MODENOW/ MODE,NFLD
   COMMON /QNRGF/ JQN
   COMMON /DEMO/ JDEMO
   integer :: ierr, lastmode, IDUM
   LOGICAL :: JAWEL

   integer, external         :: read_commandline
   integer, external         :: flow_modelinit

#if HAVE_DISPLAY==0
! For dflowfm-cli executable, switch off all GUI calls here at *runtime*,
! by setting jaGUI = 0.
! All kernel code does not need to be recompiled, because there is only
! two places that preprocess HAVE_DISPLAY at *compiletime*, all the rest
! is done by `if (jaGUI .. )`
   jaGUI = 0          !< GUI (1) or not (0)
#endif

#ifdef HAVE_MPI
   ! From calling C/C++ side, construct an MPI communicator, and call
   ! MPI_Fint MPI_Comm_c2f(MPI_Comm comm) to convert the C comm handle
   ! to a FORTRAN comm handle.
   call mpi_init(ierr)
   call mpi_comm_rank(DFM_COMM_DFMWORLD,my_rank,ierr)
   call mpi_comm_size(DFM_COMM_DFMWORLD,numranks,ierr)
   ja_mpi_init_by_fm = 1

   if ( numranks.le.1 ) then
      jampi = 0
   end if
   
!  make domain number string as soon as possible
   write(sdmn, '(I4.4)') my_rank

#else
   numranks=1
#endif

   JDEMO        = 0
   JQN          = 2
   
   call network_ini() ! initialize of the network variables
 
    md_jaopenGL = -1 ! no commandline option read for OpenGL (yet)
  
    ierr = read_commandline()
    select case(ierr)
    case (DFM_NOERR)
       continue
    case (DFM_EXIT) ! Harmless exit, no error (e.g. --help)
       goto 1234
    case default    ! Nonzero error code. Return.
       goto 1234
    end select
    

!   set jaopengl from commandline option (if available)
    call iset_jaopengl(md_jaopengl)
    CALL START()  ! start of the actual initialisation for the program
    call resetFullFlowModel()
    CALL INIDAT()
    CALL RESETB(0)
    

#ifdef HAVE_PETSC
      call startpetsc()
#endif
  
    MODE = 1
    lastmode = 1
    NFLD = 1
    KEY  = 3
    
    call processCommandLineOptions(finalizeProgram)
    
    if (finalizeProgram.eq.1) then
        goto 1234
    endif

    if ( jagui.eq.1 .and. len_trim(md_cfgfile).gt.0 ) then
       call load_displaysettings(md_cfgfile)
    end if

   
    if (len_trim(md_ident) > 0) then
        ! An MDU file was read.
        md_findcells = 0  ! try to bypass findcells
        ierr = flow_modelinit()
        if ( ierr /= DFM_NOERR ) goto 1234  ! error: finalize and stop
      
        if ( jaGUI.eq.1 .and. len_trim(md_cfgfile).eq.0 ) then
           inquire (file = trim(md_ident)//'.cfg', exist = jawel)
           if (jawel) then 
              call load_displaysettings(trim(md_ident)//'.cfg')
           endif
   
           CALL DRAWNU(KEY) ! Draw model for the first time
        end if

        if (md_jaAutoStart == MD_AUTOSTART .or. md_jaAutoStart >= MD_AUTOSTARTSTOP) then
           if (md_jaAutoStart > MD_AUTOSTARTSTOP) ntek = 0
           idum = FLOW()
           ! TODO: check whether all data (net/s1) is available before starting flow. [AvD]
           if (idum == DFM_SIGINT .or. md_jaAutoStart >= MD_AUTOSTARTSTOP) then
              goto 1234   ! finalize and stop
           end if
        end if
    end if
    
   if ( jaGUI.eq.1 ) then
       call editGui(MODE, NFLD,KEY,lastmode)
   end if
   
1234 continue

!  finalize before exit
   call partition_finalize()
   
   end program unstruc