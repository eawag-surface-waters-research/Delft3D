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

! $Id$
! $HeadURL$
module unstruc_boundaries
implicit none

integer, parameter :: max_registered_item_id = 128    
integer            :: max_ext_bnd_items      = 64  ! Starting size, will grow dynamically when needed.
character(len=max_registered_item_id), allocatable :: registered_items(:)
integer            :: num_registered_items = 0

contains

subroutine findexternalboundarypoints()             ! find external boundary points
 use m_netw
 use m_flow, filetype_hide => filetype               ! Two stages: 1 = collect elsets for which data is provided
 use m_flowgeom                                      !             2 = add relations between elsets and their providers
 use unstruc_model                                   ! This routine is based upon the network admin only,
 use timespace                                       ! not on the flow admin.
 use m_sferic
 use m_alloc
 use unstruc_messages
 use m_ship
 use properties
 use m_ec_magic_number
 use m_transport
 use m_sobekdfm
 use m_sediment
 use m_partitioninfo

 implicit none

 character(len=256)    :: filename
 integer               :: filetype
 integer, allocatable  :: kce(:)             ! kc edges (numl)
 integer, allocatable  :: ke(:)              ! kc edges (numl)
 logical               :: jawel
 integer               :: ja_ext_force
 logical               :: ext_force_bnd_used
 integer               :: ierr, method
 double precision      :: return_time
 integer               :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf
 integer               :: nx
 integer               :: ierror
 integer               :: num_bc_ini_blocks
 integer               :: ifrac
 character(len=64)     :: varname

 jatimespace = 1 

 return_time = 0
 ja_ext_force = 0
 ext_force_bnd_used = .false.

 filetype = poly_tim
 if (len(trim(md_extfile)) > 0) then
    inquire (file = trim(md_extfile), exist = jawel)
    if (jawel) then
       if (mext > 0) then
          ! Close first, if left open after prior flow_geominit().
          ! NOTE: AvD: this if-check relies on the fact that mext is *not* set to 0 in default_flowexternalforcings(), when reinitializing an already initialized model.
          call doclose(mext)
       end if

       call oldfil(mext,md_extfile)
       ja_ext_force = 1
    else
       call qnerror( 'External forcing file '''//trim(md_extfile)//''' not found.', '  ', ' ')
       write(msgbuf, '(a,a,a)') 'External forcing file ''', trim(md_extfile), ''' not found.'
       call err_flush()
    endif
 endif
 if (len(trim(md_extfile_new)) > 0) then
    inquire (file = trim(md_extfile_new), exist = jawel)
    if (jawel) then
       ext_force_bnd_used = .true.
    else
       call qnerror( 'Boundary external forcing file '''//trim(md_extfile_new)//''' not found.', '  ', ' ')
       write(msgbuf, '(a,a,a)') 'Boundary external forcing file ''', trim(md_extfile_new), ''' not found.'
       call err_flush()
    endif
 endif
 
! if (ja_ext_force == 0 .and. .not. ext_force_bnd_used) then
!    return
! endif 

 if ( allocated (xe) ) deallocate(xe, ye, xyen)     ! centre points of all net links, also needed for opening closed boundaries

 !mx1Dend = 0                                        ! count MAX nr of 1D endpoints
 !do L = 1,numl1D
 !   if ( kn(3,L) == 1) then                         ! zeker weten
 !      k1 = kn(1,L) ; k2 = kn(2,L)
 !      if (nmk(k1) == 1 .and. nmk(k2) == 2 .and. lne(1,L) < 0 .or. &
 !          nmk(k2) == 1 .and. nmk(k1) == 2 .and. lne(2,L) < 0 ) then
 !          mx1Dend = mx1Dend + 1
 !      endif
 !   endif
 !enddo
 !
 !
 !nx = numl + mx1Dend

! count number of 2D links and 1D endpoints
 call count_links(mx1Dend, Nx)


 allocate ( xe (nx) ,     stat=ierr ) ; xe = 0      ! used in findexternalboundarypoints
 call aerr('xe (nx)',     ierr, nx)
 allocate ( ye (nx) ,     stat=ierr ) ; ye = 0
 call aerr('ye (nx)',     ierr, nx)
 allocate ( xyen(2, nx) , stat=ierr ) ; xyen = 0d0
 call aerr('xyen(2, nx)', ierr, nx)

                                                    ! some temp arrays

 if (allocated(kez)) then
    ! If flow_geominit was called separately from a flow_modelinit:
    deallocate (             kez,     keu,     kes,     ketm,     kesd,     keuxy,     ket,     ken,     ke1d2d,     keg,     ked,     kep, kedb, keklep,     kegs,     kegen,     itpez,     itpenz,     itpeu,      itpenu,     kew)
 end if
 if (allocated(ftpet) ) then
    deallocate(ftpet)
 end if
 allocate ( kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx), kew(nx), ftpet(nx), stat=ierr )
 call aerr('kce(nx), ke(nx), kez(nx), keu(nx), kes(nx), ketm(nx), kesd(nx), keuxy(nx), ket(nx), ken(nx), ke1d2d(nx), keg(nx), ked(nx), kep(nx), kedb(nx), keklep(nx), kegs(nx), kegen(nx), itpez(nx), itpenz(nx), itpeu(nx) , itpenu(nx) , kew(nx), ftpet(nx)',ierr, 17*nx)
            kce = 0; ke = 0; kez = 0; keu = 0; kes = 0; ketm = 0; kesd = 0; keuxy = 0; ket = 0; ken = 0; ke1d2d = 0; keg = 0; ked = 0; kep=  0; kedb=0; keklep=0; kegen= 0; itpez = 0; itpenz = 0; itpeu = 0 ; itpenu = 0 ; kew = 0; ftpet = 1d6

 if (allocated(ketr) ) deallocate(ketr)          
 allocate ( ketr(nx,1), stat = ierr )
 call aerr('ketr(nx,1)', ierr, nx)
            ketr = 0

 if ( allocated(nbndtr) ) deallocate(nbndtr)
 allocate ( nbndtr(1), stat = ierr )
 call aerr('nbndtr(1)', ierr, 1 )
            nbndtr = 0
            
 if ( allocated(trnames) ) deallocate(trnames)
 allocate ( trnames(1), stat = ierr )
 call aerr('trnames(1)', ierr, 1 )
            trnames(1) = ''
 numtracers = 0
 
 ! JRE DEBUG sedfrac 
 
    if (allocated(kesf) ) deallocate(kesf)          
    allocate ( kesf(1,nx), stat = ierr )   ! would have been nice to have stmpar%lsedsus,
    call aerr('kesf(1,nx)', ierr, nx)      ! but no can do, jammer de bammer...
               kesf = 0
               
    if ( allocated(nbndsf) ) deallocate(nbndsf)
    allocate ( nbndsf(1), stat = ierr )
    call aerr('nbndsf(1)', ierr, 1 )
               nbndsf = 0
               
    if ( allocated(sfnames) ) deallocate(sfnames)
    allocate ( sfnames(1), stat = ierr )
    call aerr('sfnames(1)', ierr, 1 )
    sfnames = ''
    numfracs = 0
    !do ifrac = 1, stmpar%lsedsus
    !   sfnames(ifrac) = stmpar%sedpar%NAMSED(sedtot2sedsus(ifrac))
    !end do

 !\DEBUG sedfrac 
      
 call make_mirrorcells(Nx, xe, ye, xyen, kce, ke, ierror) 
 
 if ( jampi.eq.1 ) then
! disable mirror cells that are not mirror cells in the whole model by setting kce=0
    call partition_reduce_mirrorcells(Nx, kce, ke, ierror)
 end if

 nbndz = 0                                           ! startindex waterlevel bnds
 nbndu = 0                                           ! startindex velocity   bnds
 nbnds = 0                                           ! startindex salinity   bnds
 nbndtm = 0                                          ! startindex temperature bnds
 nbndt = 0                                           ! startindex tangential vel. bnds
 nbnduxy = 0                                         ! startindex uxuy vel. bnds 
 nbndn = 0                                           ! startindex normal     vel. bnds
 nbnd1d2d = 0                                        ! startindex 1d2d bnds
 ngate = 0                                           ! startindex gate links
 ncdam = 0                                           ! startindex cdam links
 npump = 0                                           ! startindex pump links
 nbndw  = 0                                          ! startindex wave energy bnds 

 nqbnd   = 0                                         ! nr of q sections   or specified q bnd's
 nqhbnd  = 0                                         ! nr of qh boundary sections or specified qh bnd's
 ngatesg = 0                                         ! nr of gate signals or specified gates ! not in loop below because flow links not ready yet
 ncdamsg = 0                                         ! nr of controllable dam signals
 npumpsg = 0                                         ! nr of pump signals
 nshiptxy = 0                                        ! nr of ship xyt signals

 nwbnd    = 0                                        ! nr of wave-energy boundaries


 num_bc_ini_blocks = 0
 if (ext_force_bnd_used) then
    ! first read the bc file (new file format for boundary conditions)
    call readlocationfilesfromboundaryblocks(trim(md_extfile_new), filetype, nx, kce, num_bc_ini_blocks, &
                                         numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
 endif
 
 do while (ja_ext_force .eq. 1)                      ! read *.ext file

    call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja_ext_force,varname)
    
    if (num_bc_ini_blocks > 0 .and. qid(len_trim(qid)-2:len_trim(qid)) == 'bnd') then
       write(msgbuf, '(a)') 'Boundaries in BOTH external forcing and bound.ext.force file is not allowed'
       call msg_flush()
       call qnerror( 'Boundaries in two files: ', trim(md_extfile_new), ' and ' // trim(md_extfile) )
       ja_ext_force = 0
    endif

    if (ja_ext_force == 1) then

        jatimespace = 1                              ! module is to be used

        call processexternalboundarypoints(qid, filename, filetype, return_time,  nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, 1d0, transformcoef)
    
    endif

 enddo

 deallocate(kce)
 deallocate(ke)

 if (mext > 0) then
    rewind (mext)                                      ! prepare input file
 end if
 numbnp = nbndz + nbndu + nbnd1d2d                             ! nr of boundary points =

end subroutine findexternalboundarypoints



subroutine readlocationfilesfromboundaryblocks(filename, filetype, nx, kce, num_bc_ini_blocks, &
                                                numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf)
 use properties
 use tree_data_types
 use tree_structures
 use messageHandling
 use m_flowgeom, only: rrtol

 implicit none

 character(len=*)      , intent(in)    :: filename
 integer               , intent(in)    :: filetype 
 integer               , intent(in)    :: nx
 integer, dimension(nx), intent(inout) :: kce
 integer               , intent(out)   :: num_bc_ini_blocks
 integer               , intent(inout) :: numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf

 type(tree_data), pointer     :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
 type(tree_data), pointer     :: node_ptr            !
 integer                      :: istat               !
 integer, parameter           :: ini_key_len   = 32  !
 integer, parameter           :: ini_value_len = 256 !
 character(len=ini_key_len)   :: groupname           !
 character(len=ini_value_len) :: quantity            !
 character(len=ini_value_len) :: locationfile        !
 character(len=ini_value_len) :: forcingfile         !
 double precision             :: return_time         !
 double precision             :: rrtolb              ! Local, optional boundary tolerance value.
 integer                      :: i                   !
 integer                      :: num_items_in_file   !
 logical                      :: file_ok             !
 logical                      :: group_ok            !
 logical                      :: property_ok         !

 call tree_create(trim(filename), bnd_ptr)
 call prop_file('ini',trim(filename),bnd_ptr,istat)
 if (istat /= 0) then
     call qnerror( 'Boundary external forcing file ', trim(filename), ' could not be read' )
     return
 end if

 num_items_in_file = 0
 if (associated(bnd_ptr%child_nodes)) then
     num_items_in_file = size(bnd_ptr%child_nodes)
 endif

 file_ok = .true.
 do i=1,num_items_in_file
    node_ptr => bnd_ptr%child_nodes(i)%node_ptr
    groupname = tree_get_name(bnd_ptr%child_nodes(i)%node_ptr)
    if (trim(groupname) == 'boundary') then
       quantity = ''
       locationfile = ''
       forcingfile = ''
       return_time = 0.0

       group_ok = .true.

       ! todo: read multiple quantities
       call prop_get_string(node_ptr, '', 'quantity', quantity, property_ok)
       if (.not. property_ok) then
          call qnerror( 'Expected property' , 'quantity', ' for boundary definition' )
       end if
       
       group_ok = group_ok .and. property_ok
       
       call prop_get_string(node_ptr, '', 'locationfile', locationfile, property_ok)
       if (.not. property_ok)  then
          call qnerror( 'Expected property' , 'locationfile', ' for boundary definition' )
       end if
       
       group_ok = group_ok .and. property_ok
       
       call prop_get_string(node_ptr, '', 'forcingfile ', forcingfile , property_ok)
       if (.not. property_ok)  then
          call qnerror( 'Expected property' , 'forcingfile', ' for boundary definition' )
       end if

       group_ok = group_ok .and. property_ok

       call prop_get_double(node_ptr, '', 'return_time', return_time )

       rrtolb = 0d0
       call prop_get_double(node_ptr, '', 'OpenBoundaryTolerance', rrtolb)

       if (group_ok) then
          if (rrtolb > 0d0) then
             call processexternalboundarypoints(quantity, locationfile, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel = (1+2*rrtolb)/(1+2*rrtol))
          else
             call processexternalboundarypoints(quantity, locationfile, filetype, return_time, nx, kce, numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf, rrtolrel = 1d0)
          end if
          num_bc_ini_blocks = num_bc_ini_blocks + 1
       endif

       file_ok = file_ok .and. group_ok

    else
       ! warning: unknown group
    endif
    
 end do

 call tree_destroy(bnd_ptr)

end subroutine readlocationfilesfromboundaryblocks
                                                
subroutine appendrettime(qidfm, nbnd, rettime)

 use m_flowexternalforcings
 use m_alloc
 
 implicit none
 
 character(len=256), intent(in)  :: qidfm ! constituent index
 integer, intent(in)             :: nbnd    ! boundary cell index
 double precision, intent(in)    :: rettime ! return time (h)
 integer                         :: thrtlen ! temp array length
 
 if (allocated(thrtt)) then 
    thrtlen = size(thrtt) + 1
 else
    thrtlen = 1
 endif 
 
 call realloc(thrtq, thrtlen, keepExisting=.true., fill='')
 thrtq(thrtlen) = qidfm
 
 call realloc(thrtn, thrtlen, keepExisting=.true., fill=0)
 thrtn(thrtlen) = nbnd
 
 call realloc(thrtt, thrtlen, keepExisting=.true., fill=0d0)
 thrtt(thrtlen) = rettime
end subroutine appendrettime


!> helper routine finding external boundary points, called for both old and new-type ext file.
!! Also used for some none-boundary quantities that also need counting total nr of elements, *prior* to flow_initexternalforcings.
!! Two stages: 1 = collect elsets for which data is provided         <-- findexternalboundarypoints + processexternalboundarypoints
!!             2 = add relations between elsets and their providers  <-- flow_initexternalforcings
!! This routine is based upon the network admin only, not on the flow admin.
subroutine processexternalboundarypoints(qid, filename, filetype, return_time, nx, kce, &
                                         numz, numu, nums, numtm, numsd, numt, numuxy, numn, num1d2d, &
                                         numqh, numw, numtr, numsf, rrtolrel, tfc) ! helper for findin external boundary points
 use m_netw
 use m_flow, qid_flow => qid, filetype_flow => filetype
 use m_flowgeom                                        
 use unstruc_model                                     
 use timespace                                         
 use m_sferic
 use m_alloc
 use unstruc_messages
 use m_ship
 use properties
 use m_ec_magic_number
 use m_transport
 use m_sediment, only: stm_included, stmpar, sedtot2sedsus
 use sediment_basics_module, only: SEDTYP_NONCOHESIVE_SUSPENDED, SEDTYP_COHESIVE
 use m_meteo, qid_meteo => qid, filetype_meteo => filetype 
 use m_sobekdfm
 use m_flowparameters, only: jawave
 use string_module
 use m_strucs, only: numgeneralkeywrd
 
 implicit none

 character(len=256)    , intent(in)    :: qid                                 !
 character(len=256)    , intent(in)    :: filename                            !
 integer               , intent(in)    :: filetype
 integer               , intent(in)    :: nx                                  !
 integer, dimension(nx), intent(inout) :: kce                                 !
 double precision      , intent(in)    :: return_time
 integer               , intent(inout) :: numz, numu, nums, numtm, numsd, &   !
                                          numt, numuxy, numn, num1d2d, numqh, numw, numtr, numsf      !
 double precision      , intent(in)    :: rrtolrel !< To enable a more strict rrtolerance value than the global rrtol. Measured w.r.t. global rrtol.
 
 double precision, dimension(numgeneralkeywrd), optional, intent(in) :: tfc
 
 character(len=256)                    :: qidfm                               !
 integer                               :: itpbn
 character (len=NAMTRACLEN)            :: tracnam, sfnam, qidnam
 integer                               :: itrac, isf
 integer, external                     :: findname
 
 integer                               :: iconst

!  call bndname_to_fm(qid,qidfm)
  qidfm = qid
  if (qidfm == 'waterlevelbnd'    .or. qidfm == 'neumannbnd'  .or. qidfm == 'riemannbnd' .or. qidfm == 'outflowbnd' .or. qidfm == 'qhbnd') then

     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kez(nbndz+1:nx), numz, usemask=.true.) !numz=number cells found
     WRITE(msgbuf,'(3a,i8,a)') trim (qid), ' ', trim( filename), numz, ' nr of open bndcells' ; call msg_flush()

     nzbnd = nzbnd + 1

     if (qidfm == 'waterlevelbnd')  itpbn = 1
     if (qidfm == 'neumannbnd'   )  itpbn = 2
     if (qidfm == 'riemannbnd'  )   then 
        itpbn = 5
        if (present(tfc)) then
           ftpet(nbndz+1:nbndz+numz) = tfc(7)    ! relaxation time riemann from ext file
        end if
     end if
     if (qidfm == 'outflowbnd'   )  itpbn = 6

     
     if (qidfm == 'qhbnd') then
         itpbn = 7
         nqhbnd = nqhbnd + 1
         numqh  = numz
         call realloc(L1qhbnd,nqhbnd) ; L1qhbnd(nqhbnd) = nbndz + 1
         call realloc(L2qhbnd,nqhbnd) ; L2qhbnd(nqhbnd) = nbndz + numz
         call realloc(atqh_all,nqhbnd);  atqh_all(nqhbnd) = 0d0
         call realloc(atqh_sum,nqhbnd);  atqh_sum(nqhbnd) = 0d0
         call realloc(qhbndz,nqhbnd)  ;  qhbndz(nqhbnd) = 0d0
         call realloc(magic_array,nqhbnd)  ;  magic_array(nqhbnd) = 0d0
     end if    
     itpez(nbndz+1:nbndz+numz) =  itpbn
     
     call addopenbndsection(numz, kez(nbndz+1:nbndz+numz), filename, IBNDTP_ZETA)
     itpenz(nbndz+1:nbndz+numz) = nopenbndsect
     nbndz = nbndz + numz
     
  else if (qidfm == 'velocitybnd' .or. qidfm == 'dischargebnd' .or. qidfm == 'qhubnd'.or. &
           qidfm == 'criticaloutflowbnd' .or. qidfm == 'weiroutflowbnd' .or. qidfm == 'absgenbnd') then
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, keu(nbndu+1:nx), numu, usemask=.true., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(3a,i8,a)') trim (qid), ' ', trim( filename), numu, ' nr of open bndcells' ; call msg_flush()

     nubnd = nubnd + 1
     
     if (qidfm == 'velocitybnd' ) then 
        itpbn = 3
     else if (qidfm == 'dischargebnd') then      
        itpbn = 4
        nqbnd = nqbnd + 1
        call realloc(L1qbnd,nqbnd) ; L1qbnd(nqbnd) = nbndu + 1
        call realloc(L2qbnd,nqbnd) ; L2qbnd(nqbnd) = nbndu + numu
        call realloc(at_all,nqbnd);  at_all(nqbnd) = 0d0
        call realloc(at_sum,nqbnd);  at_sum(nqbnd) = 0d0
        call realloc(wwssav_all,(/2,nqbnd/), keepExisting=.true., fill=0d0)
        call realloc(wwssav_sum,(/2,nqbnd/), keepExisting=.true., fill=0d0)
        call realloc(huqbnd,L2qbnd(nqbnd)); huqbnd(L1qbnd(nqbnd):L2qbnd(nqbnd)) = 0d0
     else if ( qidfm == 'absgenbnd') then
        if (.not. (jawave.eq.4)) then                 ! Safety to avoid allocation errors later on
           call qnerror( 'Absorbing-generating boundary defined without activating XBeach wave driver. Please set Wavemodelnr=4, or change the boundary condition type.', '  ', ' ')
           write(msgbuf, '(a)') 'Absorbing-generating boundary defined without activating XBeach wave driver. Please set Wavemodelnr=4, or change the boundary condition type.'
           call err_flush()
        end if
        itpbn = 5
        !ftpet(nbndu+1:nbndu+numu) = tfc(7)   ! riemann relaxation
     else if ( qidfm == 'qhubnd') then
        itpbn = 6 
     else if ( qidfm == 'criticaloutflowbnd') then
        itpbn = 8
     else if ( qidfm == 'weiroutflowbnd') then
        itpbn = 9
     endif

     itpeu(nbndu+1:nbndu+numu) = itpbn

     call addopenbndsection(numu, keu(nbndu+1:nbndu+numu), filename, IBNDTP_U)

     itpenu(nbndu+1:nbndu+numu) = nopenbndsect
     nbndu = nbndu + numu

  else if (qidfm == 'salinitybnd' .and. jasal>0 ) then

     kce   = abs(kce) ! switch kce back on, but only for all net boundaries (some of which may have been set to -1 by a flow boundary)
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kes(nbnds+1:nx), nums, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename), nums, 'nr of salinity bndcells' ; call msg_flush()
     if (nums>0) then
        call appendrettime(qidfm, nbnds + 1, return_time)
        nbnds = nbnds + nums
     end if
  ! JRE
     
  else if (qidfm == 'waveenergybnd' ) then

     kce   = abs(kce) ! switch kce back on, but only for all net boundaries (some of which may have been set to -1 by a flow boundary)
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kew(nbndw+1:nx), numw, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename), numw, 'nr of wave energy bndcells' ; call msg_flush()

     nwbnd = nwbnd + 1

     call realloc(L1wbnd,nwbnd) ; L1wbnd(nwbnd) = nbndw + 1
     call realloc(L2wbnd,nwbnd) ; L2wbnd(nwbnd) = nbndw + numw
     
     nbndw = nbndw + numw
     call realloc(fnamwbnd,nwbnd,fill='')
     fnamwbnd(nwbnd) = trim(filename)

  else if (qidfm == 'temperaturebnd' .and. jatem > 0 ) then

     kce   = abs(kce) ! switch kce back on, but only for all net boundaries (some of which may have been set to -1 by a flow boundary)
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ketm(nbndtm+1:nx), numtm, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename), numtm, 'nr of temperature bndcells' ; call msg_flush()
     if (numtm>0) then
        call appendrettime(qidfm, nbndtm + 1, return_time)
        nbndtm = nbndtm + numtm
     end if

  else if (qidfm == 'sedimentbnd' ) then

     kce   = abs(kce) ! switch kce back on, but only for all net boundaries (some of which may have been set to -1 by a flow boundary)
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kesd(nbndsd+1:nx), numsd, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename), numsd, 'nr of sediment bndcells' ; call msg_flush()
     if (numsd>0) then
        call appendrettime(qidfm, nbndsd + 1, return_time)
        nbndsd = nbndsd + numsd
     end if 
     
  else if (qidfm(1:9) == 'tracerbnd' ) then
     
     kce   = abs(kce) ! switch kce back on, but only for all net boundaries (some of which may have been set to -1 by a flow boundary)
     call get_tracername(qidfm, tracnam, qidnam)
     itrac = findname(numtracers, trnames, tracnam)
     
!    add tracer name  if it does not already exist
     if ( itrac.eq.0 ) then
     
        numtracers = numtracers+1    
!       realloc
        call realloc(ketr, (/ Nx, numtracers /), keepExisting=.true., fill=0 )
        call realloc(nbndtr, numtracers, keepExisting=.true., fill=0 )
        call realloc(trnames, numtracers, keepExisting=.true., fill='')
        call realloc(wstracers, numtracers, keepExisting=.true., fill=0d0)
        wstracers(numtracers) = transformcoef(4)
                
        trnames(numtracers) = trim(tracnam)
        itrac = numtracers
        
     end if
     
     ! kce   = 1 ! switch kce back on as points to be potentially flagged
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ketr(nbndtr(itrac)+1:,itrac), numtr, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(3a,i8,a)') trim(qid), ' ', trim(filename) , numtr, 'nr of tracer bndcells' ; call msg_flush()
     if (numtr>0) then
        call appendrettime(qidfm, nbndtr(itrac) + 1, return_time)
        nbndtr(itrac) = nbndtr(itrac) + numtr
        nbndtr_all = maxval(nbndtr(1:numtracers))
     end if
     
  else if (qid(1:13) == 'initialtracer' ) then
     call get_tracername(qid, tracnam, qidnam)
     itrac = findname(numtracers, trnames, tracnam)
     
!    add tracer name  if it does not already exist
     if ( itrac.eq.0 ) then
        
        numtracers = numtracers+1    
!       realloc
        call realloc(ketr, (/ Nx, numtracers /), keepExisting=.true., fill=0 )
        call realloc(nbndtr, numtracers, keepExisting=.true., fill=0 )
        call realloc(trnames, numtracers, keepExisting=.true., fill='')
        call realloc(wstracers, numtracers, keepExisting=.true., fill=0d0)
        wstracers(numtracers) = transformcoef(4)
  
        trnames(numtracers) = trim(tracnam)
     end if
     
  ! DEBUG JRE sedfrac
  else if (qidfm(1:10) == 'sedfracbnd' .and. jased > 0) then
     
     kce = 1     
     call get_sedfracname(qidfm, sfnam, qidnam)    
     isf = findname(numfracs, sfnames, sfnam)

     if ( isf.eq.0 ) then   ! add 
     
        numfracs = numfracs+1    
!       realloc
        call realloc(kesf, (/Nx, numfracs/), keepExisting=.true., fill=0 )
        call realloc(nbndsf, numfracs, keepExisting=.true., fill=0 )
        call realloc(sfnames, numfracs, keepExisting=.true., fill='')

        sfnames(numfracs) = trim(sfnam)
        isf = numfracs
        
     end if

     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, kesf(nbndsf(isf)+1:,isf), numsf, usemask=.false.)
     WRITE(msgbuf,'(3a,i8,a)') trim(qid), ' ', trim(filename) , numsf, ' nr of sedfrac bndcells' ; call msg_flush()
     if (numsf > 0) then
        call appendrettime(qidfm, nbndsf(isf) + 1, return_time)
        nbndsf(isf) = nbndsf(isf) + numsf
        nbndsf_all = maxval(nbndsf(1:numfracs))
     endif
     
  !\ JRE DEBUG sedfrac
     
  else if (qidfm == 'tangentialvelocitybnd' ) then

     ! kce   = 1 ! switch kce back on as points to be potentially flagged
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ket(nbndt+1:nx), numt, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename) , numt, 'nr of tangentialvelocity bndcells' ; call msg_flush()

     nbndt = nbndt + numt

  else if (qidfm == 'uxuyadvectionvelocitybnd' ) then

     ! kce   = 1 ! switch kce back on as points to be potentially flagged
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, keuxy(nbnduxy+1:nx), numuxy, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename) , numuxy, 'nr of tangentialvelocity bndcells' ; call msg_flush()

     nbnduxy = nbnduxy + numuxy


  else if (qidfm == 'normalvelocitybnd' ) then

     ! kce   = 1 ! switch kce back on as points to be potentially flagged
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ken(nbndn+1:nx), numn, usemask=.false., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename) , numn, 'nr of normalvelocity bndcells' ; call msg_flush()

     nbndn = nbndn + numn

  else if (qidfm == '1d2dbnd' ) then ! SOBEK1D-FM2D

     ! kce   = 1 ! switch kce back on as points to be potentially flagged
     call selectelset( filename, filetype, xe, ye, xyen, kce, nx, ke1d2d(nbnd1d2d+1:nx), num1d2d, usemask=.true., rrtolrel=rrtolrel)
     WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(filename) , num1d2d, 'nr of SOBEK1D-FM2D bndcells' ; call msg_flush()

     call addopenbndsection(num1d2d, ke1d2d(nbnd1d2d+1:nbnd1d2d+num1d2d), filename, IBNDTP_1D2D)
     nbnd1d2d = nbnd1d2d + num1d2d

  else if (qidfm == 'shiptxy' ) then

     nshiptxy = nshiptxy + 1
     
  else if (qidfm == 'nudgetime' .or. qidfm == 'nudge_salinity_temperature' ) then
  
     janudge = 1
     
 endif

 end subroutine processexternalboundarypoints


!> Calls the ec_addtimespacerelation with all proper unstruc-specific target arrays and element set masks.
function addtimespacerelation_boundaries(qid, filename, filetype, method, operand, forcingfile) result(success)
   use m_flowexternalforcings, no1=>qid, no2=>filetype, no3=>operand, no4 => success
   use m_meteo, no5=>qid, no6=>filetype, no7=>operand, no8 => success
   use m_flowparameters, only: jawave
   use m_flow, only: kmx
   use m_flowtimes, only: dt_nodal
   
   implicit none
   
   character(len=*),            intent(inout) :: qid         !< Identifier of current quantity (i.e., 'waterlevelbnd')
   character(len=*),            intent(in)    :: filename    !< Name of data file for current quantity.
   integer,                     intent(in)    :: filetype    !< File type of current quantity.
   integer,                     intent(in)    :: method      !< Time-interpolation method for current quantity.
   character(len=1),            intent(in)    :: operand     !< Operand w.r.t. previous data ('O'verride or '+'Append)
   character(len=*),  optional, intent(in)    :: forcingfile !< Optional forcings file, if it differs from the filename (i.e., if filename=*.pli, and forcingfile=*.bc)

   logical                       :: success
   character(len=256)            :: tracnam, sfnam, qidnam
   integer                       :: itrac, isf, iconst
   integer, external             :: findname
   double precision, dimension(:), pointer     :: pzmin, pzmax

   success = .true.   ! initialization

   ! Special forcingsfile: if name equals 'REALTIME', do not do an ec_addtimespacerelation, just leave it to the external caller to fill zbnd* target value array.
   ! TODO: AVD: we now leave it to caller to fill array with length(zbnd*),
   ! instead of the number of polyline points. Cleaner alternative is to create
   ! a poly_tim provider, with the *underlying* point child providers being REALTIME.
   if (present(forcingfile)) then
      if (trim(forcingfile) == 'REALTIME') then
         call mess(LEVEL_DEBUG, 'addtimespacerelation_boundaries: leave empty timespacerelation for '''//trim(qid)//''' from locationfile '''//trim(filename)//''' (REALTIME data).')
         return
      end if
   end if

   kx = 1
   if (nbndz > 0 .and. (qid == 'waterlevelbnd' .or. qid == 'neumannbnd' .or. qid == 'riemannbnd' .or. qid == 'outflowbnd')) then
      success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcingfile, dtnodal=dt_nodal)

   else if (nqhbnd > 0 .and. (qid == 'qhbnd')) then
      success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcingfile)
           
   else if (nbndu > 0 .and. (qid == 'dischargebnd' .or. qid == 'criticaloutflowbnd' .or. qid == 'weiroutflowbnd' .or. qid == 'absgenbnd' ) ) then
      if ( qid.eq.'absgenbnd' ) then
         jawave = 4
      end if
      success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, xy2bndu, forcingfile=forcingfile)

   else if (nbndu > 0 .and. qid == 'velocitybnd' ) then
      if (kmx == 0) then
         success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, xy2bndu, forcingfile=forcingfile)
      else
         pzmin => zminmaxu(1:nbndu)
         pzmax => zminmaxu(nbndu+1:2*nbndu)
         success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand,   &
                                           xy2bndu, z=sigmabndu, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)
      endif

   else if (nbnds > 0 .and. qid == 'salinitybnd' ) then ! 2D
      if (kmx == 0) then
         success = ec_addtimespacerelation(qid, xbnds, ybnds, kds, kx, filename, filetype, method, operand, xy2bnds, forcingfile=forcingfile)
      else
         pzmin => zminmaxs(1:nbnds)
         pzmax => zminmaxs(nbnds+1:2*nbnds)
         success = ec_addtimespacerelation(qid, xbnds, ybnds, kds, kx, filename, filetype, method, operand, xy2bnds,    &
                                           z=sigmabnds, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)
      endif
              
   else if (nbndTM > 0 .and. qid == 'temperaturebnd') then
            
      if (kmx == 0) then ! 2D
         success = ec_addtimespacerelation(qid, xbndTM, ybndTM, kdtm, kx, filename, filetype, method, operand, xy2bndtm, forcingfile=forcingfile)
      else               ! 3D
         pzmin => zminmaxtm(1:nbndTM)
         pzmax => zminmaxtm(nbndTM+1:2*nbndTM)
         success = ec_addtimespacerelation(qid, xbndTM, ybndTM, kdtm, kx, filename, filetype, method, operand, xy2bndtm,   &
                                           z=sigmabndtm, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)
      endif 
     
   else if (nbndsd > 0 .and. (qid == 'sedimentbnd')) then
         pzmin => zminmaxsd(1:nbndsd)
         pzmax => zminmaxsd(nbndsd+1:2*nbndsd)
         success = ec_addtimespacerelation(qid, xbndsd, ybndsd, kdsd, kx, filename, filetype, method, operand, xy2bndsd,   &
                                           z=sigmabndsd, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)

   else if ( numtracers > 0 .and. (qid(1:9) == 'tracerbnd') ) then
      ! get tracer boundary condition number
      call get_tracername(qid, tracnam, qidnam)
      itrac = findname(numtracers, trnames, tracnam)
           
! for parallel runs, we always need to add the tracer, even if this subdomain has no tracer boundary conditions defined
!      call add_tracer(tracnam, iconst)
!      update: all tracers are counted first and allocated later

      if ( nbndtr(itrac).gt.0 ) then
         if ( kmx.eq.0 ) then  ! 2D
            if (present(forcingfile)) then
               success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2, forcingfile=forcingfile)
            else
               success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2)
            end if
         else                  ! 3D
            pzmin => bndtr(itrac)%zminmax(1:nbndtr(itrac))
            pzmax => bndtr(itrac)%zminmax(nbndtr(itrac)+1:2*nbndtr(itrac))
            if (present(forcingfile)) then
               success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2,    & 
                                                 z=bndtr(itrac)%sigma, forcingfile=forcingfile, pzmin=pzmin, pzmax=pzmax)
            else
               success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2,    &
                                                 z=bndtr(itrac)%sigma, pzmin=pzmin, pzmax=pzmax)
            end if
         end if
      else
         success = .true.
      end if

! JRE DEBUG sedfrac
   else if ( numfracs > 0 .and. (qid(1:10) == 'sedfracbnd') .and. stm_included) then

      call get_sedfracname(qid, sfnam, qidnam)
      isf = findname(numfracs, sfnames, sfnam)

      if (isf > 0) then
         if ( nbndsf(isf).gt.0 ) then
            if ( kmx.eq.0 ) then
               success = ec_addtimespacerelation(qid, bndsf(isf)%x, bndsf(isf)%y, bndsf(isf)%kd, kx, filename, filetype, method, operand, bndsf(isf)%xy2, forcingfile=forcingfile)
            else
               success = ec_addtimespacerelation(qid, bndsf(isf)%x, bndsf(isf)%y, bndsf(isf)%kd, kx, filename, filetype, method, operand, bndsf(isf)%xy2, bndsf(isf)%sigma, forcingfile=forcingfile)
            end if
         else
            success = .true.
         end if
      else
         call mess(LEVEL_WARN, 'Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
         call qnerror('Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.',' ',' ')
      end if

   else if (nbndt > 0 .and. (qid == 'tangentialvelocitybnd')) then
      success = ec_addtimespacerelation(qid, xbndt, ybndt, kdt, kx, filename, filetype, method, operand, xy2bndt, forcingfile=forcingfile)

   else if (nbnduxy > 0 .and. (qid == 'uxuyadvectionvelocitybnd')) then

      kx = 2
      if (kmx == 0) then ! 2D
         success = ec_addtimespacerelation(qid, xbnduxy, ybnduxy, kduxy, kx, filename, filetype, method, operand, xy2bnduxy, forcingfile=forcingfile)
      else 
         pzmin => zminmaxuxy(1:nbnduxy)
         pzmax => zminmaxuxy(nbnduxy+1:2*nbnduxy)
         success = ec_addtimespacerelation(qid, xbnduxy, ybnduxy, kduxy, kx, filename, filetype, method, operand, xy2bnduxy,   &
                                           z=sigmabnduxy, pzmin=pzmin, pzmax=pzmax, forcingfile=forcingfile)
      endif 

   else if (nbndn > 0 .and. (qid == 'normalvelocitybnd')) then
      success = ec_addtimespacerelation(qid, xbndn, ybndn, kdn, kx, filename, filetype, method, operand, xy2bndn, forcingfile=forcingfile)

   else !There is some boundary that is not detected or recognized
!      success = .false.      
! SPvdP: this is not an error, especially for parallel runs
   end if
end function addtimespacerelation_boundaries

logical function initboundaryblocksforcings(filename)
 use properties
 use tree_data_types
 use tree_structures
 use messageHandling
 use m_flowexternalforcings
 use m_flowgeom
 use timespace_data, only: weightfactors, poly_tim, uniform, spaceandtime, getmeteoerror
 use m_wind ! for laterals
 use m_alloc
 use m_meteo, only: ec_addtimespacerelation
 use timespace
 use string_module, only: str_tolower


 implicit none

 character(len=*), intent(in) :: filename            !< file name for new external forcing boundary blocks
 type(tree_data), pointer     :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
 type(tree_data), pointer     :: node_ptr            !
 type(tree_data), pointer     :: block_ptr           !
 integer                      :: istat               !
 integer, parameter           :: ini_key_len   = 32  !
 integer, parameter           :: ini_value_len = 256 !
 character(len=ini_key_len)   :: groupname           !
 character(len=ini_value_len) :: property_name
 character(len=ini_value_len) :: property_value
 character(len=ini_value_len) :: quantity
 character(len=ini_value_len) :: locationfile        !
 character(len=ini_value_len) :: forcingfile         !
 integer                      :: i,j                 !
 integer                      :: num_items_in_file   !
 integer                      :: num_items_in_block
 logical                      :: retVal
 character(len=1)             :: oper                !
 character (len=300)          :: rec

 character(len=ini_value_len) :: locid
 character(len=ini_value_len) :: itemtype
 character(len=256)           :: fnam
 double precision             :: tmpval
 integer                      :: iostat, ierr
 integer                      :: ilattype
 integer                      :: k, n
 
 double precision, allocatable :: xdum(:), ydum(:)!, xy2dum(:,:)
 integer, allocatable          :: kdum(:)

 if (allocated(xdum  )) deallocate(xdum, ydum, kdum) !, xy2dum)
 allocate ( xdum(1), ydum(1), kdum(1)) !, xy2dum(2,1) , stat=ierr)
 !call aerr('xdum(1), ydum(1), kdum(1), xy2dum     ', ierr, 3)
 xdum = 1d0 ; ydum = 1d0; kdum = 1!; xy2dum = 0d0

 initboundaryblocksforcings = .true.

 call tree_create(trim(filename), bnd_ptr)
 call prop_file('ini',trim(filename),bnd_ptr,istat)
 call init_registered_items()

 num_items_in_file = 0
 if (associated(bnd_ptr%child_nodes)) then
     num_items_in_file = size(bnd_ptr%child_nodes)
 endif

 do i=1,num_items_in_file

    node_ptr => bnd_ptr%child_nodes(i)%node_ptr
    groupname = tree_get_name(node_ptr)
    select case (str_tolower(trim(groupname)))
    case ('boundary')

       ! First check for required input:
       call prop_get_string(node_ptr, '', 'quantity', quantity, retVal)
       if (.not. retVal) then
          initboundaryblocksforcings = .false.
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''quantity'' is missing.'
          call warn_flush()
          cycle
       end if

       call prop_get_string(node_ptr, '', 'locationfile', locationfile, retVal)
       if (.not. retVal) then
          initboundaryblocksforcings = .false.
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''locationfile'' is missing.'
          call warn_flush()
          cycle
       end if

       call prop_get_string(node_ptr, '', 'forcingfile ', forcingfile , retVal)
       if (.not. retVal) then
          initboundaryblocksforcings = .false.
          write(msgbuf, '(5a)') 'Incomplete block in file ''', trim(filename), ''': [', trim(groupname), ']. Field ''forcingfile'' is missing.'
          call warn_flush()
          cycle
       end if
          
       
       num_items_in_block = 0
       if (associated(node_ptr%child_nodes)) then
           num_items_in_block = size(node_ptr%child_nodes)
       endif

       ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingfile=...
       do j=1,num_items_in_block
          block_ptr => node_ptr%child_nodes(j)%node_ptr
          ! todo: read multiple quantities
          property_name = tree_get_name(block_ptr)
          call tree_get_data_string(block_ptr, property_value, retVal)
          if (retVal) then
             if (property_name == 'quantity') then
                quantity = property_value ! We already knew this
             else if (property_name == 'locationfile') then
                locationfile = property_value ! We already knew this
             else if (property_name == 'forcingfile') then
                forcingfile = property_value
                oper = 'O'
                if (quantity_pli_combination_is_registered(quantity, locationfile)) then
                   oper = '+'
                endif
                call register_quantity_pli_combination(quantity, locationfile)
                if (forcingfile == '-') then
                   retVal = addtimespacerelation_boundaries(quantity, locationfile, filetype=poly_tim, method=weightfactors, operand=oper)
                else
                   retVal = addtimespacerelation_boundaries(quantity, locationfile, filetype=poly_tim, method=weightfactors, operand=oper, forcingfile = forcingfile)
                endif				   
                initboundaryblocksforcings = initboundaryblocksforcings .and. retVal ! Remember any previous errors.
             else if (property_name == 'return_time') then
                continue                   ! used elsewhere to set Thatcher-Harleman delay 
             else if (property_name == 'openboundarytolerance') then
                continue                   ! used in findexternalboundarypoints/readlocationfiles... to set search distance. Not relevant here. 
             else
                ! initboundaryblocksforcings remains unchanged: support ignored lines in ext file.
                write(msgbuf, '(9a)') 'Unrecognized line in file ''', trim(filename), ''' for block [', trim(groupname), ']: ', trim(property_name), ' = ', trim(property_value), '. Ignoring this line.'
                call warn_flush()
                cycle
             endif
          endif
       enddo
       if (.not. retVal) then ! This addtimespace was not successful
          rec = getmeteoerror()
          if (len_trim(rec)>0) then
             call mess(LEVEL_WARN, trim(rec))
          endif
          call mess(LEVEL_WARN, 'initboundaryblockforcings: Error while initializing quantity '''//trim(quantity)//'''. Check preceding log lines for details.')
       end if
    case ('lateral')
       ! [lateral]
       ! Id = ...
       locid = ' '
       call prop_get(node_ptr, '', 'Id', locid, success)
       if (.not. success .or. len_trim(locid) == 0) then
          write(msgbuf, '(a,i0,a)') 'Required field ''Id'' missing in lateral (block #', i, ').'
          call warn_flush()
          cycle
       end if
 
       ! [lateral]
       ! Type = 1d | 2d | 1d2d
       itemtype = ' '
       call prop_get(node_ptr, '', 'Type',         itemtype, success)
       ! Type = optional for lateral
       select case (str_tolower(trim(itemtype)))
       case ('1d')
          ilattype = ILATTP_1D
       case ('2d')
          ilattype = ILATTP_2D
       case ('1d2d')
          ilattype = ILATTP_ALL
       case default
          ilattype = ILATTP_ALL
       end select

       ! [lateral]
       ! LocationFile = test.pol
       locationfile = ' '
       call prop_get(node_ptr, '', 'LocationFile', locationfile, success)
       if (.not. success .or. len_trim(locationfile) == 0) then
          write(msgbuf, '(a,a,a)') 'Required field ''LocationFile'' missing in lateral ''', trim(locid), '''.'
          call warn_flush()
          cycle
       end if
       
       ! TODO: AvD: support NodeIds instead of LocationFile

       call ini_alloc_laterals()

       call prepare_lateral_mask(kclat, ilattype)

       numlatsg = numlatsg + 1
       call selectelset_internal_nodes( locationfile, 10, xz, yz, kclat, ndxi, numlatsg, nnLat) ! find nodes in polygon  

       call realloc(qplat, numlatsg, keepExisting = .true.)

       ! [lateral]
       ! Flow = 1.23 | test.tim | REALTIME
       kx = 1
       rec = ' '
       call prop_get(node_ptr, '', 'Flow', rec, success)
       if (.not. success .or. len_trim(rec) == 0) then
          write(msgbuf, '(a,a,a)') 'Required field ''Flow'' missing in lateral ''', trim(locid), '''.'
          call warn_flush()
          cycle
       end if

       qid = 'lateraldischarge'
       success = adduniformtimerelation_objects(qid, '', 'lateral', locid, 'flow', trim(rec), numlatsg, kx, qplat)

       if (success) then
          jaqin = 1
       end if

    case default       ! Unrecognized item in a ext block
       ! initboundaryblocksforcings remains unchanged: Not an error (support commented/disabled blocks in ext file)
       write(msgbuf, '(5a)') 'Unrecognized block in file ''', trim(filename), ''': [', trim(groupname), ']. Ignoring this block.'
       call warn_flush()
    end select
 end do

 if (numlatsg > 0) then
    if (allocated (balat) ) deallocate(balat)
    allocate ( balat(numlatsg)  , stat=ierr    )
    call aerr('balat(numlatsg)' , ierr, numlatsg ); balat = 0d0
    do k = 1,ndx
       n = nnlat(k)
       if (n > 0) then 
          balat(n) = balat(n) + ba(k)
       endif   
    enddo   
    deallocate(kclat) 
 end if
 
 call tree_destroy(bnd_ptr)
 if (allocated(thrtt)) then 
    call init_threttimes()
 endif
 
end function initboundaryblocksforcings


!> Initializes memory for laterals on flow nodes.
subroutine ini_alloc_laterals()
   use m_wind
   use m_flowgeom, only: ndx
   use m_alloc
   integer :: ierr

   if (.not. allocated(QQlat) ) then                      ! just once
      allocate ( QQLat(ndx) , stat=ierr) ; QQLat = 0d0
      call aerr('QQLAT(ndx)', ierr, ndx)
      allocate ( nnLat(ndx) , stat=ierr) ; nnLat = 0  
      call aerr('nnLat(ndx)', ierr, ndx)
   endif
   if (.not. allocated(kcLat) ) then 
      allocate ( kcLat(ndx) , stat=ierr)                  ! only if needed  
      call aerr('kcLat(ndx)', ierr, ndx)
   endif
end subroutine ini_alloc_laterals


!> Prepare the 'kclat' mask array for a specific type of lateral.
subroutine prepare_lateral_mask(kc, ilattype)
   use m_flowgeom
   use m_wind
   implicit none
   
   integer         , intent(inout) :: kc(:) !< (ndx) The mask array that is to be filled.
   integer         , intent(in)    :: ilattype !< Type of the new lateral (one of ILATTP_1D|2D|1D2D)
   
   integer                         :: L, k1, k2

   kc = 0
   select case (ilattype)
   case (ILATTP_1D)       ! in everything 1D
      do L = 1,lnx1D
         !if (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0 ) then ! no pipes pos or neg, others only if pos
            k1 = ln(1,L) ; kc(k1) = 1
            k2 = ln(2,L) ; kc(k2) = 1
         !endif   
      enddo   
   case (ILATTP_2D)       ! in everything 2D
      do L = lnx1D+1,lnxi
         k1 = ln(1,L) ; kc(k1) = 1
         k2 = ln(2,L) ; kc(k2) = 1
      enddo   
   case (ILATTP_ALL)      ! both to everything 2D, and 1D, except to 1D pipes
      do L = 1,lnx1D
         if (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0 ) then ! no pipes pos or neg, others only if pos
            k1 = ln(1,L) ; kc(k1) = 1
            k2 = ln(2,L) ; kc(k2) = 1
         endif   
      enddo   
      do L = lnx1D+1,lnxi
         k1 = ln(1,L) ; kc(k1) = 1
         k2 = ln(2,L) ; kc(k2) = 1
      enddo   
   end select
end subroutine prepare_lateral_mask


!> Calls the ec_addtimespacerelation with all proper dflowfm-specific
!! target arrays and element set masks for object parameters with
!! spatially uniform time series.
!! Also handles inside one function the old-style *.ext quantities and
!! the new style *.ext and structures.ini quantities.
function adduniformtimerelation_objects(qid, locationfile, objtype, objid, paramname, paramvalue, targetindex, vectormax, targetarray) result(success)
   !use m_flowexternalforcings, no1=>qid, no2=>filetype, no3=>operand, no4 => success
   use m_meteo, no5=>qid, no6=>filetype, no7=>operand, no8 => success
   use string_module, only: strcmpi
   use timespace_parameters, only: uniform, spaceandtime
   use unstruc_messages
   
   implicit none

   character(len=*), intent(inout) :: qid            !< Identifier of current quantity (i.e., 'waterlevelbnd')
   character(len=*), intent(in)    :: locationfile   !< Name of location file (*.pli or *.pol) for current quantity (leave empty when valuestring contains value or filename).
   character(len=*), intent(in)    :: objtype        !< Type name of the object for which this relation is set (e.g., 'lateral', for prettyprinting only).
   character(len=*), intent(in)    :: objid          !< Id of the object for which this relation is set (for prettyprinting only).
   character(len=*), intent(in)    :: paramname      !< Name of the parameter that is set in this relation (e.g., 'flow', for prettyprinting only).
   character(len=*), intent(in)    :: paramvalue     !< String containing the parameter value (either a scalar double, or 'REALTIME', or a filename)
   integer,          intent(in)    :: targetindex    !< Target index in target value array (typically, the current count of this object type, e.g. numlatsg).
   integer,          intent(in)    :: vectormax      !< The number of values per object ('kx'), typically 1.
   double precision, intent(inout) :: targetarray(:) !< The target array in which the value(s) will be stored. Either now with scalar, or later via ec_gettimespacevalue() calls.
   logical                         :: success        !< Return value. Whether relation was added successfully.

   character(len=256) :: valuestring, fnam
   double precision   :: valuedble
   double precision   :: xdum(1), ydum(1)
   integer            :: kdum(1)
   integer            :: ierr, L


   success = .true.   ! initialization
   xdum = 1d0 ; ydum = 1d0; kdum = 1

   if (len_trim(paramvalue) > 0) then
      valuestring = paramvalue
   else if (len_trim(locationfile) > 0) then
      ! Old-style *.ext:
      ! Prepare time series relation, if the .pli file has an associated .tim file.
      L = index(locationfile,'.', back=.true.) - 1
      valuestring = locationfile(1:L)//'_0001.tim'
   else 
      ! TODO: AvD: error msg?
      success = .false.
   end if

   ! Now check the valuestring for either scalar/REALTIME/.tim filename
   read(valuestring, *, iostat = ierr) valuedble
   if (ierr /= 0) then ! No number, so check for timeseries filename
      if (strcmpi(trim(valuestring), 'REALTIME')) then
         success = .true.
         ! targetarray(targetindex) should be filled via DLL's API
         write(msgbuf, '(a,a,a,a,a)') 'Control for ', trim(objtype), '''' // trim(objid) // ''', ', paramname, ' set to REALTIME.'
         call dbg_flush()
      else
         fnam = trim(valuestring)
         ! Time-interpolated value will be placed in target array (e.g., qplat(n)) when calling ec_gettimespacevalue.
         if (index(trim(fnam)//'|','.tim|')>0) then 
            ! uniform=single time series vectormax = 1
            success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, vectormax, fnam, filetype=uniform, method=spaceandtime, operand='O', targetIndex=targetindex)
         endif 
         ! TODO: AvD: support .bc
      end if
   else
      targetarray(targetindex) = valuedble ! Constant value for always, set it now already.
   end if
end function adduniformtimerelation_objects


subroutine register_quantity_pli_combination(quantity, locationfile)
   use m_alloc
   implicit none
   character(len=*), intent(in)          :: quantity
   character(len=*), intent(in)          :: locationfile   
   character(len=max_registered_item_id) :: item_id

   item_id = trim(quantity) // '-' // trim(locationfile)
   
   if (num_registered_items >= max_ext_bnd_items) then
      max_ext_bnd_items = ceiling(1.2*num_registered_items)
      call realloc(registered_items, max_ext_bnd_items, keepExisting = .true., fill='')
   end if

   num_registered_items = num_registered_items + 1
   registered_items(num_registered_items) = item_id

end subroutine register_quantity_pli_combination

subroutine init_registered_items()
   implicit none
   num_registered_items = 0

   max_ext_bnd_items = 64 ! Default start size.
   if (allocated(registered_items)) deallocate(registered_items)
   allocate(registered_items(max_ext_bnd_items))

   registered_items(1:max_ext_bnd_items) = ''

end subroutine

function quantity_pli_combination_is_registered(quantity, locationfile) result(is_registered)
   implicit none
   logical                               :: is_registered
   character(len=*),intent(in)           :: quantity
   character(len=*),intent(in)           :: locationfile   
   integer                               :: i
   character(len=max_registered_item_id) :: item_id

   item_id = trim(quantity) // '-' // trim(locationfile)

   is_registered = .false.
   
   do i = 1, num_registered_items
      if (item_id == registered_items(i)) then
         is_registered = .true.
         return
      endif
   enddo

end function quantity_pli_combination_is_registered

subroutine init_threttimes()

 use m_flow
 use m_flowgeom
 use m_flowexternalforcings
 use m_transport
 use m_sediment, only: stm_included
 use unstruc_messages
 use m_missing
 
 implicit none
 
 integer             :: thrtlen, i, j, nseg, itrac, ifrac, iconst, n, ierr
 character(len=256)  :: qidfm, tracnam, sedfracnam, qidnam
 integer, external   :: findname
 
 if(jatransportmodule == 0) then
    return
 endif
 
 ! deallocation of TH arrays
 if(allocated(threttim)) then 
    deallocate(threttim)
 endif
 
 if(nopenbndsect==0) then 
    return
 endif
 
 allocate(threttim(NUMCONST,nopenbndsect), stat=ierr)
 call aerr('threttim(NUMCONST,nopenbndsect)', ierr, nopenbndsect)
 threttim = 0
 
 ! assign return times using temp arrays
 thrtlen = size(thrtt)
 do i = 1, thrtlen
    qidfm = thrtq(i)
    if(qidfm == 'salinitybnd' .and. allocated(kbnds)) then
       nseg = kbnds(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' salinity boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       threttim(ISALT,nseg) = thrtt(i)
    else if(qidfm == 'temperaturebnd' .and. allocated(kbndtm)) then
       nseg = kbndtm(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' temperature boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       threttim(ITEMP,nseg) = thrtt(i)
    else if(qidfm == 'sedimentbnd' .and. allocated(kbndsd) .and. .not. stm_included) then
       nseg = kbndsd(5,thrtn(i))
       if (nseg /=i) cycle
       if (nseg == 0 .or. nseg > nopenbndsect) then
          write(msgbuf,'(i8,a)') thrtn(i), ' sediment boundary point is assigned to incorrect boundary segment' ; call err_flush()
          cycle
       endif
       do j = ISED1, ISEDN
         threttim(j,nseg) = thrtt(i)
       enddo
    else if(qidfm(1:9) == 'tracerbnd') then
       call get_tracername(qidfm, tracnam, qidnam)
       itrac = findname(numtracers, trnames, tracnam)
       if (allocated(bndtr).and.thrtn(i)<=nbndtr(itrac) ) then
          nseg = bndtr(itrac)%k(5,thrtn(i))
          if (nseg /=i) cycle
          if (nseg == 0 .or. nseg > nopenbndsect) then
             write(msgbuf,'(i8,a)') thrtn(i), ' tracer boundary point is assigned to incorrect boundary segment' ; call err_flush()
             cycle
          endif
          iconst = itrac2const(itrac)
          threttim(iconst,nseg) = thrtt(i)
       endif
    else if(qidfm(1:10) == 'sedfracbnd') then
       ierr = 0
       call get_sedfracname(qidfm, sedfracnam, qidnam)
       ifrac = findname(numfracs, sfnames, sedfracnam)
       if (allocated(bndsf)) then
          nseg = bndsf(ifrac)%k(5,thrtn(i))
          if (nseg /=i) cycle
          if (nseg == 0 .or. nseg > nopenbndsect) then
             ierr = 1
          endif
          iconst = ifrac2const(ifrac)
          threttim(iconst,nseg) = thrtt(i)
       else
           ierr = 1
       endif
       if( ierr /= 0 ) then
           write(msgbuf,'(i8,a)') thrtn(i), ' sedfrac boundary point is assigned to incorrect boundary segment' ; call err_flush()
           cycle
       endif
    endif
 enddo
 
 if(allocated(thtbnds)) deallocate(thtbnds)
 if(allocated(thzbnds)) deallocate(thzbnds)
 if(allocated(thtbndtm)) deallocate(thtbndtm)
 if(allocated(thzbndtm)) deallocate(thzbndtm)
 if(allocated(thtbndsd)) deallocate(thtbndsd)
 if(allocated(thzbndsd)) deallocate(thzbndsd)
 
 allocate(thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd), stat=ierr)
 call aerr('thtbnds(nbnds), thzbnds(nbnds*kmxd), thtbndtm(nbndtm), thzbndtm(nbndtm*kmxd), thtbndsd(nbndsd), thzbndsd(nbndsd*kmxd)', ierr, (kmxd+1)*(nbnds+nbndtm+nbndsd))
 thzbnds = DMISS
 
 do i = 1,nbnds
    thtbnds(i) = threttim(ISALT,kbnds(5,i))
 enddo
 
 do i = 1,nbndtm
    thtbndtm(i) = threttim(ITEMP,kbndtm(5,i))
 enddo
 
 do i = 1,nbndsd
    thtbndsd(i) = threttim(ISED1,kbndsd(5,i))
 enddo
 
 if (allocated(bndtr)) then
    do itrac = 1, numtracers
       iconst = itrac2const(itrac)

       if(allocated(bndtr(itrac)%tht)) deallocate(bndtr(itrac)%tht)
       if(allocated(bndtr(itrac)%thz)) deallocate(bndtr(itrac)%thz)
    
       n = nbndtr(itrac)
    
       allocate(bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd), stat=ierr)
       call aerr('bndtr(itrac)%tht(n), bndtr(itrac)%thz(n*kmxd)', ierr, n*(kmxd+1))

       do i = 1,n
         bndtr(itrac)%tht(i) = threttim(iconst,bndtr(itrac)%k(5,i))
       enddo
    enddo
 endif
 
 do ifrac = 1, numfracs
    if(allocated(bndsf(ifrac)%tht)) deallocate(bndsf(ifrac)%tht)
    if(allocated(bndsf(ifrac)%thz)) deallocate(bndsf(ifrac)%thz)
    
    n = nbndsf(ifrac)
    
    allocate(bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd), stat=ierr)
    call aerr('bndsf(ifrac)%tht(n), bndsf(ifrac)%thz(n*kmxd)', ierr, n*(kmxd+1))

    ! mapping to constituents, just in case fracs do not map sequentially to ised1 and so on
    iconst = ifrac2const(ifrac)
    do i = 1,n
      bndsf(ifrac)%tht(i) = threttim(iconst,bndsf(ifrac)%k(5,i))
    enddo
 enddo
       
end subroutine

end module unstruc_boundaries

function flow_initwaveforcings_runtime() result(retval)              ! This is the general hook-up to wave conditions

 !use m_flowexternalforcings
 use m_flowparameters
 use m_flowtimes                                     ! Two stages: 1 = collect elsets for which data is provided
 use m_flowgeom                                      !             2 = add relations between elsets and their providers
 !use m_netw
 use unstruc_model
 use unstruc_messages
 use unstruc_files
 use timespace
 use m_missing
 use m_waves
 !use m_ship
 !use m_flow, only : teta, frcu, frculin, jafrculin, viusp, javiusp, vicouv,    &
 !                   ifrcutp, frcuni, ifrctypuni, s1, sa1, satop, kmx
 !use m_observations
 use m_alloc
 use m_meteo

 implicit none

 logical               :: retval !< Whether init was successful or not

 integer               :: ierr
 integer               :: filetype_l
 integer               :: method_l
 character(1)          :: operand_l
 character(256)        :: qid_l

 if (extfor_wave_initialized) then
    retval = .true.
    return
 end if
    

 filetype_l = 14  ! netcdf
 method_l   = 7   ! only time interpolation, extrapolation allowed (online WAVE)
 operand_l  = 'O' ! Override
 kx = 1           ! default vectormax = 1
 !
 qid_l = 'hrms'
 if (.not. allocated(hwav) ) then
    allocate ( hwav(ndx), stat=ierr) ; hwav = 0.0
    call aerr('hwav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 if (.not.success) then
    !
    ! Most commonly, WAVE data has not been written to the com-file yet.
    ! Just try it the next timestep again
    !
    retval = .false.
    goto 888
 endif
 !
 if (jatpwav == TPWAVSMOOTH) then
    ! take smoothed peak wave period. Arjen: "Deze parameter is beter"
    qid_l = 'tps'
 elseif (jatpwav == TPWAVRELATIVE) then
    ! take relative peak wave period. Bas; scale factor required!!
    qid_l = 'rtp'
 else
    qid_l = 'tp'
 endif
 if (.not. allocated(twav) ) then
    allocate ( twav(ndx), stat=ierr) ; twav = 0.0
    call aerr('twav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'dir'
 if (.not. allocated(phiwav) ) then
    allocate ( phiwav(ndx), stat=ierr) ; phiwav = 0.0
    call aerr('phiwav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'dissurf'
 if (.not. allocated(dsurf) ) then
    allocate ( dsurf(ndx), stat=ierr) ; dsurf = 0.0
    call aerr('dsurf(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'diswcap'
 if (.not. allocated(dwcap) ) then
    allocate ( dwcap(ndx), stat=ierr) ; dwcap = 0.0
    call aerr('dwcap(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'fx'
 if (.not. allocated(sxwav) ) then
    allocate ( sxwav(ndx), stat=ierr) ; sxwav = 0.0
    call aerr('sxwav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'fy'
 if (.not. allocated(sywav) ) then
    allocate ( sywav(ndx), stat=ierr) ; sywav = 0.0
    call aerr('sywav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'wsbu'
 if (.not. allocated(sbxwav) ) then
    allocate ( sbxwav(ndx), stat=ierr) ; sbxwav = 0.0
    call aerr('sbxwav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'wsbv'
 if (.not. allocated(sbywav) ) then
    allocate ( sbywav(ndx), stat=ierr) ; sbywav = 0.0
    call aerr('sbywav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'mx'
 if (.not. allocated(mxwav) ) then
    allocate ( mxwav(ndx), stat=ierr) ; mxwav = 0.0
    call aerr('mxwav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'my'
 if (.not. allocated(mywav) ) then
    allocate ( mywav(ndx), stat=ierr) ; mywav = 0.0
    call aerr('mywav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 qid_l = 'ubot'
 if (.not. allocated(hwav) ) then
    allocate ( uorbwav(ndx), stat=ierr) ; uorbwav = 0.0
    call aerr('uorbwav(ndx)', ierr, ndx)
 endif
 success = ec_addtimespacerelation(qid_l, xz(1:ndx), yz(1:ndx), kcw, kx, md_wavefile, filetype_l, method_l, operand_l, quiet=.true.)
 !
 retval = success

888 continue
 extfor_wave_initialized = retval ! Becomes .true. or .false., depending on whether the timespace relations have been created succesfully.

end function flow_initwaveforcings_runtime

 

!> Initializes controllers that force structures.
!! Currently only time series files, in the future also realtime control (RTC).
subroutine flow_init_structurecontrol()
use m_flowexternalforcings
use m_alloc
use m_flowgeom
use m_netw
use unstruc_messages
use unstruc_channel_flow
use m_structures ! Jan's channel_flow for Sobek's generalstructure (TODO)
use m_strucs     ! Herman's generalstructure
use tree_structures
! use unstruc_files
use timespace
use m_missing
! use m_ship
! use m_alloc
use m_meteo
use m_readstructures
use m_sferic
use geometry_module
USE gridoperations, only: incells
 
implicit none
character(len=256)            :: plifile
integer                       :: i, L, Lf, kb, LL, ierr, k, kbi, n, ifld
integer                       :: nstr
character (len=256)           :: fnam, rec
integer, allocatable          :: pumpidx(:), gateidx(:), cdamidx(:), cgenidx(:), dambridx(:) ! temp
double precision              :: tmpval
integer                       :: istru, istrtype, itmp
integer                       :: numg, numd, npum, ngs, numgen, numgs, ilinstr, ndambr
type(TREE_DATA), pointer      :: str_ptr
double precision, allocatable :: widths(:)
double precision              :: widthtot
integer, allocatable          :: strnums(:)
double precision, allocatable :: xdum(:), ydum(:)
integer, allocatable          :: kdum(:)
character(len=IdLen)          :: strid ! TODO: where to put IdLen (now in MessageHandling)
character(len=IdLen)          :: strtype ! TODO: where to put IdLen (now in MessageHandling)
                                    ! TODO: in readstruc* change incoming ids to len=*
integer :: istrtmp
double precision, allocatable :: hulp(:,:) ! hulp 

! dambreak
double precision              :: x_breach, y_breach, distemp
double precision              :: xc, yc, xn, yn   
integer                       :: nDambreakCoordinates, k3, k4, kpol, indexInStructure, indexLink, ja, Lstart
double precision              :: xla, xlb, yla, ylb
integer, allocatable          :: lftopol(:)
double precision, allocatable :: xl(:), yl(:)

!! if (jatimespace == 0) goto 888                      ! Just cleanup and close ext file.

ngs = 0 ! Local counter for all crossed flow liks by *all* general structures.
nstr = tree_num_nodes(strs_ptr) ! TODO: minor issue: will count *all* children in structure file.
if (nstr > 0) then
   jaoldstr = 0
else
   jaoldstr = 1 ; RETURN ! DEZE SUBROUTINE IS EEN KOPIE VAN MIJN CODE EN DAT BRENGT ME IN DE WAR
                         ! DAAROM VOORLOPIG UIT UNSTRUC.F90 VERPLAATST
end if

if (allocated(strnums)) deallocate(strnums)
if (allocated(widths)) deallocate(widths)
if (allocated(lftopol)) deallocate(lftopol)
if (allocated(dambreakLinksEffectiveLength)) deallocate(dambreakLinksEffectiveLength)
if (allocated(pumpidx)) deallocate(pumpidx)
if (allocated(gateidx)) deallocate(gateidx)
if (allocated(cdamidx)) deallocate(cdamidx)
if (allocated(cgenidx)) deallocate(cgenidx)
if (allocated(dambridx)) deallocate(dambridx)
if (allocated(dambreakPolygons)) deallocate(dambreakPolygons)

allocate(strnums(numl))
allocate(widths(numl))
allocate(lftopol(numl))
allocate(dambreakLinksEffectiveLength(numl))
allocate(pumpidx(nstr))
allocate(gateidx(nstr))
allocate(cdamidx(nstr))
allocate(cgenidx(nstr))
allocate(dambridx(nstr))
allocate(dambreakPolygons(nstr))
!initialize the index
dambridx = -1
do i=1,nstr
   str_ptr => strs_ptr%child_nodes(i)%node_ptr

   success = .true.
   
   ! check if this structure concerns Flow1D type structure
   call prop_get_string(str_ptr, '', 'branchid', strtype, success)
   if (success) then
      success = .true.
      call prop_get_string(str_ptr, '', 'type', strtype, success)
      
      if (trim(strtype) /= 'pump') then
         cycle
      endif
      
   endif
   call prop_get_string(str_ptr, '', 'filetype', strtype, success)
   if (success) then
      cycle
   endif
   
   strtype = ' '
   call prop_get_string(str_ptr, '', 'type',         strtype, success)
   if (.not. success .or. len_trim(strtype) == 0) then
      write(msgbuf, '(a,i0,a)') 'Required field ''type'' missing in structure #', i, '.'
      call warn_flush()
      cycle
   end if

   strid = ' '
   call prop_get_string(str_ptr, '', 'id', strid, success)
   if (.not. success .or. len_trim(strid) == 0) then
      write(msgbuf, '(a,i0,a)') 'Required field ''id'' missing in '//trim(strtype)//' #', i, '.'
      call warn_flush()
      cycle
   end if

   plifile = ' '
   call prop_get_string(str_ptr, '', 'polylinefile', plifile, success)
   if (.not. success .or. len_trim(plifile) == 0) then
      write(msgbuf, '(a,a,a)') 'Required field ''polylinefile'' missing in '//trim(strtype)//' ''', trim(strid), '''.'
      call warn_flush()
      cycle
   end if

   select case (strtype)
   case ('gateloweredgelevel')  ! Old-style controllable gateloweredgelevel
        !else if (qid == 'gateloweredgelevel' ) then

      call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, keg(ngate+1:numl), numg )
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , numg, ' nr of gateheight links' ; call msg_flush()


      ngatesg = ngatesg + 1
      gateidx(ngatesg) = i
      call realloc(L1gatesg,ngatesg) ; L1gatesg(ngatesg) = ngate + 1
      call realloc(L2gatesg,ngatesg) ; L2gatesg(ngatesg) = ngate + numg

      ngate   = ngate   + numg

   case ('damlevel') ! Old-style controllable damlevel
      ! else if (qid == 'damlevel' ) then

      call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, ked(ncdam+1:numl), numd )
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , numd, ' nr of dam level cells' ; call msg_flush()


      ncdamsg = ncdamsg + 1
      cdamidx(ncdamsg) = i
      call realloc(L1cdamsg,ncdamsg) ; L1cdamsg(ncdamsg) = ncdam + 1
      call realloc(L2cdamsg,ncdamsg) ; L2cdamsg(ncdamsg) = ncdam + numd

      ncdam   = ncdam   + numd

   case ('pump')
      call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, kep(npump+1:numl), npum )
      !endif
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , npum, ' nr of pump links' ; call msg_flush()


      npumpsg = npumpsg + 1
      pumpidx(npumpsg) = i
      call realloc(L1pumpsg,npumpsg) ; L1pumpsg(npumpsg) = npump + 1
      call realloc(L2pumpsg,npumpsg) ; L2pumpsg(npumpsg) = npump + npum

      npump   = npump   + npum

   case ('dambreak')

      call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, kedb(ndambreak+1:numl), ndambr, dambreakPolygons(i)%xp, dambreakPolygons(i)%yp, dambreakPolygons(i)%np, lftopol(ndambreak+1:numl))
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , ndambr, ' nr of dambreak links' ; call msg_flush()

      ndambreaksg = ndambreaksg + 1
      dambridx(ndambreaksg) = i
      call realloc(L1dambreaksg,ndambreaksg) ; L1dambreaksg(ndambreaksg) = ndambreak + 1
      call realloc(L2dambreaksg,ndambreaksg) ; L2dambreaksg(ndambreaksg) = ndambreak + ndambr

      ndambreak   = ndambreak   + ndambr


   case ('gate', 'weir', 'generalstructure') !< The various generalstructure-based structures
      call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, kegen(ncgen+1:numl), numgen )
      success = .true.
      WRITE(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(plifile) , numgen, ' nr of '//trim(strtype)//' cells' ; call msg_flush()

      ncgensg = ncgensg + 1
      cgenidx(ncgensg) = i
      call realloc(L1cgensg,ncgensg) ; L1cgensg(ncgensg) = ncgen + 1
      call realloc(L2cgensg,ncgensg) ; L2cgensg(ncgensg) = ncgen + numgen

      ncgen = ncgen + numgen
      !if(numgen > 0) then
      ! For later usage split up the set of all generalstructures into weirs, gates or true general structures (in user input)
         select case(strtype)
         case ('weir')
            nweirgen = nweirgen + 1
         case ('gate')
            ngategen = ngategen + 1
         case ('generalstructure')
            ngenstru = ngenstru + 1
         case default
            call mess(LEVEL_ERROR, 'Programming error: unhandled structure type '''//trim(strtype)//''' under general structure block.')
         end select
      !endif
   case ('generalstructuresobek') ! TODO: AvD: not hooked up yet.
      !call mess(LEVEL_ERROR, 'Programming error: structure type '''//trim(strtype)//''' not supported yet.')
      !cycle
      !call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, kegs(ngs+1:numl), numgs )
      !do LL=ngs+1,ngs+numgs
      !   L = kegs(LL)
      !   Lf = abs(L) ! flow link(s) crossed by this one general structure
      !   widths(LL-ngs) =  wu(Lf)
      !   success = readAndAddStructure(network, str_ptr, istru, istrtype, strid, L, ln(1,Lf), ln(2,Lf)) ! Note: pass link L including its +/- sign to handle orientation.
      !   strnums(LL-ngs) = istru
      !   if (.not. success) then
      !      write(msgbuf, '(a,a,a,a,a)') 'Failed to add structure ''', trim(strid), ''' of type ''', strtype, '''.'
      !      call err_flush()
      !      exit
      !   end if
      !end do
      !ilinstr = AddLineStructure(network%lns, network%sts, strnums(1:numgs), widths(1:numgs), numgs)
      !ngs = ngs + numgs
   case default
      call mess(LEVEL_WARN, 'flow_init_structurecontrol: unknown structure type '''//trim(strtype)//'''.')
   end select
end do

!if (ngate == 0) ngatesg = 0
!if (ncdam == 0) ncdamsg = 0
!if (npump == 0) npumpsg = 0
!if (ncgen == 0) ncgensg = 0
!if (ngs   == 0) ncgensg = 0 ! genstru sobek?

 allocate ( xdum(1), ydum(1), kdum(1) , stat=ierr)
 call aerr('xdum(1), ydum(1), kdum(1)', ierr, 3)
 xdum = 1d0 ; ydum = 1d0; kdum = 1
 
 if (ncgensg > 0) then  ! All generalstructure, i.e., the weir/gate/generalstructure user input
    if (allocated   (zcgen)   ) deallocate( zcgen)
    if (allocated   (kcgen)   ) deallocate( kcgen)
    kx = 3 ! 1: crest/sill, 2: gateloweredge, 3: width (?)
    allocate ( zcgen(ncgensg*kx), kcgen(4,ncgen) , stat=ierr     )
    call aerr('zcgen(ncgensg*kx), kcgen(4,ncgen)',ierr, ncgen*(2*kx+3) )
    kcgen = 0d0; zcgen = 1d10

    if (allocated(cgen_ids))     deallocate(cgen_ids)
    if (allocated(cgen_type))    deallocate(cgen_type)
    if (allocated(cgen2str))     deallocate(cgen2str)
    if (allocated(weir2cgen))    deallocate(weir2cgen)
    if (allocated(gate2cgen))    deallocate(gate2cgen)
    if (allocated(genstru2cgen)) deallocate(genstru2cgen)
    allocate(cgen_ids(ncgensg), cgen_type(ncgensg), cgen2str(ncgensg))
    allocate(weir2cgen(nweirgen), gate2cgen(ngategen), genstru2cgen(ngenstru))
    if (allocated(gates))        deallocate(gates)
    allocate (gates(ngategen) )

    nweirgen = 0
    ngategen = 0
    ngenstru = 0

   if (allocated(fusav)) deallocate(fusav)
   if (allocated(rusav)) deallocate(rusav)
   if (allocated(ausav)) deallocate(ausav)
   allocate( Fusav(2,ncgen), Rusav(2,ncgen), Ausav(2,ncgen) , stat = ierr ) ; Fusav = 0d0 ; Rusav = 0d0 ; ausav = 0d0

   do n = 1, ncgensg

       do k = L1cgensg(n), L2cgensg(n)
          Lf           = iabs(kegen(k))
          kb           = ln(1,Lf)
          kbi          = ln(2,Lf)
          if (kegen(k) > 0) then
             kcgen(1,k)   = kb
             kcgen(2,k)   = kbi
          else
             kcgen(1,k)   = kbi
             kcgen(2,k)   = kb
          end if

          kcgen(3,k)   = Lf
          kcgen(4,k)   = n              ! pointer to general structure signal nr n

          call setfixedweirscheme3onlink(Lf)
          iadv(Lf)     = 22             ! iadv = general
             
       enddo

    enddo

    allocate( hulp(numgeneralkeywrd,ncgensg) )
    hulp(1,1:ncgensg)  = 10  ! widthleftW1=10
    hulp(2,1:ncgensg)  = 0.0 ! levelleftZb1=0.0
    hulp(3,1:ncgensg)  = 10  ! widthleftWsdl=10
    hulp(4,1:ncgensg)  = 0.0 ! levelleftZbsl=0.0
    hulp(5,1:ncgensg)  = 10  ! widthcenter=10
    hulp(6,1:ncgensg)  = 0.0 ! levelcenter=0.0
    hulp(7,1:ncgensg)  = 10  ! widthrightWsdr=10
    hulp(8,1:ncgensg)  = 0.0 ! levelrightZbsr=0.0
    hulp(9,1:ncgensg)  = 10  ! widthrightW2=10
    hulp(10,1:ncgensg) = 0.0 ! levelrightZb2=0.0
    hulp(11,1:ncgensg) = 11  ! gateheight=11
    hulp(12,1:ncgensg) = 12  ! gateheightintervalcntrl=12
    hulp(13,1:ncgensg) = 1   ! pos_freegateflowcoeff=1
    hulp(14,1:ncgensg) = 1   ! pos_drowngateflowcoeff=1
    hulp(15,1:ncgensg) = 1   ! pos_freeweirflowcoeff=1
    hulp(16,1:ncgensg) = 1.0 ! pos_drownweirflowcoeff=1.0
    hulp(17,1:ncgensg) = 1.0 ! pos_contrcoeffreegate=0.6
    hulp(18,1:ncgensg) = 1   ! neg_freegateflowcoeff=1
    hulp(19,1:ncgensg) = 1   ! neg_drowngateflowcoeff=1
    hulp(20,1:ncgensg) = 1   ! neg_freeweirflowcoeff=1
    hulp(21,1:ncgensg) = 1.0 ! neg_drownweirflowcoeff=1.0
    hulp(22,1:ncgensg) = 1.0 ! neg_contrcoeffreegate=0.6
    hulp(23,1:ncgensg) = 0   ! extraresistance=0
    hulp(24,1:ncgensg) = 1.  ! dynstructext=1.
    hulp(25,1:ncgensg) = 1d10! gatedoorheight
    hulp(26,1:ncgensg) = 0.  ! door_opening_width=0
  
    if ( allocated(generalstruc) )   deallocate (generalstruc)
    allocate (generalstruc(ncgensg) )

    do n = 1, ncgensg
       if (L1cgensg(n) > L2cgensg(n)) then
          ! 0 flow links found for this gens, so cycle
!!!          cycle
       endif

      str_ptr => strs_ptr%child_nodes(cgenidx(n))%node_ptr

      strtype = ' '
      call prop_get_string(str_ptr, '', 'type',         strtype)

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      cgen_ids(n) = strid

      plifile = ' '
      call prop_get_string(str_ptr, '', 'polylinefile', plifile, success)

      ! Start with some general structure default params, and thereafter, make changes depending on actual strtype
      if (strtype /= 'generalstructure') then
         hulp(1, n) = huge(1d0)  ! widthleftW1=10
         hulp(2, n) = -huge(1d0) ! levelleftZb1=0.0
         hulp(3, n) = huge(1d0)  ! widthleftWsdl=10
         hulp(4, n) = -huge(1d0) ! levelleftZbsl=0.0
         hulp(5, n) = huge(1d0)  ! widthcenter=10
         hulp(6, n) = -huge(1d0) ! levelcenter=0.0
         hulp(7, n) = huge(1d0)  ! widthrightWsdr=10
         hulp(8, n) = -huge(1d0) ! levelrightZbsr=0.0
         hulp(9, n) = huge(1d0)  ! widthrightW2=10
         hulp(10,n) = -huge(1d0) ! levelrightZb2=0.0
         hulp(11,n) = 1d10! gateheight=11
         hulp(12,n) = 12  ! gateheightintervalcntrl=12
         hulp(13,n) = 1   ! pos_freegateflowcoeff=1
         hulp(14,n) = 1   ! pos_drowngateflowcoeff=1
         hulp(15,n) = 1   ! pos_freeweirflowcoeff=1
         hulp(16,n) = 1.0 ! pos_drownweirflowcoeff=1.0
         hulp(17,n) = 1.0 ! pos_contrcoeffreegate=0.6
         hulp(18,n) = 1   ! neg_freegateflowcoeff=1
         hulp(19,n) = 1   ! neg_drowngateflowcoeff=1
         hulp(20,n) = 1   ! neg_freeweirflowcoeff=1
         hulp(21,n) = 1.0 ! neg_drownweirflowcoeff=1.0
         hulp(22,n) = 1.0 ! neg_contrcoeffreegate=0.6
         hulp(23,n) = 0   ! extraresistance=0
         hulp(24,n) = 1.  ! dynstructext=1.
         hulp(25,n) = 1d10! gatedoorheight
         hulp(26,n) = 0d0 ! door_opening_width
      end if


      select case (strtype)
      !! WEIR !!
      case ('weir')
         rec = ' '
         call prop_get(str_ptr, '', 'crest_level', rec)
         if (.not. success .or. len_trim(rec) == 0) then
            write(msgbuf, '(a,a,a)') 'Required field ''crest_level'' missing in weir ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if
         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zcgen(1, 1+kx, ..) should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for weir ''', trim(strid), ''', crest_level set to REALTIME.'
               call dbg_flush()
            else
               qid = 'generalstructure' ! TODO: werkt dit als je de losse quantities (crest/gateloweredge/width) dezelfde id geeft, maar wel netjes correct veschillende offset?
               fnam = trim(rec)
               ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
               if (index(trim(fnam)//'|','.tim|')>0) then 
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif 
               if (index(trim(fnam)//'|','.cmp|')>0) then 
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif 
            end if
         else
            zcgen((n-1)*kx+1) = tmpval ! Constant value for always, set it now already.
            hulp(6, n)        = tmpval
         end if

         tmpval = dmiss
         call prop_get(str_ptr, '', 'lat_contr_coeff', tmpval)
         ! TODO: Herman/Jaco: this is not relevant anymore, using width (gate only)??
         if (tmpval /= dmiss) then
            hulp(13,n) = tmpval
            hulp(14,n) = tmpval
            hulp(15,n) = tmpval
            hulp(16,n) = tmpval
            hulp(17,n) = 1.0
            hulp(18,n) = tmpval
            hulp(19,n) = tmpval
            hulp(20,n) = tmpval
            hulp(21,n) = tmpval
            hulp(22,n) = 1.0
         endif
         nweirgen = nweirgen+1
         weir2cgen(nweirgen) = n ! Mapping from 1:nweirgen to underlying generalstructure --> (1:ncgensg)
         cgen2str(n)         = nweirgen ! Inverse mapping
         cgen_type(n)        = ICGENTP_WEIR

      !! GATE !!
      case ('gate')
         rec = ' '
         call prop_get(str_ptr, '', 'sill_level', rec, success)
         if (.not. success .or. len_trim(rec) == 0) then
            write(msgbuf, '(a,a,a)') 'Required field ''sill_level'' missing in gate ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if

         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zcgen(1, 1+kx, ..) should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', sill_level set to REALTIME.'
               call dbg_flush()
            else
               qid = 'generalstructure'
               fnam = trim(rec)
               ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
               if (index(trim(fnam)//'|','.tim|')>0) then 
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif 
               if (index(trim(fnam)//'|','.cmp|')>0) then 
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif 
            end if
         else
            zcgen((n-1)*kx+1) = tmpval ! Constant value for always, set it now already.
            hulp(6, n)        = tmpval
         end if

         tmpval = dmiss
         call prop_get(str_ptr, '', 'sill_width', tmpval)
         if (.not. success .or. tmpval == dmiss) then
            ! Not required, just default to all crossed flow links
            tmpval = huge(1d0)
         end if
         gates(ngategen+1)%sill_width = tmpval

         tmpval = dmiss
         call prop_get(str_ptr, '', 'door_height', tmpval) ! Door height (from lower edge level to top, i.e. NOT a level/position)
         if (.not. success .or. tmpval == dmiss) then
            write(msgbuf, '(a,a,a)') 'Required field ''door_height'' missing in gate ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if
         gates(ngategen+1)%door_height = tmpval
         hulp(25,n) = tmpval  ! gatedoorheight.

         rec = ' '
         call prop_get(str_ptr, '', 'lower_edge_level', rec, success)
         if (.not. success .or. len_trim(rec) == 0) then
            write(msgbuf, '(a,a,a)') 'Required field ''lower_edge_level'' missing in gate ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if

         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zcgen(2, 2+kx, ..) should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', lower_edge_level set to REALTIME.'
               call dbg_flush()
            else
               qid = 'generalstructure'
               fnam = trim(rec)
               ! Time-interpolated value will be placed in zcgen((n-1)*3+2) when calling ec_gettimespacevalue.
               if (index(trim(fnam)//'|','.tim|')>0) then 
                   success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+2) ! Hook up 1 component at a time, even when target element set has kx=3
               endif 
               if (index(trim(fnam)//'|','.cmp|')>0) then 
                   success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+2) ! Hook up 1 component at a time, even when target element set has kx=3
               endif 
            end if
         else
            zcgen((n-1)*kx+2) = tmpval ! Constant value for always, set it now already.
            hulp(11, n)       = tmpval
         end if

         rec = ' '
         call prop_get(str_ptr, '', 'opening_width', rec, success) ! Opening width between left and right doors. (If any. Otherwise set to 0 for a single gate door with under/overflow)
         if (.not. success) then
            call prop_get(str_ptr, '', 'door_opening_width', rec, success) ! Better keyword: door_opening_width instead of opening_width
         end if
         if (len_trim(rec) == 0) then
            zcgen((n-1)*kx+3) = dmiss   ! door_opening_width is optional
            success = .true.
         else
            read(rec, *, iostat = ierr) tmpval
            if (ierr /= 0) then ! No number, so check for timeseries filename
               if (trim(rec) == 'REALTIME') then
                  success = .true.
                  ! zcgen(3, 3+kx, ..) should be filled via DLL's API
                  write(msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', opening_width set to REALTIME.'
                  call dbg_flush()
               else
                  qid = 'generalstructure' ! todo: check met Hermans gatewidth, if any
                  fnam = trim(rec)
                  ! Time-interpolated value will be placed in zcgen((n-1)*3+3) when calling ec_gettimespacevalue.
                  if (index(trim(fnam)//'|','.tim|')>0) then 
                      success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+3) ! Hook up 1 component at a time, even when target element set has kx=3
                  endif 
                  if (index(trim(fnam)//'|','.cmp|')>0) then 
                      success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+3) ! Hook up 1 component at a time, even when target element set has kx=3
                  endif 
               end if
            else
               zcgen((n-1)*kx+3) = tmpval ! Constant value for always, set it now already.
               hulp(5, n)       = tmpval
            end if
         end if

         rec = ' '
         call prop_get(str_ptr, '', 'horizontal_opening_direction', rec, success)
         success = .true. ! horizontal_opening_direction is optional
         select case(trim(rec))
         case ('from_left')
            istrtmp = IOPENDIR_FROMLEFT
         case ('from_right')
            istrtmp = IOPENDIR_FROMRIGHT
         case ('symmetric')
            istrtmp = IOPENDIR_SYMMETRIC
         case default
            istrtmp = IOPENDIR_SYMMETRIC
         end select
         gates(ngategen+1)%opening_direction = istrtmp

         ngategen = ngategen+1
         gate2cgen(ngategen) = n ! Mapping from 1:ngategen to underlying generalstructure --> (1:ncgensg)
         cgen2str(n)         = ngategen ! Inverse mapping
         cgen_type(n)        = ICGENTP_GATE


      !! GENERALSTRUCTURE !!
      case ('generalstructure')
         do k = 1,numgeneralkeywrd        ! generalstructure keywords
            tmpval = dmiss
            call prop_get(str_ptr, '', trim(generalkeywrd(k)), rec, success)
            if (.not. success .or. len_trim(rec) == 0) then
               ! consider all fields optional for now.
               cycle
            end if
            read(rec, *, iostat = ierr) tmpval
            if (ierr /= 0) then ! No number, so check for timeseries filename
               if (trim(rec) == 'REALTIME') then
                  select case (trim(generalkeywrd(k)))
                  case ('levelcenter', 'gatedoorheight', 'gateheight', 'door_opening_width')
                     success = .true.
                     write(msgbuf, '(a,a,a)') 'Control for generalstructure ''', trim(strid), ''', '//trim(generalkeywrd(k))//' set to REALTIME.'
                     call dbg_flush()
                  case default
                     success = .false.
                     call mess(LEVEL_ERROR, 'Programming error: general structure via structures.ini file does not support REALTIME control for '//trim(generalkeywrd(k)))
               end select
                  
                     
               else
                  success = .false.
                  select case (trim(generalkeywrd(k)))
                  case ('levelcenter')
                     ifld = 1
                  case ('gateheight') ! TODO: UNST-1936
                     ifld = 2
                  case ('door_opening_width')
                     ifld = 3
                  case default
                     success = .false.
                     call mess(LEVEL_ERROR, 'Programming error: general structure via structures.ini file does not yet support timeseries for '//trim(generalkeywrd(k)))
                     ifld = 0
                  end select
                  if (ifld > 0) then
                     ! Time-interpolated value will be placed in zcgen((n-1)*3+...) when calling ec_gettimespacevalue.
                     qid = 'generalstructure'
                     fnam = trim(rec)
                     if (index(trim(fnam)//'|','.tim|')>0) then 
                         success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+ifld) ! Hook up 1 component at a time, even when target element set has kx=3
                     endif 
                     if (index(trim(fnam)//'|','.cmp|')>0) then 
                         success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+ifld) ! Hook up 1 component at a time, even when target element set has kx=3
                     endif 
                  end if
               end if
            else
               hulp(k,n) = tmpval ! Constant value for always, set it now already.
            end if
         end do

         ! Set some zcgen values to their initial scalar values (for example, zcgen((n-1)*3+1) is quickly need for updating bobs.)
         zcgen((n-1)*3+1) = hulp( 6, n) ! levelcenter 
         zcgen((n-1)*3+2) = hulp(11, n) ! gateheight  == 'gateloweredgelevel', really a level
         zcgen((n-1)*3+3) = hulp(26, n) ! door_opening_width 

         ngenstru = ngenstru+1
         genstru2cgen(ngenstru) = n ! Mapping from 1:ngenstru to underlying generalstructure --> (1:ncgensg)
         cgen2str(n)            = ngenstru ! Inverse mapping
         cgen_type(n)           = ICGENTP_GENSTRU
      end select

      widthtot = 0d0
      do k = L1cgensg(n), L2cgensg(n)
         L  = kegen(k)
         Lf = kcgen(3,k)
         widths(k-L1cgensg(n)+1) = wu(Lf)
         widthtot = widthtot + wu(Lf)
      enddo
      numgen = L2cgensg(n)-L1cgensg(n)+1

      call togeneral(n, hulp(:,n), numgen, widths(1:numgen))

    enddo

    deallocate( hulp )

endif ! generalstructure: weir, gate, or true generalstructure

if (ngate > 0) then ! Old-style controllable gateloweredgelevel

   if (allocated (kgate) ) then
      deallocate(zgate, kgate)
   endif

   if (allocated   (gate_ids)   ) deallocate( gate_ids)
   allocate (gate_ids(ngatesg))
   allocate ( zgate(ngatesg), kgate(3,ngate), stat=ierr)
   call aerr('zgate(ngatesg), kgate(3,ngate)', ierr, ngate*5)
   kgate = 0d0; zgate = 1d10
   kx = 1

   do n = 1, ngatesg

      do k = L1gatesg(n), L2gatesg(n)
         Lf           = iabs(keg(k))
         kb           = ln(1,Lf)
         kbi          = ln(2,Lf)
         kgate(1,k)   = kb
         kgate(2,k)   = kbi
         kgate(3,k)   = Lf

         call setfixedweirscheme3onlink(Lf)
      enddo

   enddo

 do n = 1, ngatesg ! and now add it (poly_tim xys have just been prepared in separate loop)
      str_ptr => strs_ptr%child_nodes(gateidx(n))%node_ptr

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      gate_ids(n) = strid

      plifile = ' '
      call prop_get_string(str_ptr, '', 'polylinefile', plifile, success)

      rec = ' '
      call prop_get(str_ptr, '', 'lower_edge_level', rec, success)
      if (.not. success .or. len_trim(rec) == 0) then
         write(msgbuf, '(a,a,a)') 'Required field ''lower_edge_level'' missing in gate ''', trim(strid), '''.'
         call warn_flush()
         cycle
      end if

      read(rec, *, iostat = ierr) tmpval
      if (ierr /= 0) then ! No number, so check for timeseries filename
         if (trim(rec) == 'REALTIME') then
            success = .true.
            ! zgate should be filled via DLL's API
            write(msgbuf, '(a,a,a)') 'Control for gateloweredgelevel ''', trim(strid), ''' set to REALTIME.'
            call dbg_flush()
         else
            qid = 'gateloweredgelevel'
            fnam = trim(rec)
            if (index(trim(fnam)//'|','.tim|')>0) then 
               ! Time-interpolated value will be placed in zgate(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
            endif 
            if (index(trim(fnam)//'|','.cmp|')>0) then 
               ! Evaluated harmonic signals value will be placed in zgate(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
            endif 
         end if
      else
         zgate(n) = tmpval ! Constant value for always, set it now already.
      end if

   enddo

endif ! Old style controllable gateloweredgelevel

if (ncdamsg > 0) then ! Old-style controllable damlevel
   if (allocated   (zcdam)   ) deallocate( zcdam)
   if (allocated   (kcdam)   ) deallocate( kcdam)

   if (allocated   (cdam_ids)   ) deallocate(cdam_ids)
   allocate (cdam_ids(ncdamsg))
   allocate ( zcdam(ncdamsg), kcdam(3,ncdam), stat=ierr)
   call aerr('zcdam(ncdamsg), kcdam(3,ncdam)', ierr, ncdam*5)
   kcdam = 0d0; zcdam = 1d10
   kx = 1

   do n = 1, ncdamsg

      do k = L1cdamsg(n), L2cdamsg(n)
         Lf           = iabs(ked(k))
         kb           = ln(1,Lf) ! TODO: HK: moeten we hier niet altijd de upstream kb pakken (af van sign(ked(k))?
         kbi          = ln(2,Lf)
         kcdam(1,k)   = kb
         kcdam(2,k)   = kbi
         kcdam(3,k)   = Lf

         call setfixedweirscheme3onlink(Lf)

      enddo

   enddo

   do n = 1, ncdamsg ! and now add it (poly_tim xys have just been prepared in separate loop)
      str_ptr => strs_ptr%child_nodes(cdamidx(n))%node_ptr

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      cdam_ids(n) = strid

      plifile = ' '
      call prop_get_string(str_ptr, '', 'polylinefile', plifile)

      rec = ' '
      call prop_get(str_ptr, '', 'crest_level', rec)
      read(rec, *, iostat = ierr) tmpval
      if (ierr /= 0) then ! No number, so check for timeseries filename
         if (trim(rec) == 'REALTIME') then
            success = .true.
            ! zcdam should be filled via DLL's API
            write(msgbuf, '(a,a,a)') 'Control for damlevel ''', trim(strid), ''' set to REALTIME.'
            call dbg_flush()
         else
            qid = 'damlevel'
            fnam = trim(rec)
            if (index(trim(fnam)//'|','.tim|')>0) then 
               ! Time-interpolated value will be placed in zcdam(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
            endif 
            if (index(trim(fnam)//'|','.cmp|')>0) then 
               ! Evaluated harmonic signals value will be placed in zcdam(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
            endif 
         end if
      else
         zcdam(n) = tmpval ! Constant value for always, set it now already.
      end if

   enddo
endif

!
! pumps, including staged pumps
!
if (npump > 0) then
   if (allocated   (qpump)   ) deallocate( qpump)
   if (allocated   (kpump)   ) deallocate( kpump)

   if (allocated   (pump_ids)   ) deallocate( pump_ids)
   allocate (pump_ids(npumpsg))
   allocate ( qpump(npumpsg), kpump(3,npump), stat=ierr)
   call aerr('qpump(npumpsg), kpump(3,npump)', ierr, npump*5)
   kpump = 0d0; qpump = 0d0
   kx = 1

   do n = 1, npumpsg

      do k = L1pumpsg(n), L2pumpsg(n)
         L             = kep(k)
         Lf            = iabs(L)
         if (L > 0) then
            kb         = ln(1,Lf)
            kbi        = ln(2,Lf)
         else
            kb         = ln(2,Lf)
            kbi        = ln(1,Lf)
         endif
         kpump(1,k)    = kb
         kpump(2,k)    = kbi
         kpump(3,k)    = L ! f
      enddo
   end do
   
   nPumpsWithLevels = 0
   
   if (allocated(pumpsWithLevels)) deallocate(pumpsWithLevels)
   allocate(pumpsWithLevels(npumpsg))
   pumpsWithLevels = -1;
   
   if (allocated(waterLevelsPumpLeft)) deallocate(waterLevelsPumpLeft)
   allocate(waterLevelsPumpLeft(npumpsg))
   waterLevelsPumpLeft = 0d0;
   
   if (allocated(waterLevelsPumpRight)) deallocate(waterLevelsPumpRight)
   allocate(waterLevelsPumpRight(npumpsg))
   waterLevelsPumpRight = 0d0;
   
   if (allocated(pumpAveraging)) deallocate(pumpAveraging)
   allocate(pumpAveraging(2,npumpsg))
   pumpAveraging = 0d0;

   ! initialize
   pumpsWithLevels = -1
   do n = 1, npumpsg ! and now add it (poly_tim xys have just been prepared in separate loop)

      str_ptr => strs_ptr%child_nodes(pumpidx(n))%node_ptr

      ! read the id first
      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      pump_ids(n) = strid

      ! read the type
      strtype = ' '
      call prop_get_string(str_ptr, '', 'type', strtype, success)
      istrtype  = getStructype(strtype)

      ! Do a try-read to determine whether this is a staged flow1d pump. If not, just continue (capacity is enough then).
      call prop_get_integer(str_ptr, 'structure', 'nrstages', itmp, success)
      if (success) then
         ! flow1d_io library: add and read SOBEK pump
         ! just use the first link of the the structure (the network%sts%struct(istrtmp)%link_number  is not used in computations)
         k = L1pumpsg(n)
         istrtmp   = addStructure(network%sts, kpump(1,k), kpump(2,k), iabs(kpump(3,k)), -1, "", strid, istrtype)
         call readPump(network%sts%struct(istrtmp)%pump, str_ptr, success)
      end if
      
      ! mapping for qpump array
      if (success) then
         nPumpsWithLevels   = nPumpsWithLevels + 1
         pumpsWithLevels(n) = istrtmp
      endif

      if (.not. success) then ! Original pump code, with only a capacity.

         plifile = ' '
         call prop_get_string(str_ptr, '', 'polylinefile', plifile)

         rec = ' '
         call prop_get(str_ptr, '', 'capacity', rec)
         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zgate should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for pump ''', trim(strid), ''' set to REALTIME.'
               call dbg_flush()
            else
               qid = 'pump'
               fnam = trim(rec)
               if (index(trim(fnam)//'|','.tim|')>0) then
                  ! Time-interpolated value will be placed in qpump(n) when calling ec_gettimespacevalue.
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n) 
                  if(.not.success) then
                     call qnerror( getECMessage() , ' for ',strid)
                  endif
               endif
               if (index(trim(fnam)//'|','.cmp|')>0) then
                  ! Evaluated harmonic signals value will be placed in qpump(n) when calling ec_gettimespacevalue.
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
                  if(.not.success) then
                     call qnerror( getECMessage() , ' for ',strid)
                  endif
               endif
            end if
         else
            qpump(n) = tmpval ! Constant value for always, set it now already.
            success = .true.
         end if
      end if
   enddo
endif

!
! dambreak
!
if (ndambreak > 0) then

   if (allocated(maximumDambreakWidths)) deallocate(maximumDambreakWidths)
   allocate(maximumDambreakWidths(ndambreak))
   maximumDambreakWidths = 0d0;
   
   if (allocated(kdambreak)) deallocate(kdambreak)
   allocate(kdambreak(3,ndambreak), stat=ierr) ! the last row stores the actual 
   kdambreak = 0d0; 
   
   if (allocated(dambreaks)) deallocate(dambreaks)
   allocate(dambreaks(ndambreaksg))
   dambreaks = 0
   
   if (allocated(LStartBreach)) deallocate(LStartBreach)
   allocate(LStartBreach(ndambreaksg))
   LStartBreach     = - 1
   
   if (allocated(waterLevelsDambreakDownStream)) deallocate(waterLevelsDambreakDownStream)
   allocate(waterLevelsDambreakDownStream(ndambreaksg))
   waterLevelsDambreakDownStream = 0.0d0
   
   if (allocated(waterLevelsDambreakUpStream)) deallocate(waterLevelsDambreakUpStream)
   allocate(waterLevelsDambreakUpStream(ndambreaksg))
   waterLevelsDambreakUpStream   = 0.0d0
   
   if (allocated(breachDepthDambreak)) deallocate(breachDepthDambreak)
   allocate(breachDepthDambreak(ndambreaksg))
   breachDepthDambreak           = 0.0d0
   
   if (allocated(breachWidthDambreak)) deallocate(breachWidthDambreak)
   allocate(breachWidthDambreak(ndambreaksg))
   breachWidthDambreak           = 0.0d0
   
   if (allocated(dambreak_ids)) deallocate(dambreak_ids)
   allocate(dambreak_ids(ndambreaksg))
   
   if(allocated(activeDambreakLinks)) deallocate(activeDambreakLinks)
   allocate(activeDambreakLinks(ndambreak))
   activeDambreakLinks = 0
   
   if(allocated(normalVelocityDambreak)) deallocate(normalVelocityDambreak)
   allocate(normalVelocityDambreak(ndambreaksg))
   normalVelocityDambreak = 0.0d0
   
   if(allocated(dambreakAveraging)) deallocate(dambreakAveraging)
   allocate(dambreakAveraging(2,ndambreaksg))
   dambreakAveraging = 0.0d0
   
   if(allocated(dambreakLevelsAndWidthsFromTable)) deallocate(dambreakLevelsAndWidthsFromTable)
   allocate(dambreakLevelsAndWidthsFromTable(ndambreaksg*2))
   dambreakLevelsAndWidthsFromTable = 0.0d0
   
   if(allocated(breachWidthDerivativeDambreak)) deallocate(breachWidthDerivativeDambreak)
   allocate(breachWidthDerivativeDambreak(ndambreaksg))
   breachWidthDerivativeDambreak = 0.0d0
   
   if(allocated(waterLevelJumpDambreak)) deallocate(waterLevelJumpDambreak)
   allocate(waterLevelJumpDambreak(ndambreaksg))
   waterLevelJumpDambreak = 0.0d0
   
   if(allocated(waterLevelJumpDambreak)) deallocate(waterLevelJumpDambreak)
   allocate(waterLevelJumpDambreak(ndambreaksg))
   waterLevelJumpDambreak = 0.0d0
   
   ! dambreak upstream
   if(allocated(dambreakLocationsUpstreamMapping)) deallocate(dambreakLocationsUpstreamMapping)
   allocate(dambreakLocationsUpstreamMapping(ndambreaksg))
   dambreakLocationsUpstreamMapping = 0.0d0
   
   if(allocated(dambreakLocationsUpstream)) deallocate(dambreakLocationsUpstream)
   allocate(dambreakLocationsUpstream(ndambreaksg))
   dambreakLocationsUpstream = 0.0d0
   
   if(allocated(dambreakAverigingUpstreamMapping)) deallocate(dambreakAverigingUpstreamMapping)
   allocate(dambreakAverigingUpstreamMapping(ndambreaksg))
   dambreakAverigingUpstreamMapping = 0.0d0
   
   nDambreakLocationsUpstream = 0
   nDambreakAveragingUpstream = 0
   
   ! dambreak downstream
   if(allocated(dambreakLocationsDownstreamMapping)) deallocate(dambreakLocationsDownstreamMapping)
   allocate(dambreakLocationsDownstreamMapping(ndambreaksg))
   dambreakLocationsDownstreamMapping = 0.0d0
   
   if(allocated(dambreakLocationsDownstream)) deallocate(dambreakLocationsDownstream)
   allocate(dambreakLocationsDownstream(ndambreaksg))
   dambreakLocationsDownstream = 0.0d0
   
   if(allocated(dambreakAverigingDownstreamMapping)) deallocate(dambreakAverigingDownstreamMapping)
   allocate(dambreakAverigingDownstreamMapping(ndambreaksg))
   dambreakAverigingDownstreamMapping = 0.0d0  

   nDambreakLocationsDownstream = 0
   nDambreakAveragingDownstream = 0

   do n = 1, ndambreaksg
      do k = L1dambreaksg(n), L2dambreaksg(n)
         L               = kedb(k)
         Lf              = iabs(L)
         if (L > 0) then
            kb           = ln(1,Lf)
            kbi          = ln(2,Lf)
         else
            kb           = ln(2,Lf)
            kbi          = ln(1,Lf)
         endif
         ! kdambreak
         kdambreak(1,k)  = kb
         kdambreak(2,k)  = kbi
         kdambreak(3,k)  = L   
      end do
   enddo
   
   ! number of columns in the dambreak hights and widths tim file
   kx = 2
   do n = 1, ndambreaksg

      !The index of the structure
      indexInStructure = dambridx(n)
      if (indexInStructure == -1 ) cycle
      
      str_ptr => strs_ptr%child_nodes(indexInStructure)%node_ptr

      ! read the id first
      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      dambreak_ids(n) = strid

      ! read the type
      strtype = ' '
      call prop_get_string(str_ptr, '', 'type', strtype, success)
      istrtype  = getStructype(strtype)
      ! flow1d_io library: add and read SOBEK dambreak
      ! just use the first link of the the structure (the network%sts%struct(istrtmp)%link_number is not used in computations)
      k = L1dambreaksg(n)
      istrtmp = addStructure(network%sts, kdambreak(1,k), kdambreak(2,k), iabs(kdambreak(3,k)), -1, "", strid, istrtype)
      call readDambreak(network%sts%struct(istrtmp)%dambreak, str_ptr, success)

      if (success) then
         ! new dambreak format
         write(msgbuf, '(a,a,a)') 'Dambreak ''', trim(strid), ''' set to new format.'
         call msg_flush()
         ! mapping
         dambreaks(n) = istrtmp
         ! set initial phase, width, crest level, coefficents if algorithm is 1
         network%sts%struct(istrtmp)%dambreak%phase  = 0
         network%sts%struct(istrtmp)%dambreak%width  = 0d0
         network%sts%struct(istrtmp)%dambreak%crl    = network%sts%struct(istrtmp)%dambreak%crestLevelIni
         if (network%sts%struct(istrtmp)%dambreak%algorithm == 3) then
            ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
            qid='dambreakLevelsAndWidths'
            network%sts%struct(istrtmp)%dambreak%levelsAndWidths = trim(network%sts%struct(istrtmp)%dambreak%levelsAndWidths)
            if (index(trim(network%sts%struct(istrtmp)%dambreak%levelsAndWidths)//'|','.tim|')>0) then   
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, network%sts%struct(istrtmp)%dambreak%levelsAndWidths , uniform, spaceandtime, 'O', targetIndex=n) ! Hook up 1 component at a time, even when target element set has kx=3
            else
               success = .false.
            endif            
         endif

         ! inquire if the water level upstream has to be taken from a location or be a result of averaging
         if (network%sts%struct(istrtmp)%dambreak%algorithm == 2) then
            xla = network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamLocationX
            yla = network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamLocationY
            if ((xla.ne.dmiss).and.(yla.ne.dmiss)) then
               call incells(xla,yla,k)
               if (k > 0) then
                  nDambreakLocationsUpstream = nDambreakLocationsUpstream + 1
                  dambreakLocationsUpstreamMapping(nDambreakLocationsUpstream) = n
                  dambreakLocationsUpstream(nDambreakLocationsUpstream) = k
               endif
            else
               nDambreakAveragingUpstream = nDambreakAveragingUpstream + 1
               dambreakAverigingUpstreamMapping(nDambreakAveragingUpstream) = n
            endif
         endif

         ! inquire if the water level downstream has to be taken from a location or be a result of averaging
         if (network%sts%struct(istrtmp)%dambreak%algorithm == 2) then
            xla = network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamLocationX
            yla = network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamLocationY
            if ((xla.ne.dmiss).and.(yla.ne.dmiss)) then
               call incells(xla,yla,k)
               if (k > 0) then
                  nDambreakLocationsDownstream = nDambreakLocationsDownstream + 1
                  dambreakLocationsDownstreamMapping(nDambreakLocationsDownstream) = n
                  dambreakLocationsDownstream(nDambreakLocationsDownstream) = k
               endif
            else
               nDambreakAveragingDownstream = nDambreakAveragingDownstream + 1
               dambreakAverigingDownstreamMapping(nDambreakAveragingDownstream) = n
            endif
         endif
         
      else
         ! old dambreak format
         write(msgbuf, '(a,a,a)') 'Dambreak ''', trim(strid), ''' could not be read. Perhaps missing fields in structure file?'
         call err_flush()
         cycle
      endif
      
      ! Project the start of the breach on the polyline, find xn and yn
      if(.not.allocated(dambreakPolygons(indexInStructure)%xp)) cycle
      if(.not.allocated(dambreakPolygons(indexInStructure)%yp)) cycle
     
      ! Create the array with the coordinates of the flow links
      if(allocated(xl)) deallocate(xl)
      if(allocated(yl)) deallocate(yl)
      nDambreakCoordinates = L2dambreaksg(n) - L1dambreaksg(n)  + 2
      allocate(xl(nDambreakCoordinates))
      allocate(yl(nDambreakCoordinates))
      indexLink = 0
      do k = L1dambreaksg(n), L2dambreaksg(n)
         indexLink = indexLink + 1
         ! compute the mid point
         Lf = iabs(kdambreak(3,k))
         k3 = lncn(1,Lf) 
         k4 = lncn(2,Lf)
         xl(indexLink)     = xk(k3)
         xl(indexLink + 1) = xk(k4)
         yl(indexLink)     = yk(k3)
         yl(indexLink + 1) = yk(k4)
      enddo
   
      ! comp_breach_point takes plain arrays to compute the breach point (also used in unstruct_bmi)      
      call comp_breach_point(network%sts%struct(istrtmp)%dambreak%startLocationX, & 
                             network%sts%struct(istrtmp)%dambreak%startLocationY, & 
                             dambreakPolygons(indexInStructure)%xp, & 
                             dambreakPolygons(indexInStructure)%yp, & 
                             dambreakPolygons(indexInStructure)%np, & 
                             xl, & 
                             yl, & 
                             Lstart, & 
                             x_breach, & 
                             y_breach, & 
                             jsferic, & 
                             jasfer3D,&
                             dmiss)
      
      LStartBreach(n) = L1dambreaksg(n) -  1  + Lstart 
      activeDambreakLinks(LStartBreach(n)) = 1
      
      ! compute the normal projections of the start and endpoints of the flow links
      do k = L1dambreaksg(n), L2dambreaksg(n)
         Lf = iabs(kdambreak(3,k))
         k3 = lncn(1,Lf)
         k4 = lncn(2,Lf)
         kpol = lftopol(k)
         xla = dambreakPolygons(indexInStructure)%xp(kpol)
         xlb = dambreakPolygons(indexInStructure)%xp(kpol + 1)
         yla = dambreakPolygons(indexInStructure)%yp(kpol)
         ylb = dambreakPolygons(indexInStructure)%yp(kpol + 1)
         
         call normalout( xla, yla, xlb, ylb, xn, yn, jsferic, jasfer3D, dmiss, dxymis)
         dambreakLinksEffectiveLength(k) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)
         dambreakLinksEffectiveLength(k) = dambreakLinksEffectiveLength(k) * abs( xn*csu(Lf) + yn*snu(Lf) )   
         ! Sum the length of the intersected flow links (required to bound maximum breach width)
         maximumDambreakWidths(n) = maximumDambreakWidths(n) + dambreakLinksEffectiveLength(k)
      enddo
      
      ! Now we can deallocate the polygon
      deallocate(dambreakPolygons(indexInStructure)%yp)
      deallocate(dambreakPolygons(indexInStructure)%xp)
   enddo
endif

! Cleanup:
888 continue
 if (mext > 0) then
!    call doclose(mext) ! close ext file
!    deallocate ( keg, ked, kep, kegs) ! TODO: AvD: cleanup now still done in initexternalforcings. Split off later, or not?
 end if

 if (allocated (xdum))     deallocate (xdum, ydum, kdum)
 if (allocated (kdss))     deallocate (kdss)

 if (allocated (strnums) ) deallocate (strnums)
 if (allocated (widths) )  deallocate (widths)
 if (allocated (pumpidx) ) deallocate (pumpidx)
 if (allocated (gateidx) ) deallocate (gateidx)
 if (allocated (cdamidx) ) deallocate (cdamidx)
 if (allocated (cgenidx) ) deallocate (cgenidx)
   end subroutine flow_init_structurecontrol
 

!> Returns the index of a structure in the controllable value arrays.
!! Structure is identified by strtypename, e.g. 'pumps', and structure name, e.g., 'Pump01'.
!! Returned index can be used to directly address variables like, m_flowexternalforcings::qpump, zgate, etc.
subroutine getStructureIndex(strtypename, strname, index)
! NOTE: this will only return the GUI-used structures (i.e., the new gates and weirs via general structure, not the old ext-based damlevel and gateloweredgelevel).
! TODO: longer-term all structure sets run via channel_flow and t_structureset, cleanup this function then.
   use m_flowexternalforcings
   implicit none
   character(len=*), intent(in)  :: strtypename !< the type of the structure: 'pumps', 'weirs', 'gates', ...
   character(len=*), intent(in)  :: strname     !< Id/name of the requested structure, e.g. 'Pump01'
   integer,          intent(out) :: index       !< Returned index of the found structure in its controllable value arrays.
 
   integer :: i, nstr, icgen
   integer, pointer :: cgen_mapping(:)
   index = 0

   if (trim(strtypename) == 'pumps') then
      do i=1,npumpsg
         if (trim(pump_ids(i)) == trim(strname)) then
            if (L2pumpsg(i) - L1pumpsg(i) >= 0) then
               ! Only return this pump index if pump is active in flowgeom (i.e., at least 1 flow link associated)
               index = i
               exit
            end if
         end if
      end do
   else if (trim(strtypename) == 'sourcesinks') then
      do i=1,numsrc
         if (trim(srcname(i)) == trim(strname)) then
            index = i
            exit
         end if
      end do
   else if (trim(strtypename) == 'dambreak') then
      do i=1,ndambreaksg
         if (trim(dambreak_ids(i)) == trim(strname)) then
            if (L2dambreaksg(i) - L1dambreaksg(i) >= 0) then
               ! Only return this dambreak index if dambreak is active in flowgeom (i.e., at least 1 flow link associated)
               index = i
               exit
            end if
         end if
      end do
   else
      select case(strtypename)
      case('weirs')
         cgen_mapping => weir2cgen
         nstr = nweirgen
      case('gates')
         cgen_mapping => gate2cgen
         nstr = ngategen
      case('generalstructures')
         cgen_mapping => genstru2cgen
         nstr = ngenstru
      case default
         nstr = 0
      end select

      do i=1,nstr
         icgen = cgen_mapping(i)
         if (trim(cgen_ids(icgen)) == trim(strname)) then
            if (L2cgensg(icgen) - L1cgensg(icgen) >= 0) then
               ! Only return this structure index if structure is active in flowgeom (i.e., at least 1 flow link associated)
               index = icgen
               exit
            end if
         end if
      end do
   end if

end subroutine getStructureIndex

!> Reads a key=value entry from a property block and tries to interpret the value.
!! The (single!) property block should come from an already-parsed .ini file.
!! The string value is always returned, if found, and an attempt is also made to
!! parse it into a scalar double, or alternatively to check whether it is an existing file.
subroutine read_required_property(prop_ptr, key, strvalue, dblvalue, is_double, typeandid, success)
   use properties
   use unstruc_messages
   implicit none
   type(TREE_DATA), pointer        :: prop_ptr   !< Property tree as read from a single .ini block
   character(len=*), intent(in)    :: key        !< Property key that should be read.
   character(len=*), intent(inout) :: strvalue   !< Returned string value for requested property key.
   double precision, intent(inout) :: dblvalue   !< Returned scalar double value for requested property key, IF possible.
   logical,          intent(out)   :: is_double  !< Tells whether the found value could be parsed into a scalar double value.
   character(len=*), intent(in)    :: typeandid  !< String with type and name, to be used in warning message to be printed if property key not found. Example: "gate 'Maeslant'"
   logical,          intent(out)   :: success    !< Whether value was read successfully or not.

   double precision :: tmpvalue
   integer :: ierr

   success   = .false.
   is_double = .false.
   
   call prop_get(prop_ptr, '', trim(key), strvalue, success)
   if (.not. success .or. len_trim(strvalue) == 0) then
      write(msgbuf, '(a,a,a,a,a)') 'Required field ''', trim(key), ''' missing in ', trim(typeandid), '.'
      call warn_flush()
      goto 888
   else
      read(strvalue, *, iostat = ierr) tmpvalue
      if (ierr == 0) then
         dblvalue = tmpvalue
         is_double = .true.
      end if
   end if

   success = .true.
888 continue

end subroutine read_required_property
   
subroutine flow_init_discharge()
   use properties
   implicit none
   type(TREE_DATA), pointer :: dis_ptr

   character(len=64) :: dis_type
   character(len=1024) :: rec

         rec = ' '
         ! [discharge]
         call prop_get(dis_ptr, '', 'id', rec)
         call prop_get(dis_ptr, '', 'polylinefile', rec)
         call prop_get(dis_ptr, '', 'type', dis_type) ! normal, momentum, walking, in-out
         !call prop_get(dis_ptr, '', 'interpolation', rec) ! linear, block
         
         !if (.not. success .or. len_trim(rec) == 0) then
         !   write(msgbuf, '(a,a,a)') 'Required field ''crest_level'' missing in weir ''', trim(strid), '''.'
         !   call warn_flush()
         !   cycle
         !end if
         !read(rec, *, iostat = ierr) tmpval
         !if (ierr /= 0) then ! No number, so check for timeseries filename
         !   if (trim(rec) == 'REALTIME') then
         !      success = .true.
         !      ! zcgen(1, 1+kx, ..) should be filled via DLL's API
         !      write(msgbuf, '(a,a,a)') 'Control for weir ''', trim(strid), ''', crest_level set to REALTIME.'
         !      call dbg_flush()
         !   else
         !      qid = 'generalstructure' ! TODO: werkt dit als je de losse quantities (crest/gateloweredge/width) dezelfde id geeft, maar wel netjes correct veschillende offset?
         !      fnam = trim(rec)
         !      ! Time-interpolated value will be placed in zcdam(n) when calling ec_gettimespacevalue.
         !      success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+1)
         !   end if
         !else
         !   zcdam((n-1)*kx+1) = tmpval ! Constant value for always, set it now already.
         !end if
         !
         !tmpval = dmiss
         !call prop_get(str_ptr, '', 'lat_contr_coeff', tmpval)
         !! TODO: Herman/Jaco: this is not relevant anymore, using width (gate only)??
         !
         !nweirgen = nweirgen+1
         !weir2cgen(nweirgen) = n ! Mapping from 1:nweirgen to underlying generalstructure --> (1:ncgensg)


end subroutine flow_init_discharge
