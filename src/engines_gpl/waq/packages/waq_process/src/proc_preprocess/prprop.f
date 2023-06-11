!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine prprop ( lunrep, laswi, config, no_act, actlst, allitems, procesdef,
     +                    noinfo, nowarn, old_items, ierror  )

!     Deltares Software Centre

!>/File
!>                fills proces properties from PB nefis tables

!     Created   : Nov   1992 by Jan van Beek

!     Modified  : Aug   2012 by Jan van Beek : licence check configurations moved from rd_tabs

      use m_zoek
      use m_srstop
      use timers         !< performance timers
      use processet      !< use processet definitions
      implicit none

      ! arguments

      integer           , intent(in   ) :: lunrep                 !< report file
      logical           , intent(inout) :: laswi                  !< active processes only switch
      character(len=*)  , intent(in   ) :: config                 !< requested configuration
      integer           , intent(in   ) :: no_act                 !< number of activated processes
      character(len=*)  , intent(in   ) :: actlst(*)              !< list of activated processes
      type(itempropcoll)                :: allitems               !< all items known
      type(procespropcoll)              :: procesdef              !< the proces definition
      integer           , intent(inout) :: noinfo                 !< cummulative information count
      integer           , intent(inout) :: nowarn                 !< cummulative warning count
      type(old_item_coll)               :: old_items              !< old_items table
      integer           , intent(  out) :: ierror                 !< error indicator
!
!     Common declarations
!
      INCLUDE 'data.inc'
!
!     Local declarations
!
      type(ProcesProp)      :: aProcesProp         ! one statistical proces definition
      type(ItemProp)        :: aItemProp           ! one item
      type(IOitemProp)      :: aIOitemProp         ! one IOitem
      type(IOitemPropColl)  :: input_item          ! one collection of items
      type(IOitemPropColl)  :: output_item         ! one collection of items
      type(IOitemPropColl)  :: FluxOutput          ! one collection of items
      type(StochiProp)      :: aStochiProp         ! one Stochi
      type(StochiPropColl)  :: FluxStochi          ! one collection of Stochis
      type(StochiPropColl)  :: VeloStochi          ! one collection of Stochis
      type(StochiPropColl)  :: DispStochi          ! one collection of Stochis
!
      INTEGER
     +                                       IPROC , IOFFSE,
     +               NAANTA, IINPU , IITEM , JNDEX , IOUTP ,
     +               IOFFS2, NAANT2, IDISP , IVELO , IOUTF ,
     +               ISTOC , IFLUX , IPRCNF, IGET  , IACT  ,
     +               ILIC  , IC
      INTEGER        iret
      REAL           ACTDEF
      LOGICAL        SWITUI, SWIT2D
      CHARACTER*80   LINE
      integer , allocatable :: itemidx(:)          ! index in the aleady existing item collection from the statistics
      integer               :: ierr_alloc
      integer               :: indx
      integer               :: i
      integer               :: ifound
      integer , allocatable :: actuse(:)           ! activated processes status indicator
      integer               :: iitem2              ! temporary index item
!
!     license stuff
!
      integer       iconf
      integer(4)                :: ithndl = 0      ! handle for performance timer
      if (timon) call timstrt( "prprop", ithndl )
!
!     Some init
!
      SWITUI = .FALSE.
      SWIT2D = .FALSE.
      IERROR = 0
!
!     get license infromation on configurations, removed always license
!
      liconf = 1
!
!     check licence for configuration if not in active only mode
!     MDK 27-05-2022: this is a remnant of an old licensing system. To be removed
!
      laswi = .true.

      if ( .not. laswi ) then
         call zoek   ( config, nconf , confid, 10    , iconf  )
         if ( iconf .le. 0 ) then
            write(lunrep,*)
     +       'error: configuration not found in process definition file'
            write(lunrep,*) 'configuration id:',config
            call srstop(1)
         else
            if ( liconf(iconf) .ne. 1 ) then
               write(lunrep,*)
     +         'error: no valid license found for configuration:',config
               call srstop(1)
            endif
         endif
      else
         iconf = 0
      endif
!
!     fill the items collection, add them to the statistical items check for doubles
!     first find then replace to speed up the process
!
      allocate(itemidx(nitem),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         WRITE ( lunrep , * ) 'error allocating work array in PRPROP:',ierr_alloc,nitem
         call srstop(1)
      endif
      do iitem = 1 , nitem
         aItemProp%name = itemid(iitem)
         itemidx(iitem) = ItemPropCollFind( AllItems, aItemProp )
      enddo
      do iitem = 1 , nitem
         aItemProp%name    = itemid(iitem)
         aItemProp%text    = itemnm(iitem)
         aItemProp%unit    = itemun(iitem)
         aItemProp%stdn    = itemsn(iitem)
         aItemProp%stdu    = itemsu(iitem)
         aItemProp%default = itemde(iitem)
         aItemProp%aggrega = itemag(iitem)
         aItemProp%disaggr = itemda(iitem)
         aItemProp%groupid = itemgr(iitem)
         aItemProp%segx    = itemse(iitem)
         aItemProp%wk      = itemwk(iitem)
         aItemProp%waqtype = WAQTYPE_UNKNOWN
         iret = itemidx(iitem)
         if ( iret .gt. 0 ) then
!
!           from statistics, replace content
!
            AllItems%ItempropPnts(iret)%pnt = aItemProp
         else
!
!           add item
!
            iret = ItemPropCollAdd( AllItems, aItemProp )
         endif
      enddo
      deallocate(itemidx)

      ! set old defaults

      do i = 1, old_items%cursize
         if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_DEFAULT ) then
            if ( old_items%old_items(i)%serial .gt. old_items%target_serial ) then
               aItemProp%name = old_items%old_items(i)%old_name
               iitem = ItemPropCollFind( AllItems, aItemProp )
               if ( iitem .gt. 0 ) then
                  write(lunrep,'(3a)') ' Replaced default [',old_items%old_items(i)%old_name,']'
                  write(lunrep,'(a,g13.6)') ' current value     :',AllItems%ItemPropPnts(iitem)%pnt%default
                  write(lunrep,'(a,g13.6)') ' replaced by       :',old_items%old_items(i)%old_default
                  if ( old_items%old_items(i)%serial .lt. 2100000000 ) then
                     write(lunrep,'(a,i13)') ' based on serial no:     :',old_items%old_items(i)%serial
                  else
                     write(lunrep,'(a)') ' based on old process definition'
                  endif
                  AllItems%ItemPropPnts(iitem)%pnt%default = old_items%old_items(i)%old_default
               endif
            endif
         endif
      enddo
!
!     loop over all processes
!
      allocate(actuse(no_act))
      do 800 iproc=1,nproc
!
!        Check if process is requested and licensed
!
         IF ( .NOT. LASWI ) THEN
            IPRCNF = (IPROC-1)*NCONF + ICONF
            IGET   = ICNPRO(IPRCNF)
         ELSE
            CALL ZOEK ( PROCID(IPROC), NO_ACT, ACTLST, 10    , IACT  )
            IF ( IACT .GT. 0 ) THEN
               IGET = 1
               ACTUSE(IACT) = 1
            ELSE
               IGET = 0
            ENDIF
         ENDIF
         IF ( IGET .GT. 0 ) THEN

            ! initialise process structure

            aProcesProp%name       = PROCID(IPROC)
            aProcesProp%text       = PROCNM(IPROC)
            aProcesProp%routine    = PROCFO(IPROC)
            aProcesProp%swtransp   = PROCCO(IPROC)
            aProcesProp%linvok     = .false.
            aProcesProp%active     = .false.
            aProcesProp%sfrac_type = SFRAC_SPLITFLUX

            input_item%maxsize  = 0
            output_item%maxsize = 0
            FluxOutput%maxsize  = 0
            DispStochi%maxsize  = 0
            VeloStochi%maxsize  = 0
            FluxStochi%maxsize  = 0
            input_item%cursize  = 0
            output_item%cursize = 0
            FluxOutput%cursize  = 0
            DispStochi%cursize  = 0
            VeloStochi%cursize  = 0
            FluxStochi%cursize  = 0

            ! input items on segment level/exchange level

            do iinpu = 1 , ninpu
               call zoek ( procid(iproc), 1, inpupr(iinpu), 10, jndex)
               if ( jndex .gt.0 ) then

                  ! get item

                  aItemProp%name = inpuit(iinpu)
                  iitem = ItemPropCollFind( AllItems, aItemProp )
                  if ( iitem .le. 0 ) then
                     write(lunrep,*) 'ERROR: unknown ITEM:',aItemProp%name
                     call srstop(1)
                  endif

                  ! check this name in the old items table

                  do i = 1, old_items%cursize
                     if ( old_items%old_items(i)%action_type .eq. ITEM_ACTION_PPEQUAL2 ) then
                        call zoek(old_items%old_items(i)%new_name, 1, inpuit(iinpu), 10, ifound)
                        if ( ifound .ge. 0 ) then
                           inpuit(iinpu) = old_items%old_items(i)%old_name
                           write(lunrep,'(7a)') ' Input item  [',old_items%old_items(i)%new_name,
     +                                          '] replaced by [',old_items%old_items(i)%old_name,
     +                                          '] for process [',procid(iproc),']'
                           aItemProp%name = inpuit(iinpu)
                           iitem2 = ItemPropCollFind( AllItems, aItemProp )
                           if ( iitem2 .le. 0 ) then
                              aItemProp = AllItems%ItemPropPnts(iitem)%pnt
                              aItemProp%name = old_items%old_items(i)%old_name
                              iitem = ItemPropCollAdd( AllItems, aItemProp )
                           else
                              iitem = iitem2
                           endif
                        endif
                     endif
                  enddo

                  if ( inpude(iinpu) .eq. 'Y' .or. switui ) then
                      actdef = AllItems%ItemPropPnts(iitem)%pnt%default
                  elseif ( inpude(iinpu) .eq. 'G' ) then
                      actdef = -888.
                  elseif ( inpude(iinpu) .eq. 'B' ) then
                      actdef = -101.
                  elseif ( inpude(iinpu) .eq. 'M' ) then
                      actdef = -11.
                  elseif ( inpude(iinpu) .eq. 'O' ) then
                      actdef = -1.
                  else
                      actdef = -999.
                  endif

                  aIOitemProp%item   =>AllItems%ItemPropPnts(iitem)%pnt
                  aIOitemProp%name   = AllItems%ItemPropPnts(iitem)%pnt%name
                  aIOitemProp%actdef = actdef
                  aIOitemProp%indx   = inpunm(iinpu)
                  aIOitemProp%ip_val = 0
                  if ( inpusx(iinpu) .eq. 1 ) then
                     aIOitemProp%type = IOTYPE_SEGMENT_INPUT
                  else
                     aIOitemProp%type = IOTYPE_EXCHANG_INPUT
                  endif
                  indx = inpunm(iinpu)
                  iret = IOitemPropCollAddIndx( input_item , aIOitemProp , indx )
               endif
            enddo

            ! output items on segment level/exchange level

            do ioutp = 1 , noutp
               call zoek ( procid(iproc), 1, outppr(ioutp),10 , jndex)
               if ( jndex .gt.0 ) then

                  ! lookup item in items table

                  aItemProp%name = outpit(ioutp)
                  iitem = ItemPropCollFind( AllItems, aItemProp )
                  if ( iitem .le. 0 ) then
                     write(lunrep,*) 'ERROR: unknown ITEM:',aItemProp%name
                     call srstop(1)
                  endif
                  aIOitemProp%item   =>AllItems%ItemPropPnts(iitem)%pnt
                  aIOitemProp%name   = AllItems%ItemPropPnts(iitem)%pnt%name
                  aIOitemProp%indx   = outpnm(ioutp)
                  aIOitemProp%ip_val = 0
                  if ( outpsx(ioutp) .eq. 1 ) then
                     aIOitemProp%type = IOTYPE_SEGMENT_OUTPUT
                  else
                     aIOitemProp%type = IOTYPE_EXCHANG_OUTPUT
                  endif
                  indx = outpnm(ioutp)
                  iret = IOitemPropCollAddIndx( output_item, aIOitemProp , indx )

                  if ( outpsx(ioutp) .ne. 1 ) then

                      ! scan disp table for lines associated with current output item on exchange level

                      if ( .not. swit2d ) then
                         do idisp = 1 , ndisp
                            call zoek ( outpit(ioutp)  , 1, dispit(idisp), 10, jndex)
                            if ( jndex .gt. 0 ) then
                               aStochiProp%type      = STOCHITYPE_DISPERSION
                               aStochiProp%ioitem    = dispit(idisp)
                               aStochiProp%substance = dispsu(idisp)
                               aStochiProp%subindx   = 0
                               aStochiProp%scale     = dispsc(idisp)
                               iret = StochiPropCollAdd( DispStochi , aStochiProp )
                            endif
                         enddo
                      endif

                      ! scan velo table for lines associated with current output item on exchange level

                      if ( .not. swit2d ) then
                         do ivelo = 1 , nvelo
                            call zoek ( outpit(ioutp)  , 1, veloit(ivelo), 10, jndex)
                            if ( jndex .gt. 0 ) then
                               aStochiProp%type      = STOCHITYPE_VELOCITY
                               aStochiProp%ioitem    = veloit(ivelo)
                               aStochiProp%substance = velosu(ivelo)
                               aStochiProp%subindx   = 0
                               aStochiProp%scale     = velosc(ivelo)
                               iret = StochiPropCollAdd( VeloStochi , aStochiProp )
                            endif
                         enddo
                      endif

                  endif

               endif
            enddo

            ! fluxes

            do ioutf = 1 , noutf
               call zoek ( procid(iproc), 1, outfpr(ioutf), 10, jndex)
               if ( jndex .gt.0 ) then

                  ! find and store flux properties

                  aItemProp%name = outffl(ioutf)
                  iflux = ItemPropCollFind( AllItems, aItemProp )
                  if ( iflux .le. 0 ) then
                     write(lunrep,*) 'ERROR: unknown ITEM:',aItemProp%name
                     ierror = 3
                     goto 900
                  endif
                  aIOitemProp%item   =>AllItems%ItemPropPnts(iflux)%pnt
                  aIOitemProp%name   = AllItems%ItemPropPnts(iflux)%pnt%name
                  aIOitemProp%indx   = outfnm(ioutf)
                  aIOitemProp%type   = IOTYPE_FLUX
                  aIOitemProp%ip_val = 0

                  indx = outfnm(ioutf)
                  iret = IOitemPropCollAddIndx( FluxOutput , aIOitemProp , indx )

                  ! scan stochi table for lines associated with present flux

                  do istoc = 1 , nstoc
                     call zoek ( outffl(ioutf), 1, stocfl(istoc), 10, jndex)
                     if ( jndex .gt.0 ) then
                        aStochiProp%type      = STOCHITYPE_FLUX
                        aStochiProp%ioitem    = stocfl(istoc)
                        aStochiProp%substance = stocsu(istoc)
                        aStochiProp%subindx   = 0
                        aStochiProp%scale     = stocsc(istoc)
                        iret = StochiPropCollAdd( FluxStochi , aStochiProp )
                     endif
                  enddo

               endif
            enddo

            ! process complete, add to the collection

            aProcesProp%no_input     = input_item%cursize
            aProcesProp%input_item   =>input_item%IOitemProps
            aProcesProp%no_output    = output_item%cursize
            aProcesProp%output_item  =>output_item%IOitemProps
            aProcesProp%no_FluxOutput= FluxOutput%cursize
            aProcesProp%FluxOutput   =>FluxOutput%IOitemProps
            aProcesProp%no_FluxStochi= FluxStochi%cursize
            aProcesProp%FluxStochi   =>FluxStochi%StochiProps
            aProcesProp%no_DispStochi= DispStochi%cursize
            aProcesProp%DispStochi   =>DispStochi%StochiProps
            aProcesProp%no_VeloStochi= VeloStochi%cursize
            aProcesProp%VeloStochi   =>VeloStochi%StochiProps
            iret = ProcesPropCollAdd( ProcesDef , aProcesProp )

         endif

  800 continue
!
      IF ( LASWI ) THEN
         DO IACT = 1 , NO_ACT
            IF ( ACTUSE(IACT) .NE. 1 ) THEN
               WRITE(lunrep,*) ' WARNING: activated process not found ',
     +           'in process definition file'
               WRITE(lunrep,*) ' process ID: ',ACTLST(IACT)
               NOWARN = NOWARN + 1
            ENDIF
         ENDDO
      ENDIF
      deallocate(actuse)
!
  900 CONTINUE
      if (timon) call timstop( ithndl )
      RETURN
      END
