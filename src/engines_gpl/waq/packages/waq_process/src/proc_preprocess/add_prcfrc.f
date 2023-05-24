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

      subroutine add_prcfrc ( lunrep, procesdef, allitems, sfracs, no_act,
     +                        actlst, nbpr     )

      ! add processes per fractions

      use m_zoek
      use m_srstop
      use processet
      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      integer                   :: lunrep          ! report file
      type(procespropcoll)      :: procesdef       ! the process definition
      type(itempropcoll)        :: allitems        ! all items of the proces system
      type(sfracsprop)          :: sfracs          ! substance fraction properties
      integer                   :: no_act          ! number of active processes
      character(len=*)          :: actlst(*)       ! active processes names
      integer                   :: nbpr            ! number of processes

      ! local declarations

      type(procesprop), pointer :: proc            ! single process
      type(procesprop)          :: procn           ! process to be added
      type(itemprop)            :: item            ! one item
      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      integer                   :: iproc_new       ! index inserted process
      integer                   :: isfrac          ! index substance fractions
      integer                   :: isfrac_2        ! index substance fractions
      integer                   :: nfrac           ! number fractions in substance fraction
      integer                   :: nlink           ! number of linked substance fractions
      integer                   :: ilink           ! index of linked substance fractions
      integer                   :: linklst(sfracs%nsfrac) ! index linked substance fractions
      character(len=20)         :: basnam          ! base name substance fractions
      character(len=20)         :: fracnam         ! name of substance fraction
      character(len=20)         :: lnknam          ! base name of linked substance fractions
      integer                   :: i_input         ! index input item
      integer                   :: i_output        ! index output item
      integer                   :: i_flux          ! index flux item
      integer                   :: i_star          ! index of * in name
      integer                   :: nzoek           ! nzoek
      integer                   :: istochi         ! index stochi
      integer                   :: istochi2        ! index stochi
      integer                   :: indx            ! index in list
      integer                   :: iret            ! index in collection
      integer                   :: ifrac           ! fraction number
      character(len=3)          :: suffix          ! suffix
      integer                   :: ierr_alloc      ! error indication
      logical                   :: l_copied        ! if process is copied
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "add_prcfrc", ithndl )

      ! loop over the processes

      nproc = procesdef%cursize
      proc_loop: do iproc = 1, nproc

         proc => procesdef%procesprops(iproc)

         ! only for processes which need to be copied

         if ( procesdef%procesprops(iproc)%sfrac_type .eq. SFRAC_DUPLICATE) then

            ! check for fluxes on substances with fractions

            l_copied = .false.
            sfrac_loop: do isfrac = 1, sfracs%nsfrac

               nfrac  = sfracs%nfrac(isfrac)
               basnam = sfracs%name(isfrac)
               nlink  = 0
               if ( sfracs%linked(isfrac) .ne. 0 ) then
                  do isfrac_2 = 1, sfracs%nsfrac
                     if ( isfrac_2 .ne. isfrac ) then
                        if ( sfracs%linked(isfrac_2) .eq. sfracs%linked(isfrac) ) then
                           nlink = nlink + 1
                           linklst(nlink) = isfrac_2
                        endif
                     endif
                  enddo
               endif

               stochi_loop: do istochi = 1 , proc%no_fluxstochi

                  ! include also "dummy" rules (factor equal zero)

                     call zoek( basnam, 1, proc%fluxstochi(istochi)%substance, 10, indx)
                     if ( indx .eq. 1 ) then

                        ! construct copy processes

                        do ifrac = 1, nfrac
                           if ( ifrac .lt. 100 ) then
                              write(suffix,'(i2.2)') ifrac
                           else
                              write(suffix,'(i3.3)') ifrac
                           endif

                           fracnam             = trim(basnam)//trim(suffix)
                           procn%name          = trim(proc%name)//'*'//fracnam
                           procn%routine       = proc%routine
                           procn%text          = proc%text
                           procn%swtransp      = proc%swtransp
                           procn%type          = proc%type
                           procn%sfrac_type    = SFRAC_DUPLICATED
                           procn%no_input      = proc%no_input
                           procn%no_output     = proc%no_output
                           procn%no_fluxoutput = proc%no_fluxoutput
                           procn%no_fluxstochi = proc%no_fluxstochi
                           procn%no_dispstochi = proc%no_dispstochi
                           procn%no_velostochi = proc%no_velostochi
                           allocate(procn%input_item(procn%no_input)     ,
     +                              procn%output_item(procn%no_output)   ,
     +                              procn%fluxoutput(procn%no_fluxoutput),
     +                              procn%fluxstochi(procn%no_fluxstochi),
     +                              procn%dispstochi(procn%no_dispstochi),
     +                              procn%velostochi(procn%no_velostochi),
     +                              stat=ierr_alloc)
                           if ( ierr_alloc .ne. 0 ) then
                              write(lunrep,*) 'error allocating work array in routine add_prcfrc:',ierr_alloc
                              write(lunrep,*) 'routine            :',trim(procn%name)
                              write(lunrep,*) 'procn%no_input     :',procn%no_input
                              write(lunrep,*) 'procn%no_output    :',procn%no_output
                              write(lunrep,*) 'procn%no_fluxoutput:',procn%no_fluxoutput
                              write(lunrep,*) 'procn%no_fluxstochi:',procn%no_fluxstochi
                              write(lunrep,*) 'procn%no_dispstochi:',procn%no_dispstochi
                              write(lunrep,*) 'procn%no_velostochi:',procn%no_velostochi
                              write(*,*) 'error allocating array:',ierr_alloc
                              call srstop(1)
                           endif

                           ! copy input

                           do i_input = 1, procn%no_input
                              procn%input_item(i_input) = proc%input_item(i_input)

                              ! check if it is a fraction

                              call zoek( basnam, 1, procn%input_item(i_input)%name, 10, indx)
                              if ( indx .eq. 1 ) then
                                 procn%input_item(i_input)%name = fracnam
                                 item%name                      = procn%input_item(i_input)%name
                                 iret                           = itempropcollfind( allitems, item )
                                 if ( iret .le. 0 ) then
                                    item%text    = proc%input_item(i_input)%item%text
                                    item%default = proc%input_item(i_input)%item%default
                                    item%waqtype = proc%input_item(i_input)%item%waqtype
                                    iret         = itempropcolladd( allitems, item )
                                 endif
                                 procn%input_item(i_input)%item=>allitems%itemproppnts(iret)%pnt
                              else

                                ! is it a fraction specific input ?

                                 i_star = index(procn%input_item(i_input)%name,'*')
                                 if ( i_star .gt. 0 ) then
                                    nzoek = 20-i_star
                                    call zoek( basnam, 1, procn%input_item(i_input)%name(i_star+1:),  nzoek, indx)
                                    if ( indx .eq. 1 ) then
                                       procn%input_item(i_input)%name = procn%input_item(i_input)%name(1:i_star)//fracnam
                                       item%name                      = procn%input_item(i_input)%name
                                       iret                           = itempropcollfind( allitems, item )
                                       if ( iret .le. 0 ) then
                                          item%text    = proc%input_item(i_input)%item%text
                                          item%default = proc%input_item(i_input)%item%default
                                          item%waqtype = proc%input_item(i_input)%item%waqtype
                                          iret         = itempropcolladd( allitems, item )
                                       endif
                                       procn%input_item(i_input)%item=>allitems%itemproppnts(iret)%pnt
                                    endif
                                 else

                                    ! add fraction name anyhow the user may want to enter this, the original name will also be valid input

                                    procn%input_item(i_input)%name = trim(procn%input_item(i_input)%name)//'*'//fracnam

                                 endif

                              endif


                           enddo

                           ! copy output always fraction name added unless fraction specific output

                           do i_output = 1, procn%no_output
                              procn%output_item(i_output)      = proc%output_item(i_output)
                              !
                              i_star = index(procn%output_item(i_output)%name,'*')
                              if ( i_star .gt. 0 ) then
                                 nzoek = 20-i_star
                                 call zoek( basnam, 1, procn%output_item(i_output)%name(i_star+1:),  nzoek, indx)
                                 if ( indx .eq. 1 ) then
                                    procn%output_item(i_output)%name = procn%output_item(i_output)%name(1:i_star)//fracnam
                                 else
                                    procn%output_item(i_output)%name = trim(proc%output_item(i_output)%name)//'*'//fracnam
                                 endif
                              else
                                 procn%output_item(i_output)%name = trim(proc%output_item(i_output)%name)//'*'//fracnam
                              endif
                              !
                              item%name                   = procn%output_item(i_output)%name
                              iret                        = itempropcollfind( allitems, item )
                              if ( iret .le. 0 ) then
                                 item%text    = proc%output_item(i_output)%item%text
                                 item%default = proc%output_item(i_output)%item%default
                                 item%waqtype = proc%output_item(i_output)%item%waqtype
                                 iret         = itempropcolladd( allitems, item )
                              endif
                              procn%output_item(i_output)%item=>allitems%itemproppnts(iret)%pnt
                           enddo

                           ! copy flux always fraction name added unless fraction specific flux

                           do i_flux = 1, procn%no_fluxoutput
                              !
                              i_star = index(proc%fluxoutput(i_flux)%name,'*')
                              if ( i_star .gt. 0 ) then
                                 nzoek = 20-i_star
                                 call zoek( basnam, 1, proc%fluxoutput(i_flux)%name(i_star+1:),  nzoek, indx)
                                 if ( indx .eq. 1 ) then
                                    procn%fluxoutput(i_flux)%name = proc%fluxoutput(i_flux)%name(1:i_star)//fracnam
                                 else
                                    procn%fluxoutput(i_flux)%name = trim(proc%fluxoutput(i_flux)%name)//'*'//fracnam
                                 endif
                              else
                                 procn%fluxoutput(i_flux)%name = trim(proc%fluxoutput(i_flux)%name)//'*'//fracnam
                              endif
                              !
                              procn%fluxoutput(i_flux)%type   = proc%fluxoutput(i_flux)%type
                              procn%fluxoutput(i_flux)%actdef = proc%fluxoutput(i_flux)%actdef
                              procn%fluxoutput(i_flux)%indx   = proc%fluxoutput(i_flux)%indx
                              procn%fluxoutput(i_flux)%ip_val = proc%fluxoutput(i_flux)%ip_val
                              item%name                       = procn%fluxoutput(i_flux)%name
                              iret                            = itempropcollfind( allitems, item )
                              if ( iret .le. 0 ) then
                                 item%text    = proc%fluxoutput(i_flux)%item%text
                                 item%default = proc%fluxoutput(i_flux)%item%default
                                 item%waqtype = proc%fluxoutput(i_flux)%item%waqtype
                                 iret         = itempropcolladd( allitems, item )
                              endif
                              procn%fluxoutput(i_flux)%item=>allitems%itemproppnts(iret)%pnt
                           enddo

                           ! flux stochi

                           do istochi2 = 1, procn%no_fluxstochi
                              procn%fluxstochi(istochi2)%type      = proc%fluxstochi(istochi2)%type
                              procn%fluxstochi(istochi2)%ioitem    = trim(proc%fluxstochi(istochi2)%ioitem)//'*'//fracnam
                              procn%fluxstochi(istochi2)%substance = proc%fluxstochi(istochi2)%substance
                              procn%fluxstochi(istochi2)%subindx   = proc%fluxstochi(istochi2)%subindx
                              procn%fluxstochi(istochi2)%scale     = proc%fluxstochi(istochi2)%scale

                              ! look for fraction

                              call zoek( basnam, 1, procn%fluxstochi(istochi2)%substance, 10, indx)
                              if ( indx .eq. 1 ) then
                                 procn%fluxstochi(istochi2)%substance = fracnam
                              else

                                 ! look for linked fraction

                                 do ilink = 1, nlink
                                    lnknam = sfracs%name(linklst(ilink))
                                    call zoek( lnknam, 1, procn%fluxstochi(istochi2)%substance, 10, indx)
                                    if ( indx .eq. 1 ) then
                                       procn%fluxstochi(istochi2)%substance = trim(lnknam)//suffix
                                    endif
                                 enddo
                              endif

                           enddo

                           ! disp stochi

                           do istochi2 = 1, procn%no_dispstochi
                              procn%dispstochi(istochi2)%type      = proc%dispstochi(istochi2)%type
                              procn%dispstochi(istochi2)%ioitem    = trim(proc%dispstochi(istochi2)%ioitem)//'*'//fracnam
                              procn%dispstochi(istochi2)%substance = proc%dispstochi(istochi2)%substance
                              procn%dispstochi(istochi2)%subindx   = proc%dispstochi(istochi2)%subindx
                              procn%dispstochi(istochi2)%scale     = proc%dispstochi(istochi2)%scale

                              ! look for fraction

                              call zoek( basnam, 1, procn%dispstochi(istochi2)%substance, 10, indx)
                              if ( indx .eq. 1 ) then
                                 procn%dispstochi(istochi2)%substance = fracnam
                              else

                                 ! look for linked fraction

                                 do ilink = 1, nlink
                                    lnknam = sfracs%name(linklst(ilink))
                                    call zoek( lnknam, 1, procn%dispstochi(istochi2)%substance, 10, indx)
                                    if ( indx .eq. 1 ) then
                                       procn%dispstochi(istochi2)%substance = trim(lnknam)//suffix
                                    endif
                                 enddo
                              endif

                           enddo

                           ! velo stochi

                           do istochi2 = 1, procn%no_velostochi
                              procn%velostochi(istochi2)%type      = proc%velostochi(istochi2)%type
                              procn%velostochi(istochi2)%ioitem    = trim(proc%velostochi(istochi2)%ioitem)//'*'//fracnam
                              procn%velostochi(istochi2)%substance = proc%velostochi(istochi2)%substance
                              procn%velostochi(istochi2)%subindx   = proc%velostochi(istochi2)%subindx
                              procn%velostochi(istochi2)%scale     = proc%velostochi(istochi2)%scale

                              ! look for fraction

                              call zoek( basnam, 1, procn%velostochi(istochi2)%substance, 10, indx)
                              if ( indx .eq. 1 ) then
                                 procn%velostochi(istochi2)%substance = fracnam
                              else

                                 ! look for linked fraction

                                 do ilink = 1, nlink
                                    lnknam = sfracs%name(linklst(ilink))
                                    call zoek( lnknam, 1, procn%velostochi(istochi2)%substance, 10, indx)
                                    if ( indx .eq. 1 ) then
                                       procn%velostochi(istochi2)%substance = trim(lnknam)//suffix
                                    endif
                                 enddo
                              endif

                           enddo

                           ! insert process at the end

                           iproc_new = procespropcolladd( procesdef , procn )
                           no_act = no_act + 1
                           actlst(no_act) = procn%name
                           nbpr   = nbpr + 1
                           write(lunrep,2000) ' adding duplicate process [',procn%name,']'

                           ! repointer proc, the procespropcolladd makes this necessary

                           proc => procesdef%procesprops(iproc)


                        enddo

                        ! this proces is copied, exit the stochi and sfrac loops

                        l_copied = .true.
                        procesdef%procesprops(iproc)%sfrac_type = SFRAC_DUPLICATED_ORIGINAL
                        exit

                     endif
                  !endif
               enddo stochi_loop
               if ( l_copied ) exit
            enddo sfrac_loop
         endif
      enddo proc_loop

      if (timon) call timstop( ithndl )
      return
 2000 format ( 3a )
      end
