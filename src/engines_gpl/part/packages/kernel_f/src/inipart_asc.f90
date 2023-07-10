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
module m_inipart_asc

implicit none

contains


      subroutine inipart_asc( lgrid   , lgrid2  , nmax    , mmax    , xcor,     &
                          ycor    , nopart  , nosubs  , subst   , ini_file,     &
                          xpol    , ypol    , wpart   , xpart   ,     conc2,    &
                          ypart   , zpart   , npart   , mpart   , kpart   ,     &
                          iptime  , npmax   , nrowsmax, lunpr   )
!
      use m_skip_comments
      use precision_part ! single/double precision
      use timers
      use get_key_mod
      use grid_search_mod
      use pinpok_mod
      use random_generator
      use m_stop_exit
      use m_get_index

      implicit none ! force explicit typing

!     Arguments

!     kind           function         name                      description

      integer  ( ip), intent(in   ) :: nmax                    !< first dimension matrix
      integer  ( ip), intent(in   ) :: mmax                    !< second dimension matrix
      integer  ( ip), intent(in   ) :: npmax                   !< maximum number of particles
      integer  ( ip), intent(inout) :: nopart                  !< number of active particles
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid matrix
      real     ( rp), intent(in   ) :: xcor  (nmax*mmax)
      real     ( rp), intent(in   ) :: ycor  (nmax*mmax)
      integer  ( ip), intent(inout) :: nosubs                  !< number of substances
      character( * ), intent(in   ) :: subst (*)               !< substance names
      character( * ), intent(in   ) :: ini_file                !< polygon file
      integer  ( ip), intent(in   ) :: nrowsmax                !< dimension of poligons
      real     ( rp), intent(  out) :: xpol  (nrowsmax)        !< xvalues polygons
      real     ( rp), intent(  out) :: ypol  (nrowsmax)        !< yvalues polygons
      real     ( rp), intent(  out) :: wpart (nosubs,npmax)    !< weight of the particles
      real     ( rp), intent(  out) :: xpart (npmax)           !< x of theparticles
      real     ( rp), intent(  out) :: ypart (npmax)           !< y of the particles
      real     ( rp), intent(  out) :: zpart (npmax)           !< z of the particles
      integer  ( ip), intent(  out) :: npart (npmax)           !< n of the particles
      integer  ( ip), intent(  out) :: mpart (npmax)           !< m of the particles
      integer  ( ip), intent(  out) :: kpart (npmax)           !< k of the particles
      integer  ( ip), intent(  out) :: iptime(npmax)           !< time in the system
      integer  ( ip), intent(in   ) :: lunpr                   !< unit nr of the diagnostics file
      real     ( rp), intent( out)  :: conc2 (nrowsmax)          ! concentration factor per coordinate

      integer(ip), parameter            :: max_len_line=200
      integer(ip), parameter            :: max_len_blockname=4
      integer(ip), parameter            :: max_len_key=20

      integer(ip)                       :: lun_ini
      integer(ip)                       :: ios, ier
      integer(ip)                       :: npart_pol, npart_fact,isub, i, np, nerr
      integer(ip)                       :: npart_size, npsub
      integer(ip)                       :: nnpart, mmpart

      real   (sp)                       :: xx,yy
      real   (sp)                       :: totmass, avgmass
      real   (sp)                       :: xxcel, yycel
      logical                           :: okay
      logical                           :: end_of_file,read_error
      logical                           :: key_found
      logical                           :: substance_found

      character(len=max_len_key      ) :: key
      character(len=max_len_line     ) :: fract

      real (dp) :: rseed = 0.5d0
!
!     local scalars
!
      integer(ip) :: len_file, len_fract
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "inipart_asc", ithndl )

      len_file          =  len_trim(ini_file)

      open(newunit=lun_ini,file=ini_file,status='old',iostat=ios)
      if (ios /= 0) go to 900
!
         key = 'fraction'
         call get_string_key(lun_ini,  &
                         'fraction',fract,len_fract,key_found)
         if (.not. key_found) go to 910

!        get total mass
         key = 'mass'
         call get_real_key  (lun_ini,'mass',totmass,key_found)
         if (.not. key_found) go to 910

!        get no. of particles
         key = 'particles'
         call get_int_key   (lun_ini,'particles',npart_pol,key_found)
         if (.not. key_found) go to 910
!        get factor multiplier
         key = 'factor'
         call get_int_key   (lun_ini,'factor',npart_fact,key_found)
         if (.not. key_found) go to 910

!        get size of gridcel
         key = 'size'
         call get_int_key   (lun_ini,'size',npart_size,key_found)
         if (.not. key_found) go to 910

         avgmass = totmass/(npart_pol*npart_fact)  ! average mass per particle

         isub    = get_index (nosubs,subst,fract) ! find proper index
         substance_found = isub >= 0
         if (.not. substance_found) go to 940
!
!        read polygone (tekal format)
!
         call skip_comment_lines(lun_ini,ios)
         end_of_file = ios < 0
         read_error  = ios > 0
         if (end_of_file) go to 920
         if (read_error ) go to 930

         write(lunpr,*) trim(fract), totmass, npart_pol*npart_fact, avgmass
         do i=1,npart_pol
            read(lun_ini,*,iostat=ios) xpol(i), ypol(i), conc2(i)
!            write(*,*)' initial:',xpol(i), ypol(i), conc(i)
            end_of_file = ios < 0
            read_error  = ios > 0
            if (end_of_file) go to 920
            if (read_error ) go to 930
         enddo
!
!        double sample polygone area by random (x,y) pairs,
!        resulting in uniform spreading of particles over polygone area
!
         np = 0
         nerr = 0
         do while (np < npart_pol)
           npsub=0
           do while (npsub < npart_fact)
            xx = rnd(rseed)*npart_size + xpol(np+1)-npart_size/2.
            yy = rnd(rseed)*npart_size + ypol(np+1)-npart_size/2.
            npsub=npsub+1
                call part07 (lgrid  , lgrid2 , nmax   , mmax   , xcor  ,       &
                             ycor   , xx     , yy     , nnpart , mmpart,       &
                             xxcel  , yycel  , ier )

                if ( ier == 0 ) then
                   nerr = 0
!
                   nopart = nopart + 1 !count total number spread particles

                   xpart(nopart)     = xxcel    !  0 <= xxcel <= 1
                   ypart(nopart)     = yycel    !  0 <= yycel <= 1

                   npart(nopart)     = nnpart
                   mpart(nopart)     = mmpart
!
!                  locate particles at water surface
!                  (top of surface layer (z=0.0 ; k=1))
!
                   zpart(nopart)     = 0.0   ! top of layer
                   kpart(nopart)     = 1     ! top layer (at water surface)

                   wpart(isub,nopart) = avgmass*conc2(np+1)  !mass of the particle is scaled with the factor in the dataset (to allow for concentration variations)
                   iptime(nopart)     = 0     ! age of particle
                else
                   nerr = nerr + 1
                   if (nerr .gt. 10000) go to 950
                end if
           enddo
           np=np+1
!           else
                np = np ! debug statement
!            endif
         enddo
!
         okay = ios==0
!      enddo
      if ( timon ) call timstop ( ithndl )
      close(lun_ini)
      return
!     error handling

  900 write(*,'(//a,a)')       ' Error: problem with ini-file ',ini_file(:len_file)
      write(*,'(a)')           ' Could not open/find ini-file ??'
      write(lunpr,'(//a,a)')   ' Error: problem with ini-file ',ini_file(:len_file)
      write(lunpr,'(a,a)')     ' Could not open/find ini-file ??'
      call stop_exit(1)

  910 write(*,'(//a,a)')       ' Error: problem with ini-file ',ini_file(:len_file)
      write(*,'(a,a)')         ' Could not find key ',key
      write(lunpr,'(//a,a)')   ' Error: problem with ini-file ',ini_file(:len_file)
      write(lunpr,'(a,a)')     ' Could not find key ',key
      call stop_exit(1)

  920 write(*,'(//a,a)')       ' Error: problem with ini-file ',ini_file(:len_file)
      write(*,'(//a,a)')       ' End-of-file found on ini-file '
      write(lunpr,'(//a,a)')   ' Error: problem with ini-file ',ini_file(:len_file)
      write(lunpr,'(//a,a)')   ' End-of-file found on ini-file '
      call stop_exit(1)

  930 write(*,'(//a,a)')       ' Error: problem with ini-file ',ini_file(:len_file)
      write(*,'(//a,a)')       ' Error while reading ini-file'
      write(lunpr,'(//a,a)')   ' Error: problem with ini-file ',ini_file(:len_file)
      write(lunpr,'(//a,a)')   ' Error while reading ini-file'
      call stop_exit(1)

  940 write(*,'(//a,a)')       ' Error: problem with ini-file ',ini_file(:len_file)
      write(*,'(//a,a)')       ' Could not find substance ',fract
      write(lunpr,'(//a,a)')   ' Error: problem with ini-file ',ini_file(:len_file)
      write(lunpr,'(//a,a)')   ' Could not find substance ',fract
      call stop_exit(1)

  950 write(*,'(//a,a)')       ' Error: problem with ini-file ',ini_file(:len_file)
      write(*,'(//a)')         ' Couldn''t find cells for the initial oil particles.'
      write(*,'(//a)')         ' Is (part of) the polygon within the grid range?'
      write(lunpr,'(//a,a)')   ' Error: problem with ini-file ',ini_file(:len_file)
      write(lunpr,'(//a)')     ' Couldn''t find cells for the initial oil particles.'
      write(lunpr,'(//a)')     ' Is (part of) the polygon within the grid range?'
      call stop_exit(1)

      end subroutine inipart_asc




end module m_inipart_asc
