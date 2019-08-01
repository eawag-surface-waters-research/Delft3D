!!  Copyright (C)  Stichting Deltares, 2012-2019.
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

!    Module to read BLOOM input files
!
      subroutine blinpu (ntyp_m, ntyp_a, ngro_a, algtyp, lmixo, lfixn, lcarb, nunucom, nutcon, flxcon, con2out)
      
      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_arran   
      use bloom_data_mass_balance  
      use bloom_data_caldynam
      use bloom_data_io  
      use bloom_data_matrix  
      use bloom_data_phyt    
      use bloom_data_putin   
      use bloom_data_sumou   

      implicit none

      integer      ntyp_m               ! Maximum number of types
      integer      ntyp_a               ! Actual number of types
      integer      ngro_a               ! Actual number of groups
      integer      nunucom              ! Number of constrains
      integer      j, k, is             ! Indexes
      real         algtyp(0:20,ntyp_m)  ! Characteristics per algae type
      logical      lmixo                ! Flag mixotrophy
      logical      lfixn                ! Flag N-fixation
      logical      lcarb                ! Flag carbon limitation
      integer      nutcon(nunucom)      ! Nutrients involved in active nutrient constraints
      integer      flxcon(nunucom)      ! Uptake fluxes involved in active nutrient constraints
      integer      con2out(nunucom)     ! Mapping of actual nutrient constraints to DELWAQ output

!     Local variables
      integer      lparam, i
      real         autofr

!  Read title lines of BLOOM II input file.
      read (inuni, '(I4,1X,9A8)') iyear, (case (i), i = 1,9)
      read (inuni, '(9A8,8X)') com

!  Determine nuspec and nuecog
      is = 0
      nuecog = 0
   60   is = is + 1
        if ((algtyp(0,is).gt.-100.).and.(is.le.ntyp_m)) then
          if (is.eq.1) then
            j=1
            it2(1,1)=1
          elseif (is.eq.ntyp_m) then
            it2(j,2) = ntyp_m
          elseif (nint(algtyp(1,is)).ne.nint(algtyp(1,is-1))) then
            it2(j,2) = is-1
            j = j + 1
            it2(j,1) = is
          endif
          it2(j,2) = is
          nuecog = max(nuecog,nint(algtyp(1,is)))
          goto 60
        endif

      nuecog = j
      nuspec = is - 1
      if ((is.eq.ntyp_m).and.(algtyp(0,ntyp_m).gt.-100.)) nuspec =ntyp_m

!  Set the algae characteristics
      lmixo = .false.
      lfixn = .false.
      do j=1,nuecog
         grname(j)(1:1) = char(ichar('a')+j-1)
         k = 0
         do i=it2(j,1),it2(j,2)
            k = k + 1
            spname(i)(1:1) = char(ichar('a')+j-1)
            write(spname(i)(3:3),'(i1)') k
            ctodry(i) = algtyp(3,i)
            ekx(i)    = algtyp(2,i) * 0.001 / ctodry(i)
            if (algtyp(16,i).gt.0.0) lmixo = .true.
            if (algtyp(17,i).gt.0.0) lmixo = .true.
            if (algtyp(18,i).gt.0.0) lfixn = .true.
            chltoc(i) = 1./ algtyp(7,i)
            chlr(i)   = chltoc(i)*ctodry(i)
            pmax1(i)  = algtyp(8,i)
            pmax2(i)  = algtyp(9,i)
            if (nint(algtyp(10,i)).eq.0) then
               lpmax(i) = 1
            else
               lpmax(i) = 0
            endif
            rmort1(i) = algtyp(11,i)
            rmort2(i) = algtyp(12,i)
            rmort3(i) = algtyp(20,i)
            res1(i) = algtyp(13,i)
            res2(i) = algtyp(14,i)
            sdmix(i) = algtyp(19,i)
            autofr = algtyp(15,i)
            availn(i) = dble(1.d0 - autofr)
         end do
      end do

!     Set admin dependent on NUNUCO
!     Note that we handle different sets of nutrient constraints
!      - optional carbon limitation (LCARB) 
!      - mixotrophy (N,P) (LMIXO)
!      - N-fixation (LFIXN)
      do i=1,nuspec
         aa(1,i)   = algtyp(4,i) / ctodry(i)
         aa(2,i)   = algtyp(5,i) / ctodry(i)
         aa(3,i)   = algtyp(6,i) / ctodry(i)
         if (lcarb) aa(4,i)   = 1. / ctodry(i)
      enddo
      nutcon (1) = 1
      nutcon (2) = 2
      nutcon (3) = 3
      flxcon (1) = 2  ! NH4 uptake
      flxcon (2) = 4  ! PO4 uptake
      flxcon (3) = 5  ! Si uptake
      con2out(1) = 1
      con2out(2) = 2
      con2out(3) = 3
      nunuco = 3
      if (lcarb) then
         nutcon (nunuco+1) = 4
         flxcon (nunuco+1) = 1  ! C uptake
         con2out(nunuco+1) = 4
         nunuco = 4
      endif
      if (lmixo) then
         do i=1,nuspec
            aa(nunuco+1,i) = max(0.0,algtyp(16,i) / ctodry(i))
            aa(nunuco+2,i) = max(0.0,algtyp(17,i) / ctodry(i))
         enddo
         cstra(nunuco+1) = 'N-Detr'
         limnam(nunuco+1) = 'N-D'
         cstra(nunuco+2) = 'P-Detr'
         limnam(nunuco+2) = 'P-D'
         nutcon (nunuco+1) = 1
         nutcon (nunuco+2) = 2
         flxcon (nunuco+1) = 6  ! DetN uptake
         flxcon (nunuco+2) = 7  ! DetP uptake
         con2out(nunuco+1) = 5
         con2out(nunuco+2) = 6
         nunuco = nunuco + 2
      endif
      if (lfixn) then
        do i=1,nuspec
           aa(nunuco+1,i) = max(0.0,algtyp(18,i) / ctodry(i))
        enddo
        cstra(nunuco+1) = 'N-Fix'
        limnam(nunuco+1) = 'N-F'
        nutcon (nunuco+1) = 1
        flxcon (nunuco+1) = 8  ! NFix
        con2out(nunuco+1) = 7
        nunuco = nunuco + 1
      endif
      if (nunuco.gt.nunucom) goto 901

!  Call subroutine INPUT2 to read BLOOM specific data for
!  species, constraints, stochiometry etc.
      call input2 (inuni,iou(12),1)

!  Close the efficiency file.
      close (iou(12))

!  Set various counters used in several routines of BLOOM II.
!  NREP   = counter for number of calls to all main BLOOM II routines.
!  NPRINT = counter for print routines.
!  NPRODU = counter for BLOOM II production routines (which are NOT
!           used here).
!  LPRINT = flag indicating whether normal BLOOM II output routines
!           are called (LPRINT = 1) or not (LPRINT = 0).
!  MI     = number of time periods considered in one computation step of
!           BLOOM II.
      nrep   = 0
      nprint = 0
      nprodu = 0
      lprint = 1
      mi     = nper (1,3)
      imu    = 1

!  Call subroutine "OPTION" to read options for program control.
!  If "RUN" was not specified or if the program has detected
!  an error, it will terminate.
      call option (0,lparam)
      if (lparam. eq. 1) call change(1)
      close (iou(9))
      if (nuspec .gt. mt) goto 901
      if (nunuco .gt. mn) goto 901
      if (lrun .eq. 0) then
         write (ouuni,40)
   40    format (1X,'No "RUN" command or a fatal error was detected; ',
     1           'execution terminates',/)
         goto 902
      endif

!     Pass actual number of groups and species to main program

      ntyp_a = nuspec
      ngro_a = nuecog

   50 continue
      return

!     Maximum number permitted species exceeded
!     Present program version can only handle MT phytoplankton species.
  901 write(*,*) 'Fatal error 901 in BLINPU'
      call srstop(1)

!     No "RUN" command or a fatal error was detected,
!     execution terminates
  902 write(*,*) 'Fatal error 902 in BLINPU'
      call srstop(1)
      end
