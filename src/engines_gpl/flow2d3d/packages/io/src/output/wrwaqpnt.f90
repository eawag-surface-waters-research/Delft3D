      subroutine wrwaqpnt ( nmax   , mmax   , kmax   , nlb    , nub    , &
     &                      mlb    , mub    , kcs    , kfsmin , isaggr , &
     &                      ilaggr , iqaggr , ifrmto , aggre  , flaggr , &
     &                      noseg  , noq1   , noq2   , noq3   , noq    , &
     &                      nobnd  , kmk    , zmodel , filnam , lundia )

!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      implicit none
!                          parameters
      integer(4) nmax                      !!  dimension  first index in 2d arrays
      integer(4) mmax                      !!  dimension second index in 2d arrays
      integer(4) kmax                      !!  number of layers
      integer(4) nlb                       !!  Lower bound of all n dimensions
      integer(4) nub                       !!  Upper bound of all n dimensions
      integer(4) mlb                       !!  Lower bound of all m dimensions
      integer(4) mub                       !!  Upper bound of all m dimensions
      integer(4) kcs   (nlb:nub,mlb:mub)   !!  Fixed prop. of computational volumes
      integer(4) kfsmin(nlb:nub,mlb:mub)   !!  Variable lowest active layer (z-model-only)
      integer(4) isaggr(nmax*mmax*kmax)    !!  grid aggregation pointer
      integer(4) ilaggr(kmax)              !!  vertical layer aggregation array
      integer(4) iqaggr( 3*nmax*mmax*kmax) !!  flow aggregation pointer
      integer(4) ifrmto(12*nmax*mmax*kmax) !!  from-to pointer table
      integer(4) aggre                     !!  1 if aggregation, 0 is condensation
      character(*) flaggr                  !!  Name of aggregation file id aggre
      integer(4) noseg                     !!  number of segments
      integer(4) noq1                      !!  number of exchanges in 1st direction
      integer(4) noq2                      !!  number of exchanges in 2nd direction
      integer(4) noq3                      !!  number of exchanges in 3rd direction
      integer(4) noq                       !!  total number of exchanges
      integer(4) nobnd                     !!  number of open boundaries
      integer(4) kmk (nmax*mmax*kmax)      !!  feature array
      logical    zmodel                    !!  true if z-model feature is used
      character(*) filnam
      integer(4) lundia                    !!  file unit number to an output file
!                          local
      integer(4) i, j, k, m, n, iseg, ilay, kfmin
      integer(4), pointer :: lgrid(:,:)    !!  lgrid table (currently a full matrix
      integer(4), pointer :: iapnt( : )    !!  linear pointer to active cells
      integer(4), pointer :: ibpnt( : )    !!  linear pointer on active cells
      integer(4), pointer :: imint( : )    !!  linear pointer to lowest z-layer cell
      integer(4) mnmax, notot              !!  nmax*mmax   and times kmax
      integer(4) nosegl                    !!  layer help variables
      logical    filex                     !!  TRUE if file exists
      integer, external :: newlun_nogdp
      integer(4) lunout, lunaggr
      integer(4) istat                     !!  allocate return status
      character(300) message
      character( 2 ) kenout(nmax)          !!  this is now allocated on the stack !!!
!
!! executable statements -------------------------------------------------------
!
      lunout = newlun_nogdp()
      mnmax  = nmax*mmax
      notot  = mnmax*kmax
                    allocate ( lgrid( nmax , mmax ) , stat=istat)    !!  lgrid-table is now a full matrix
      if (istat==0) allocate ( iapnt( 0:mnmax   )   , stat=istat)
      if (istat==0 .and. zmodel) allocate ( imint( 0:mnmax   )   , stat=istat)
      if (istat/=0) then
         write(*,*) '*** ERROR: wrwaqpnt: memory allocation error'
         return
      endif

!            write the 'total' lgrid table (currently a full matrix)

      open  ( lunout , file=trim(filnam)//'lgt', form='binary' )
      k     = 1
      do j = 1,mmax
         do i = 1,nmax
            lgrid ( i, j ) = k
            k = k + 1
         enddo
      enddo
      write ( lunout ) nmax, mmax, mnmax, ilaggr(kmax), lgrid
      close ( lunout )

!            write the lgrid tabel with zero's and bounds

      open  ( lunout , file=trim(filnam)//'lgo', form='binary' )
      write ( lunout ) nmax, mmax, mnmax, ilaggr(kmax)
      k     = 1
      nobnd = 0
      iapnt = 0
      if ( zmodel ) imint = 0
      do j = 1,mmax
         do i = 1,nmax
            if ( kcs (i,j) .ne. 0 ) then
               if ( kcs (i,j) .eq. 2 .or. kcs(i,j) .eq. 3 ) then
                  nobnd = nobnd+1
                  iapnt(k) = -nobnd
               else
                  iapnt(k) = k
               endif
               if ( zmodel ) imint(k) = kfsmin(i,j)
            endif
            k = k + 1
         enddo
      enddo
      write ( lunout ) iapnt(1:mnmax)
      close ( lunout )

!            make the 'from' 'to' pointer table and the aggregation table

      open  ( lunout , file=trim(filnam)//'poi', form='binary' )
!            determine the type of aggregation
      aggre = -1                                         ! no aggregation
      if ( flaggr .eq. 'active only' ) aggre = 0         ! active cells only
      inquire ( file=flaggr, EXIST=filex )
      if ( filex ) then                          ! the dido aggregation file
         aggre = 1
         lunaggr = newlun_nogdp()
         open ( lunaggr , file=flaggr )
         read ( lunaggr , * , iostat=istat) n, m, k, i, j
         if ( istat /= 0) then
            write ( message , '(3A)' ) &
     &       '*** ERROR: unable to read dimensions in aggregation file ''', &
     &       trim(flaggr), '''. Coupling done without aggregation !'
            write( *      , '(A)' ) trim(message)
            write( lundia , '(A)' ) trim(message)
            aggre = -1
            close ( lunaggr )
         endif
         if ( m .ne. mmax .or. n .ne. nmax ) then
            write ( message , '(A,I6,A,I6,A,I6,A,I6,A)' )                &
     &       '*** ERROR: dimensions in aggregation file: (',             &
     &       m,',',n,') don''t match dimensions of problem: (',          &
     &       mmax,',',nmax,') coupling done without aggregation !'
            write( *      , '(A)' ) trim(message)
            write( lundia , '(A)' ) trim(message)
            aggre = -1
            close ( lunaggr )
         endif
      endif
!            the making of the pointers themself
      if ( aggre .eq. -1 ) then                          ! no aggregation
         call wrwaqnpnt ( mmax    , nmax  , kmax  , mnmax , nobnd ,      &
     &                    iapnt(1), ilaggr, ifrmto, isaggr, iqaggr,      &
     &                    zmodel  , noseg , noq1  , noq2  , noq3  )
      else
         if ( aggre .eq. 0 ) then                        ! active cells only
            k     = 1
            noseg = 0
            nobnd = 0
            iapnt = 0
            do j = 1,mmax
               do i = 1,nmax
                  if ( kcs (i,j) .ne. 0 ) then
                     if ( kcs (i,j) .eq. 2 .or. kcs(i,j) .eq. 3 ) then
                        nobnd = nobnd+1
                        iapnt(k) = -nobnd
                     else
                        noseg = noseg + 1
                        iapnt(k) = noseg ! this is the difference with line 110
                     endif
                  endif
                  k = k + 1
               enddo
            enddo
         else                                    ! the dido aggregation file
            read  ( lunaggr , * ) iapnt(1:mnmax)
            close ( lunaggr )
            do i = 1, mnmax
               noseg = MAX( noseg,  iapnt(i) )
               nobnd = MAX( nobnd, -iapnt(i) )
            enddo
         endif
         iqaggr = 0
         call wrwaqapnt ( mmax   , nmax  , kmax   , mnmax  , lgrid ,     &
     &                    iapnt  , ilaggr, noseg  , noq1   , noq2  ,     &
     &                    noq3   , nobnd , isaggr , iqaggr , ifrmto,     &
     &                    aggre  , zmodel)
!
      endif
      nosegl = noseg / ilaggr(kmax)
      noq    = noq1 + noq2 + noq3

!            For the z-model set below-bottom cells at zero pointers
!            It is unclear whether this is really necessary, but Jan
!            did it this way so it is maintained for the time being.

      if ( zmodel ) then
         allocate ( ibpnt( 0:noseg ) , stat=istat)
         if (istat/=0) then
            write(*,*) '*** ERROR: wrwaqpnt: memory allocation error'
            return
         endif

         ibpnt = 0
         do i = 1, notot
            m = isaggr(i)
            if ( m .gt. 0 ) then
               j =    (i-1)/mnmax  + 1          ! layer number 1 to k
               k = mod(i-1 ,mnmax) + 1          ! index
               if ( j .lt. imint(k) ) then
                  isaggr(i) = 0                 ! non aggregated cell below bottom
               else
                  ibpnt (m) = 1                 ! aggregated cell above bottom
               endif
            endif
         enddo
         do i = 1, noq*4
            m = ifrmto(i)
            if ( m .gt. 0 ) then
               if ( ibpnt(m) .eq. 0 ) ifrmto(i) = 0
            endif
         enddo
         deallocate ( ibpnt )
      endif

!            write the 'from' 'to' pointer table

      write ( lunout ) ( ifrmto(i),i=1,4*noq )
      close ( lunout )

!            write the aggregated lgrid tabel with zero's and bounds

      open  ( lunout , file=trim(filnam)//'lga', form='binary' )
      write ( lunout ) nmax, mmax, nosegl, ilaggr(kmax), noq1, noq2, noq3
      write ( lunout ) iapnt(1:mnmax)
      close ( lunout )

!            write the attribute file

      open  ( lunout, file=trim(filnam)//'atr', recl=max(min(nmax*2,1000),128) )
      write ( lunout , '(a)' )  '         ; DELWAQ_COMPLETE_ATTRIBUTES'
      write ( lunout , '(a)' )  '    2    ; two blocks with input     '
      write ( lunout , '(a)' )  '    1    ; number of attributes, they are :'
      write ( lunout , '(a)' )  '    1    ;  ''1'' is active ''0'' is not'
      write ( lunout , '(a)' )  '    1    ; data follows in this file '
      write ( lunout , '(a)' )  '    1    ; all data is given without defaults'
      kmk = 0
      do j = 1,mmax
         do i = 1,nmax
            if (kcs(i,j) .ne. 1) cycle
            kfmin = 1
            if ( zmodel ) kfmin = kfsmin(i,j)
            do k = kfmin, kmax
               iseg = isaggr( (k-1)*mnmax+(j-1)*nmax + i )
               kmk(iseg) = 1
            enddo
         enddo
      enddo
      do ilay = 1,ilaggr(kmax)
         write ( lunout , * ) '  ;    layer: ',ilay
         k =  1
         n = (ilay-1)*nosegl
         if ( aggre .le. 0 ) then
            do j = 1, mmax
               do i = 1,nmax
                  kenout(i) = '  '
                  if ( aggre .eq. -1 ) m =       k
                  if ( aggre .eq.  0 ) m = iapnt(k)
                  if ( m .gt. 0 ) write( kenout(i), '(I2)' ) kmk( m+n )
                  k = k + 1
               enddo
               write ( lunout, '(500a2)' ) kenout
            enddo
         else
            do j = 1, nosegl
               write ( lunout, '(100i2)' ) kmk(j+n)
            enddo
         endif
      enddo
      write ( lunout , '(a)' )  '    1    ; number of attributes, they are :'
      write ( lunout , '(a)' )  '    2    ;  ''1'' has surface ''3'' has bottom'
      write ( lunout , '(a)' )  '         ;  ''0'' has both    ''2'' has none  '
      write ( lunout , '(a)' )  '    1    ; data follows in this file '
      write ( lunout , '(a)' )  '    1    ; all data is given without defaults'
      kmk = -1
      do j = 1,mmax
         do i = 1,nmax
            if (kcs(i,j) .ne. 1) cycle
            kfmin = 1
            if ( zmodel ) kfmin = kfsmin(i,j)
            do k = kfmin, kmax
               iseg = isaggr( (k-1)*mnmax+(j-1)*nmax + i )
               if ( zmodel ) then
                  if ( k .eq. kfmin ) kmk(iseg) = 3      !  bottom segment
                  if ( k .eq. kmax  ) then               !  surface segment
                     if ( kmk(iseg) .eq. -1 .or.                         &
     &                    kmk(iseg) .eq.  2      ) then  !  no bottom no surface
                        kmk(iseg) = 1                    !  so surface
                     endif
                     if ( kmk(iseg) .eq. 3 ) then      !  also bottom segment
                        kmk(iseg) = 0                  !  so both
                     endif
                  endif
                  if ( k .ne. kfmin .and. k .ne. kmax ) then
                     if ( kmk(iseg) .eq. -1 ) kmk(iseg) = 2
                  endif
               else
                  if ( kmk(iseg) .eq. -1 ) kmk(iseg) = 2
                  if ( k .eq.   1  ) kmk(iseg) = 1
                  if ( k .eq. kmax ) then
                     if ( kmk(iseg) .eq. 1 ) kmk(iseg) = 0
                     if ( kmk(iseg) .eq. 2 ) kmk(iseg) = 3
                  endif
               endif
            enddo
         enddo
      enddo
      do iseg = 1, noseg
         if ( kmk(iseg) .eq. -1 ) kmk(iseg) = 0
      enddo
      do ilay = 1,ilaggr(kmax)               !       write the properties
         write ( lunout , * ) '  ;    layer: ',ilay
         k =  1
         n = (ilay-1)*nosegl
         if ( aggre .le. 0 ) then
            do j = 1, mmax
               do i = 1,nmax
                  kenout(i) = '  '
                  if ( aggre .eq. -1 ) m =       k
                  if ( aggre .eq.  0 ) m = iapnt(k)
                  if ( m .gt. 0 ) write( kenout(i), '(I2)' ) kmk( m+n )
                  k = k + 1
               enddo
               write ( lunout, '(500a2)' ) kenout
            enddo
         else
            do j = 1, nosegl
               write ( lunout, '(100i2)' ) kmk(j+n)
            enddo
         endif
      enddo
      write ( lunout , '(a)' )  '    0    ; no time dependent attributes'
      close ( lunout )

!            deallocate the loccaly allocated arrays

      deallocate ( lgrid )
      deallocate ( iapnt )
      if ( zmodel ) deallocate ( imint )
!
      end subroutine wrwaqpnt
