      module wrwaq
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2014.                                
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
!-------------------------------------------------------------------------------
      
      contains

      function openwaqbinfile(filename) result (lun)
         character*(*), intent(in) :: filename
         integer :: lun
         integer, external :: newunit
!
!           WARNING: WAQ input files must be written using form=binary
!                    instead of unformatted.
!                    Although it is not standard Fortran
!
         lun    = newunit()
         open  ( lun , file=filename , form = 'binary' , SHARED )
      end function openwaqbinfile


      function openasciifile(filename) result (lun)
         character*(*), intent(in) :: filename
         integer :: lun
         integer, external :: newunit
!
!           NOTE: Opens a simple ASCII-file. Function is intended only
!                 to isolate newunit dependency.
!
         lun    = newunit()
         open  ( lun , file=filename , SHARED )
      end function openasciifile


      subroutine wrwaqpoi(ifrmto, noq, filename, ascii)
!!--description-----------------------------------------------------------------
! Write ASCII or binary pointer file
!!--declarations----------------------------------------------------------------
!     use xxx
!
      implicit none
!
!           Global variables
!
      integer                , intent(in) :: noq
      integer, dimension(:,:), intent(in) :: ifrmto
      logical                , intent(in) :: ascii
      character(*)           , intent(in) :: filename
!
!           Local variables
!
      integer :: i
      integer :: lunout
      integer :: q
!
!! executable statements -------------------------------------------------------
!
      if (ascii) then
         !
         ! ascii output
         !
         lunout = openasciifile(filename)
         do q = 1,noq
            write(lunout,'(4i)') ( ifrmto(i,q), i=1,4 )
         enddo
         close(lunout)
      else
         !
         ! binary output
         !
         lunout = openwaqbinfile(filename)
         write(lunout) ( ( ifrmto(i,q), i=1,4 ), q=1,noq )
         close(lunout)
      endif
      end subroutine wrwaqpoi


      subroutine wrwaqlen(lenex, noq, filename, ascii)
!!--description-----------------------------------------------------------------
! Write (binary) from/to length file
!!--declarations----------------------------------------------------------------
      use precision
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in) :: noq
      real(hp), dimension(:,:), intent(in) :: lenex
      logical                 , intent(in) :: ascii
      character(*)            , intent(in) :: filename
!
!           Local variables
!
      integer :: i
      integer :: lunout
      integer :: q
!
!! executable statements -------------------------------------------------------
!
      if (ascii) then
         !
         ! ascii output
         !
         lunout = openasciifile(filename)
         do q = 1,noq
            write(lunout,'(i,2f18.8)') q, ( lenex(i,q), i=1,2 )
         enddo
         close(lunout)
      else
         !
         ! binary output
         !
         lunout = openwaqbinfile(filename)
         write(lunout) noq
         write(lunout) (( real(lenex(i,q),sp), i=1,2 ), q=1,noq )
         close(lunout)
      endif
      end subroutine wrwaqlen


      subroutine wrwaqbin(itim, quant, nquant, filename, ascii, lunout)
!!--description-----------------------------------------------------------------
! Write (binary) exchange file(s): area and fluxes
!!--declarations----------------------------------------------------------------
      use precision
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in)    :: itim
      integer                 , intent(in)    :: nquant
      real(hp), dimension(:)  , intent(in)    :: quant
      logical                 , intent(in)    :: ascii
      character(*)            , intent(in)    :: filename
      integer                 , intent(inout) :: lunout
!
!           Local variables
!
      integer :: q
!
!! executable statements -------------------------------------------------------
!
      if (ascii) then
         !
         ! ascii output
         !
         if (lunout<0) then
            lunout = openasciifile(filename)
         endif
         write(lunout,'(a,i)') 'Time = ', itim
         do q = 1,nquant
            write(lunout,'(i,f18.8)') q, quant(q)
         enddo
         !close(lunout)
      else
         !
         ! binary output
         !
         if (lunout<0) then
            lunout = openwaqbinfile(filename)
         endif
         write(lunout) itim
         write(lunout) ( real(quant(q),sp), q=1,nquant )
         !close(lunout)
      endif
      end subroutine wrwaqbin


      subroutine wrmonseg(noseg, filename)
!!--description-----------------------------------------------------------------
! Write monitoring segments file (each segment is a monitoring area)
!!--declarations----------------------------------------------------------------
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in) :: noseg
      character(*)            , intent(in) :: filename
!
!           Local variables
!
      integer :: lunout
      integer :: s
!
!! executable statements -------------------------------------------------------
!
      lunout = openasciifile(filename)
      write(lunout,'(i5)') noseg
      do s = 1,noseg
         write(lunout,'(a,i4,a,i5)') '''Segment ',s,''' 1 ',s
      enddo
      close(lunout)
      end subroutine wrmonseg


      subroutine wr_nrofseg(noseg, filename)
!!--description-----------------------------------------------------------------
! Write NROFSEGM.DAT file
!!--declarations----------------------------------------------------------------
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in) :: noseg
      character(*)            , intent(in) :: filename
!
!           Local variables
!
      integer :: lunout
!
!! executable statements -------------------------------------------------------
!
      lunout = openasciifile(filename)
      write(lunout,'(i12,a)') noseg,'   ; number of segments'
      close(lunout)
      end subroutine wr_nrofseg


      subroutine wr_nrofexch(noq1, noq2, noq3, filename)
!!--description-----------------------------------------------------------------
! Write NROFEXCH.DAT file
!!--declarations----------------------------------------------------------------
!
      implicit none
!
!           Global variables
!
      integer                 , intent(in) :: noq1
      integer                 , intent(in) :: noq2
      integer                 , intent(in) :: noq3
      character(*)            , intent(in) :: filename
!
!           Local variables
!
      integer :: lunout
!
!! executable statements -------------------------------------------------------
!
      lunout = openasciifile(filename)
      write(lunout,'(3i12,a)') noq1, noq2, noq3, '   ; number of exchanges in three directions'
      close(lunout)
      end subroutine wr_nrofexch

      end module wrwaq