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

      subroutine outhnc ( iohis , namfih, itime , moname, nodump,
     +                    idump , duname, notot1, synam1, conc1 ,
     +                    notot2, synam2, conc2 , init  )
!     Deltares Software Centre

!     Function            : Writes history output to NetCDF

      use timers

      integer       iohis , itime , nodump, notot1, notot2,
     +              init
      integer       idump(*)
      character*(*) moname(4), namfih
      character*(*) duname(*), synam1(*), synam2(*)
      real          conc1(*) , conc2(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "outhis", ithandl )
!
!     Initialize file
!
      if ( init .eq. 1 ) then
         init = 0
         write (iohis) (moname(i),i=1,4)
         write (iohis)  notot1+notot2,nodump
         write (iohis) (synam1(i),i=1,notot1),(synam2(i),i=1,notot2)
         write (iohis) (i,duname(i),i=1,nodump)
      endif
!
!     Perform output
!
      write (iohis) itime,(
     +              (conc1(k1+(idump(j)-1)*notot1),k1=1,notot1),
     +              (conc2(k2+(j-1)*notot2)       ,k2=1,notot2),
     +                                             j=1,nodump   )
!
      if ( timon ) call timstop ( ithandl )
      return
      end
