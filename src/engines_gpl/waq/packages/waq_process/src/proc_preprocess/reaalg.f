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

      subroutine reaalg ( lunrep , lunblm , verspe , maxtyp , maxcof , 
     +                    notyp  , nocof  , noutgrp, nouttyp, alggrp ,
     +                    abrgrp , algtyp , abrtyp , algdsc , cofnam ,
     +                    algcof , outgrp , outtyp , noprot , namprot,
     +                    nampact, nopralg, nampralg)
!
!     Read the BLOOM-species database.
!
      use m_srstop
      use timers       !   performance timers

      implicit none
      
      integer       lunrep, lunblm
      real          verspe                     ! version number of bloom.spe-file
      integer       maxtyp, maxcof
      integer       notyp , nocof , nogrp , noutgrp, nouttyp
      character*10  alggrp(maxtyp), algtyp(maxtyp)
      character*5   abrgrp(maxtyp), abrtyp(maxtyp)
      character*80  algdsc(maxtyp)
      character*10  cofnam(maxcof)
      real          algcof(maxcof,maxtyp)
      character*10  outgrp(maxtyp), outtyp(maxtyp)
      integer       noprot , nopralg
      character*10  namprot(maxtyp), nampact(maxtyp),
     +              nampralg(maxtyp)
      character*80  cdummy(5)
      integer       i, iatyp
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "reaalg", ithndl )
!
      read(lunblm,*,err=900) notyp
      read(lunblm,*,err=901) nocof
      if (verspe.lt.2.0) then
         read(lunblm,2000,err=902) (cofnam(i),i=1,nocof)
         do iatyp = 1 , notyp
            read(lunblm,2010,err=903) alggrp(iatyp),abrgrp(iatyp),
     +                        algtyp(iatyp),abrtyp(iatyp),algdsc(iatyp),
     +                        (algcof(i,iatyp),i=1,nocof)
         enddo
      else
         read(lunblm,*,err=902) (cdummy(i),i=1,5), (cofnam(i),i=1,nocof)
         do iatyp = 1 , notyp
            read(lunblm,*,err=903) alggrp(iatyp),abrgrp(iatyp),
     +                        algtyp(iatyp),abrtyp(iatyp),algdsc(iatyp),
     +                        (algcof(i,iatyp),i=1,nocof)
         enddo
      endif
      read(lunblm,*,err=904) noutgrp
      if (verspe.lt.2.0) then
         do i = 1 , noutgrp
            read(lunblm,*,err=905) outgrp(i)
         enddo
      else
         read(lunblm,*,err=905) (outgrp(i), i = 1 , noutgrp)
      endif
      read(lunblm,*,err=906) nouttyp
      if (verspe.lt.2.0) then
         do i = 1 , nouttyp
            read(lunblm,*,err=907) outtyp(i)
         enddo
      else
         read (lunblm,*,err=907) (outtyp(i), i = 1 , nouttyp)
      endif
      read(lunblm,*,err=908) noprot
      if (verspe.lt.2.0) then
         do i = 1 , noprot
            read(lunblm,*,err=909) namprot(i),nampact(i)
         enddo
      else
         read(lunblm,*,err=909) (namprot(i),nampact(i), i = 1 , noprot)
      endif
      read(lunblm,*,err=910) nopralg
      if (verspe.lt.2.0) then
         do i = 1 , nopralg
            read(lunblm,*,err=911) nampralg(i)
         enddo
      else
         read(lunblm,*,err=911) (nampralg(i), i = 1 , nopralg)
      endif
!
      if (timon) call timstop( ithndl )
      return
!
 2000 format(10x,1x,5x,1x,10x,1x,5x,1x,30x,50(1x,a10))
 2010 format(a10,1x,a5,1x,a10,1x,a5,1x,a30,50(1x,f10.0))
!
  900 continue
      write(lunrep,3000)
      write( *    ,3000)
      call srstop(1)
 3000 format(' Error reading BLOOM database, number of types')
  901 continue
      write(lunrep,3001)
      write( *    ,3001)
      call srstop(1)
 3001 format(' Error reading BLOOM database, number of coefficients')
  902 continue
      write(lunrep,3002)
      write( *    ,3002)
      call srstop(1)
 3002 format(' Error reading BLOOM database, coefficient names')
  903 continue
      write(lunrep,3003)
      write( *    ,3003)
      call srstop(1)
 3003 format(' Error reading BLOOM database, types and coefficients')
  904 continue
      write(lunrep,3004)
      write( *    ,3004)
      call srstop(1)
 3004 format(' Error reading BLOOM database, no. of output per group')
  905 continue
      write(lunrep,3005)
      write( *    ,3005)
      call srstop(1)
 3005 format(' Error reading BLOOM database, output var. per group')
  906 continue
      write(lunrep,3006)
      write( *    ,3006)
      call srstop(1)
 3006 format(' Error reading BLOOM database, no. of output per type')
  907 continue
      write(lunrep,3007)
      write( *    ,3007)
      call srstop(1)
 3007 format(' Error reading BLOOM database, output var. per type')
  908 continue
      write(lunrep,3008)
      write( *    ,3008)
      call srstop(1)
 3008 format(' Error reading BLOOM database, no. of single processes')
  909 continue
      write(lunrep,3009)
      write( *    ,3009)
      call srstop(1)
 3009 format(' Error reading BLOOM database, single processes')
  910 continue
      write(lunrep,3010)
      write( *    ,3010)
      call srstop(1)
 3010 format(' Error reading BLOOM database, no. of processes per type')
  911 continue
      write(lunrep,3011)
      write( *    ,3011)
      call srstop(1)
 3011 format(' Error reading BLOOM database, processes per type')
!
      end
