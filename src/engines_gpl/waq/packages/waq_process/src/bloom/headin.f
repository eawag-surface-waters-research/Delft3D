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

!  *********************************************************************
!  *     SUBROUTINE TO PRINT HEADINGS FOR SEVERAL OUTPUT FILES         *
!  *********************************************************************

      subroutine headin(nzout,words)

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_arran   
      use bloom_data_io  
      use bloom_data_phyt    
      use bloom_data_sumou   

      implicit none

      integer numtyp(mt), i, j, k, kk, nueco2, nzout
      character*4 words2(12),words3(20),words4(20)
      character*8 words(*),wwords(mg)

!  Print heading for output on units OUUNI, IOU(14), NZOUT, IOU(21), IOU(24) and IOU(25).
!
!  Set print-array indices.

      nts6=nuabco+3
      nts7=nts6+1
      nts14=nuecog+nts7+1

! Exit if LPRINT <= 1: nothing more to be done here.
      if (lprint .le. 1) return

!  Determine main active program options and store them in WORDS4.
      j = 1
      if (lobfun .eq. 1) then
         words4(j) = 'Grow'
         j = j + 1
         words4(j) = 'th. '
      else
         words4(j) = 'Biom'
         j = j + 1
         words4(j) = 'ass.'
      end if
      if (lgroch .eq. 1) then
         j = j + 1
         words4(j) = 'Groc'
         j = j + 1
         words4(j) = 'heck'
      end if
      if (lmorch .eq. 1) THEN
         j = j + 1
         words4(j) = 'Morc'
         j = j + 1
         words4(j) = 'heck'
      end if
      if (ldayeu .eq. 1) then
         j = j + 1
         words4(j) = 'Daye'
         j = j + 1
         words4(j) = 'upho'
      end if
      if (lpmort .eq. 1) then
         j = j + 1
         words4(j) = 'Pmax'
         j = j + 1
         words4(j) = '+Mor'
      end if

!  Write heading for standard output file (OUUNI).
      write(ouuni,25) (words4(i),i=1,j)
      write(ouuni,30)
   25 format(4X,'Model objective: Maximize ',2A4,/,4X,'Main active program options: ',9(A4,A4,'; '))
   30 format(2(' ',/),14X,'Summary of solutions for this run:',/,' ')

!  Write heading unit IOU(24). This file contains biomasses of all
!  types.
      call formfe (iou(24))
      write(iou(24),40)
   40 format(8X,'Dry weight biomasses of phytoplankton types:',2(/,' '))

!  Split species names in two parts and store truncated second parts
!  of names in "WORDS2".
      do 50 k=1,nuecog
      words2(k) = grname(k) (5:8)
   50 continue
      words2(nuecog+1) = words(6) (5:8)
      words2(nuecog+2) = words(7) (5:8)
      nueco2=nuecog+2

! Get names and relative numbers of types for heading unit IOU(24).
! Store first parts of names in WORDS3, get second parts from WORDS2.
      kk = 0
      do i = 1,nuecog
      k = 0
         do j = it2(i,1),it2(i,2)
            kk =kk + 1
            k = k + 1
            numtyp(kk) = k
            words3(kk) = grname (i) (1:4)
            words4(kk) = words2(i)
         end do
      end do

! Print names and relative numbers of types to unit IOU(24).
      write(iou(24),80) words(1),(words3(k),k=1,nuspec)
      write(iou(24),90) (words4(k),k=1,nuspec)
   80 format (1x,a4,1x,2x,20(a4,'-',1x))
   90 format (9x,20(a4,2x))
      write(iou(24),100) (numtyp(k),k=1,nuspec)
  100 format (7x,20(i4,2x))

! Write heading for unit OUUNI. This heading differs for interactive
! and batch runs.
      if (ioflag .eq. 0) then
         write(ouuni,140) (words(k),k=1,3),words(5),(grname(k),k=1,nuecog),(words(k),k=6,7)
      else
         write(ouuni,110) (words(k),k=1,3),(grname(k),k=1,nuecog),(words(k),k=6,7)
  110    format(1x,a4,1x,a5,4x,a4,3x,11(a4,'-',1x))
         write(ouuni,120) (words2(k),k=1,nueco2)
  120    format(24x,11(a4,2x))

!  Write heading for output IOU(21). This is the same heading written to
!  unit OUUNI in a batch job.
         write(iou(21),30)
         write(iou(21),140) (words(k),k=1,3),words(5),(grname(k),k=1,nuecog),(words(k),k=6,7)
  140    format(2x,a4,2x,2(a8,1x),8x,a4,5x,12(a8,1x))
  141    format(2x,a4,2x,2(a8,1x),8x,32(a8,1x))
         write (iou(21),150)
      end if
      write (ouuni,150)
  150 format(' ',/,' ')

!  Write heading unit IOU(25). This unit contains forcing function
!  values. Overwrite (!) WORDS3, which is no longer needed for species
!  names.
      call formfe (iou(25))
      write (iou(25),160)
  160 format (10x,'Summary of forcing functions used this run.',/,' ',/,' ')
      words3(1) = 'Date'
      words3(2) = 'Temp'
      words3(3) = 'Sola'
      words3(4) = 'Chl '
      do i = 1,nunuco
         words3(i+4) = cstra (i) (1:4)
      end do
      words3(nunuco+5) = 'Kb  '
      words3(nunuco+6) = 'Dayl'
      words3(nunuco+7) = 'Mort'
      words3(nunuco+8) = 'Zood'
      words3(nunuco+9) = 'Dept'
      write(iou(25),180) (words3(i),i=1,nunuco+9)
  180 format(1x,15(a4,4x))

!  Write heading for output IOU(14).
      write (iou(14),200)
  200 format (2X,'Particulate organic and dissolved nutrient concentra',
     1        'tions at equilibrium in mg / m3',/,'  Comparison of ',
     2        'calculated maximum steady state values of chlorophyll',
     3        ' to observations',3(/,' '))
      write (iou(14),220) words(1),(words(9),words(10),k=1,nunuco),words(12),words(7),words(11)
  220 format (2x,a4,6x,12(a8,4x))
      write (iou(14),240) ((cstra(k),i=1,2),k=1,nunuco)
  240 format (10x,10(a8,4x))
      return
      end
