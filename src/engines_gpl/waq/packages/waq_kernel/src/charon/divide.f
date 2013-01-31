!!  Copyright(C) Stichting Deltares, 2012.
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

      function divide(divdnd, divsor)
c           4-69
c           check for overflow before dividing.  if result will be
c        greater than or equal to largest value set result to plus or
c        minus ''infinity''.  aquot contains the number that represents
c        infinity.  it was chosen so that it could be recognized on
c        output.
c           subprogram start sets the values for anumbr and aquot.
c        anumlg is the logarithm of anumbr.  iquot is a toggle.  it is
c        zero if no overflow, one if overflow.
c           divide uses double precision version of
c              alog
      implicit real*8 (a-h,o-z)
      common /ovflo/ anumbr, anumlg, aquot, iquot
      iquot = 0
c        store sign values of dividend and divisor as +1 or -1
      sgndnd = 1.0
      sgnsor = 1.0
      if (divdnd.lt.0.0) sgndnd = -1.0
      if (divsor.lt.0.0) sgnsor = -1.0
      if (divsor.eq.0.0) go to 10
      if (divdnd.eq.0.0) go to 20
      if (dlog(dabs(divdnd)).lt.(dlog(dabs(divsor))+anumlg)) go to 20
c        set quotient to infinity
   10 divide = sgndnd*sgnsor*aquot
      iquot = 1
      go to 30
c        compute quotient
   20 divide = divdnd/divsor
   30 return
      end
