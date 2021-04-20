!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2021.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! $Id$
! $HeadURL$

      SUBROUTINE REACLASSES(MINP,CLASS,NUMQ,NUMCLASS)
      USE M_MISSING
      implicit none
      integer :: ja
      integer :: k
      integer :: l
      integer :: minp
      integer :: numc
      integer :: numclass
      integer :: numq
      CHARACTER REC*132
      double precision :: CLASS(NUMQ,NUMCLASS)
      LOGICAL EMPTY


      CALL MISARR(CLASS,NUMQ,NUMCLASS)
      CALL ZOEKAL(MINp,REC,'TEKAL BLOCK INDICATORS',JA)
      if (ja .ne. 1) return

      L = 1
   10 CONTINUE
         READ(MINP,'(A)',END = 999) REC
         IF (REC(1:1) .NE. ' ' .OR. EMPTY(REC) ) THEN
            NUMC = L - 1
            RETURN
         ENDIF
         READ(REC,*,ERR = 888) (CLASS(K,L),K=1,NUMQ)
         L = L + 1
      GOTO 10

  888 CALL READERROR('READING 5 REALS BUT GETTING',REC,MINP)

  999 CALL EOFERROR(MINP)
      END
