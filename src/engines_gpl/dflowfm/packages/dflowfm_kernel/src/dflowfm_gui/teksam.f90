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

   SUBROUTINE TEKSAM( XS,YS,ZS,NS,MET)

      use unstruc_colors
      use m_missing, only: DMISS
      use unstruc_opengl, only: jaopengl
      use unstruc_display

      implicit none
      double precision :: deltx, RC
      double precision :: delty
      double precision :: deltz
      double precision :: dscr
      double precision :: hrc
      integer :: i, KMOD
      integer :: jastart
      integer :: key
      integer :: mcs
      integer :: ncol
      integer :: ncs
      integer :: ndraw
      integer :: ns1
      double precision :: wpqr
      double precision :: x
      double precision :: xold
      double precision :: y
      double precision :: yold
      double precision :: z
      double precision :: zfac
      double precision :: zupw
      integer :: NS,MET
      double precision :: XS(NS), YS(NS), ZS(NS)
!     TEKEN SAMPLES
      COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /SAMPLESADM/  MCS,NCS,NS1
      double precision :: VS(4,4)
      logical inview

      IF (NS .EQ. 0 .OR. MET .EQ. 0) RETURN
      IF (MET .EQ. 4 .OR. MET .EQ. 5) CALL SETTEXTSIZE()
      RC      = 1.7d0*RCIR
      HRC     = RCIR/2
      JASTART = 0
      XOLD    = XS(1)
      YOLD    = YS(1)
      KMOD    = MAX(1,NS/100)
      key     = 0

!     Fix for OpenGL rendering
      if ( jaopengl.eq.1 .and. MET.eq.1 ) then
         MET = 7
      end if

      if (met <= 0) then
          return
      end if

      if (met == 5) then
          CALL SETCOL(KLSAM)
      else
          call minmxsam()
      endif

      DO 20 I = 1,NS
         IF (MOD(I,KMOD) .EQ. 0) THEN
            CALL HALT2(KEY)
            IF (KEY .EQ. 1) RETURN

         ENDIF
         X = XS(I)
         Y = YS(I)
         Z = ZS(I)

         if ( Z.EQ.DMISS ) cycle ! SPvdP: structured sample data may comprise missing values

         IF (INVIEW (X,Y) ) THEN
            IF (NDRAW(9) .EQ. 2) THEN
!               CALL VIEW(XS(I),YS(I),ZS(I),X0S,Y0S,VS,X,Y,ZC)
            ENDIF
            IF (MET .ne. 5) THEN
               CALL ISOCOL2(Z,NCOL)
            ENDIF
            IF (MET .EQ. 1 .OR. MET .EQ. 2) THEN
               IF (NDRAW(9) .EQ. 1) THEN
!
!                  CALL MOVABS(X,Y)
!                  CALL CIR(RCIR)
!!                 CALL HTEXT(ZS(I),X,Y)

                  call box(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)

                  IF (MET .EQ. 2) THEN
                     CALL MOVABS(X,Y)
                     CALL IGRFILLPATTERN(0,0,0)
                     CALL SETCOL(1)
                     CALL CIR(RCIR)
                     CALL IGRFILLPATTERN(4,0,0)
                  ENDIF

               ELSE IF (NDRAW(9) .EQ. 2) THEN
                  IF (MET .EQ. 1) THEN
!                     CALL PERREC(XS(I),YS(I),ZS(I),RC,NCOL,NCOL)
                  ELSE
!                     CALL PERREC(XS(I),YS(I),ZS(I),RC,NCOL,0)
                  ENDIF
               ENDIF
            ELSE IF (MET .EQ. 3) THEN
               CALL PTABS(X,Y)
            ELSE IF (MET .EQ. 4 .OR. MET .EQ. 5) THEN
               CALL HTEXT(ZS(I),X,Y)
            ELSE IF (MET .EQ. 6) THEN
               CALL MOVABS(X,Y)
               CALL CIR(RCIR)
               CALL HTEXT(ZS(I),X+rcir,Y)
            ELSE IF (MET .EQ. 7) THEN
               CALL KREC5(X,Y,HRC,HRC)
            ENDIF
         ELSE
            JASTART = 0
         ENDIF
   20 CONTINUE
      CALL IGRFILLPATTERN(4,0,0)
      CALL IGRCHARDIRECTION('H')
      RETURN
      END SUBROUTINE TEKSAM