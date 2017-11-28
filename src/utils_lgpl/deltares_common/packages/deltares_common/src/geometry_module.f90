module geometry_module
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2017.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.            
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!
!    Function: - Various geometrical routines
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   implicit none

   private

   !
   ! functions and subroutines
   !
   public :: clockwise
   public :: pinpok
   public :: dpinpok
   public :: dbdistance
   public :: getdx
   public :: getdy
   public :: getdxdy
   public :: sphertocart3D
   public :: dbpinpol
   public :: cross
   public :: dbpinpol_optinside_perpol
   public :: get_startend

   interface clockwise
      module procedure clockwise_sp
      module procedure clockwise_hp
   end interface clockwise

   contains

      !> Checks orientation of a polygon in single precision.
      function clockwise_sp(x,y) result(cw)
          use precision
      
          implicit none
          
          real(sp), dimension(:), intent(in) :: x   !< x-coordinates
          real(sp), dimension(:), intent(in) :: y   !< y-coordinates
          logical                            :: cw  !< true if clockwise, false if not
          
          integer                             :: n        !< number of points in polygon
          real(hp), dimension(:), allocatable :: xhp      !< temporary double precision x-coordinates
          real(hp), dimension(:), allocatable :: yhp      !< temporary double precision y-coordinates
          
          n = size(x)
          allocate(xhp(n),yhp(n))
          xhp = real(x,hp)
          yhp = real(y,hp)
          cw = clockwise_hp(xhp,yhp)
          deallocate(xhp,yhp)
      end function clockwise_sp

      !> Checks orientation of a polygon in high precision.
      function clockwise_hp(x,y) result(cw)
          use precision
      
          implicit none
          
          real(hp), dimension(:), intent(in) :: x   !< x-coordinates
          real(hp), dimension(:), intent(in) :: y   !< y-coordinates
          logical                            :: cw  !< true if clockwise, false if not
          
          integer :: i        !< loop variable
          integer :: i0       !< index of lowest left most point
          integer :: in       !< index of next point (not equal to x0,y0)
          integer :: ip       !< index of previous point (not equal to x0,y0)
          integer :: n        !< number of points in polygon
          real(hp) :: an      !< angle of next point compared to horizontal x-axis
          real(hp) :: ap      !< angle of previous point compared to horizontal x-axis
          real(hp) :: x0      !< x-coordinate of point i0
          real(hp) :: y0      !< y-coordinate of point i0
          
          !
          ! select lowest left most point
          !
          n = size(x)
          x0 = x(1)
          y0 = y(1)
          i0 = 1
          do i = 2, n
              if ( x(i)<x0 ) then
                  x0 = x(i)
                  y0 = y(i)
                  i0 = i
              elseif (x(i)==x0 .and. y(i)<y0) then
                  y0 = y(i)
                  i0 = i
              endif
          enddo
          !
          ! find point before
          ! note that this will give an infinite loop if n==1 or more generally if all(x==x0 and y==y0)
          !
          ip = i0
          do while (x(ip)==x0 .and. y(ip)==y0)
              if (ip==1) then
                  ip = n
              else
                  ip = ip-1
              endif
          enddo
          !
          ! find point after
          !
          in = i0
          do while (x(in)==x0 .and. y(in)==y0)
              if (in==n) then
                  in = 1
              else
                  in = in+1
              endif
          enddo
          !
          ! if "point after" lies above "point before" the orientation is clockwise ...
          !
          an = atan2(y(in)-y0,x(in)-x0)
          ap = atan2(y(ip)-y0,x(ip)-x0)
          cw = an>ap
      end function clockwise_hp
      
      !
      ! PINPOK
      !
      subroutine pinpok(XL, YL, N, X, Y, INSIDE, jins, dmiss) ! basic subroutine
      
      implicit none
      
      integer                      :: N, INSIDE
      integer, intent(in)          :: jins
      double precision, intent(in) :: dmiss
      double precision             :: X(N), Y(N), XL, YL

      integer          :: i, i1, i2, np
      double precision :: rechts, x1, x2, y1, y2, rm, rl

      IF (N .LE. 2) THEN
         INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
      ELSE
         NP = 0
5        CONTINUE
         NP = NP + 1
         IF (NP .LE. N) THEN
            IF ( X(NP) .NE. dmiss) GOTO 5
         ENDIF
         NP = NP - 1
         INSIDE = 0
         RECHTS = 0
         I = 0
10       CONTINUE
         I1 = I + 1  !MOD(I,NP) + 1
         I2 = I1 + 1 !MOD(I1,NP) + 1
         IF (I2 > NP) I2 = 1
         X1 = X(I1)
         X2 = X(I2)
         Y1 = Y(I1)
         Y2 = Y(I2)
         IF (XL .GE. MIN(X1,X2) .AND. XL .LE. MAX(X1,X2) ) THEN
            !           tussen of op lijnstuk
            IF (XL .EQ. X1 .AND. YL .EQ. Y1 .OR.                                     &
               !              op punt 1
               (X1 .EQ. X2 .AND. YL .GE. MIN(Y1,Y2) .AND. YL .LE. MAX(Y1,Y2) ) .OR.  &
               !              op verticale lijn
               (YL .EQ. Y1 .AND. Y1 .EQ. Y2)  ) THEN
            !              op horizontale lijn
            INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
            RETURN
            ELSE IF (X1 .NE. X2) THEN
               !              scheve lijn
               RL = ( XL - X1 )  / ( X2 - X1 )
               RM = ( Y1 - YL )  + RL * ( Y2 - Y1 )
               IF (RM .EQ. 0) THEN
                  !                 op scheve lijn
                  INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
                  RETURN
               ELSE IF (RM .GT. 0d0) THEN
                  !                 onder scheve lijn
                  IF (XL .EQ. X1 .OR. XL .EQ. X2) THEN
                     IF (X1 .GT. XL .OR. X2 .GT. XL) THEN
                        RECHTS = RECHTS + 1
                     ENDIF
                  ENDIF
                  INSIDE = 1 - INSIDE
               ENDIF
            ENDIF
         ENDIF
         I = I + 1
         IF (I .LT. NP) GOTO 10
         IF (MOD(RECHTS,2d0) .NE. 0) INSIDE = 1 - INSIDE
      ENDIF
      IF (jins .EQ. 0) INSIDE = 1 - INSIDE
      RETURN
      END SUBROUTINE PINPOK

      !
      ! DPINPOK
      !
      subroutine DPINPOK(X, Y, Z, NP, XP, YP, INSIDE, jins, dmiss)  ! multiple basic + jins (inside=1/outside=0) check
            
      implicit none
      
      double precision             :: X,Y,Z
      integer                      :: NP, INSIDE
      integer, intent(in)          :: jins
      double precision, intent(in) :: dmiss
      double precision             :: XP(NP), YP(NP)

      integer                                     :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                     :: istart, iend   ! point to start and and node of a polygon in polygon array respectively

      IF (NP .LE. 2) THEN
         INSIDE = 1
      ELSE

         ipoint = 1

         INSIDE = 0
         do while ( ipoint.lt.NP )
            !        get polygon start and end pointer respectively
            call get_startend(NP-ipoint+1,xp(ipoint:NP),yp(ipoint:NP), istart, iend, dmiss)
            istart = istart+ipoint-1
            iend   = iend  +ipoint-1

            if ( istart.ge.iend .or. iend.gt.NP ) exit ! done
            CALL PINPOK(X, Y, iend-istart+1, XP(istart:iend), YP(istart:iend), INSIDE, jins, dmiss)

            if ( INSIDE.eq.1 .and. JINS.eq.1 ) exit
            if ( INSIDE.eq.0 .and. JINS.eq.0 ) exit

            !        advance pointer
            ipoint = iend+2
         end do   ! do while ( ipoint.lt.NP )
      ENDIF
      RETURN
      END SUBROUTINE DPINPOK

      !> compute distance from (x1,y1) to (x2,y2)
      double precision function dbdistance(x1,y1,x2,y2, jsferic, jasfer3D, dmiss)    ! distance point 1 -> 2
      implicit none
      double precision, intent(in) :: x1, y1, x2, y2
      ! locals
      double precision              :: ddx, ddy, rr
      double precision              :: xx1, yy1, zz1, xx2, yy2, zz2
      integer, intent(in)           :: jsferic, jasfer3D
      double precision, intent(in)  :: dmiss

      if ( x1.eq.dmiss .or. x2.eq.dmiss .or. y1.eq.dmiss .or. y2.eq.dmiss ) then
         dbdistance = 0d0
         return
      end if

      if ( jsferic.eq.1 .and. jasfer3D.eq.1 ) then
         call sphertocart3D(x1,y1,xx1,yy1,zz1, jsferic)
         call sphertocart3D(x2,y2,xx2,yy2,zz2, jsferic)
         dbdistance = sqrt( (xx2-xx1)**2 + (yy2-yy1)**2 + (zz2-zz1)**2 )
      else
         ddx = getdx(x1,y1,x2,y2,jsferic)
         ddy = getdy(x1,y1,x2,y2,jsferic)
         rr  = ddx*ddx + ddy*ddy
         if (rr == 0d0) then
            dbdistance = 0d0
         else
            dbdistance = sqrt(rr)
         endif
      endif

      end function dbdistance

      !
      ! getdx
      !
      double precision function getdx(x1,y1,x2,y2, jsferic)

      use mathconsts, only: degrad_hp
      use physicalconsts, only: earth_radius, dtol_pole
      implicit none
      double precision                    :: x1, y1, x2, y2
      double precision                    :: xx1, yy1, xx2, yy2
      double precision                    :: diff1, diff2, dy, r, dx2
      integer, intent(in)                 :: jsferic
      double precision                    :: csphi

      if (jsferic == 1) then

         ! fix for poles
         diff1 = abs(abs(y1)-90d0)
         diff2 = abs(abs(y2)-90d0)
         if ( (diff1.le.dtol_pole .and. diff2.gt.dtol_pole) .or. &
            (diff1.gt.dtol_pole .and. diff2.le.dtol_pole) ) then
         getdx = 0d0
         return
         end if

         xx1   = x1
         xx2   = x2
         if      (xx1 - xx2 >  180d0) then
            xx1 = xx1 - 360d0
         else if (xx1 - xx2 < -180d0) then
            xx1 = xx1 + 360d0
         endif
         xx1   = xx1*degrad_hp
         xx2   = xx2*degrad_hp
         yy1   = y1 *degrad_hp
         yy2   = y2 *degrad_hp
         csphi = dcos(0.5d0*(yy1+yy2))
         getdx = earth_radius*csphi*(xx2-xx1)
      else
         getdx = x2-x1
      endif
      end function getdx

      !
      ! getdy
      !
      double precision function getdy(x1,y1,x2,y2, jsferic)
      
      use mathconsts, only: degrad_hp
      use physicalconsts, only: earth_radius, dtol_pole

      implicit none
      
      double precision :: x1, y1, x2, y2
      double precision :: xx1, yy1,yy2
      integer, intent(in)  :: jsferic

      if (jsferic == 1) then
         yy1   = y1*degrad_hp
         yy2   = y2*degrad_hp
         getdy = earth_radius*(yy2-yy1)
      else
         getdy = y2-y1
      endif
      end function getdy

      !
      ! getdxdy
      !
      subroutine getdxdy(x1,y1,x2,y2,dx,dy, jsferic)
      
      implicit none
      
      double precision :: x1, y1, x2, y2, dx, dy, dx2, dy2, dum
      integer, intent(in)  :: jsferic
      
      if (Jsferic == 1) then
         dx = getdx(x1,y1,x2,y2,jsferic)
         dy = getdy(x1,y1,x2,y2,jsferic)
      else
         dx = x2-x1
         dy = y2-y1
      endif

      end subroutine getdxdy
      
      !
      ! sphertocart3D
      !
      subroutine sphertocart3D(x1,y1,xx1,yy1,zz1, jsferic) ! from spherical 2D to Cartesian 3D coordinates
      use mathconsts, only: degrad_hp
      use physicalconsts, only: earth_radius, dtol_pole
      implicit none
      double precision     :: x1,y1,xx1,yy1,zz1,rr
      integer, intent(in)  :: jsferic
      
      if ( jsferic.eq.1 ) then
         zz1 = earth_radius*sin(y1*degrad_hp)
         rr  = earth_radius*cos(y1*degrad_hp)
         xx1 = rr*cos(x1*degrad_hp)
         yy1 = rr*sin(x1*degrad_hp)
      else
         zz1 = 0d0
         xx1 = x1
         yy1 = y1
      end if
      
      end subroutine sphertocart3D
      
      !
      ! CROSS
      !
      subroutine CROSS(x1, y1, x2, y2, x3, y3, x4, y4, JACROS,SL,SM,XCR,YCR,CRP, jsferic, dmiss)
      
      implicit none
       
      double precision, intent(inout) :: crp !< crp (in)==-1234 will make crp (out) non-dimensional
      double precision                :: det
      double precision                :: eps
      integer                         :: jacros, jamakenondimensional 
      double precision                :: sl
      double precision                :: sm
      double precision, intent(in)    :: x1, y1, x2, y2, x3, y3, x4, y4
      double precision                :: x21, y21, x31, y31, x43, y43, xcr, ycr
      integer, intent(in)             :: jsferic
      double precision, intent(in)    :: dmiss

     
!     safety check on crp (in)
      if ( isnan(crp) ) then
         crp = 0d0
      end if

      ! Set defaults for no crossing at all:
      jamakenondimensional = 0
      if ( abs(crp+1234d0).lt.0.5d0 ) then
         jamakenondimensional = 1
         crp = 0d0
      endif
      
      JACROS = 0
      EPS    = 0.00001d0
      SL     = DMISS
      SM     = DMISS
!     SL     = LABDA TUSSEN 0 EN 1 OP EERSTE PAAR
!     Sm     = LABDA TUSSEN 0 EN 1 OP TWEEDE PAAR
      
      call getdxdy(x1,y1,x2,y2,x21,y21,jsferic)
      call getdxdy(x3,y3,x4,y4,x43,y43,jsferic)
      call getdxdy(x1,y1,x3,y3,x31,y31,jsferic)

      !X21 =  getdx(x1,y1,x2,y2)
      !Y21 =  getdy(x1,y1,x2,y2)
      !X43 =  getdx(x3,y3,x4,y4)
      !Y43 =  getdy(x3,y3,x4,y4)
      !X31 =  getdx(x1,y1,x3,y3)
      !Y31 =  getdy(x1,y1,x3,y3)

      DET =  X43*Y21 - Y43*X21

!     SPvdP: make eps have proper dimension
      EPS = max(EPS*MAXVAL((/ X21,Y21,X43,Y43,X31,Y31 /)), tiny(0d0))
      IF (ABS(DET) .LT. EPS) THEN
         RETURN
      ELSE
         SM = (Y31*X21 - X31*Y21) / DET
         IF (ABS(X21) .GT. EPS) THEN
            SL = (SM*X43 + X31) / X21
         ELSE IF (ABS(Y21) .GT. EPS) THEN
            SL = (SM*Y43 + Y31) / Y21
         ELSE
            SL   = 0d0
         ENDIF
         IF (SM .GE. 0d0 .AND. SM .LE. 1d0 .AND. &
             SL .GE. 0d0 .AND. SL .LE. 1d0) THEN
            JACROS = 1
         ENDIF
         XCR    = X1 + SL*(X2-X1)
         YCR    = Y1 + SL*(Y2-Y1)
         if ( jamakenondimensional.eq.1 ) then  ! make crp non-dimensional (for spline2curvi)
            CRP    = -DET / ( sqrt(x21**2+y21**2) * sqrt(x43**2 + y43**2) + 1d-8 )
         else
            CRP    = -DET
         end if
      ENDIF
      RETURN
      
      end subroutine CROSS

      subroutine dbpinpol(xp,yp, in, dmiss, JINS, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN
      implicit none
      double precision,                 intent(in)    :: xp, yp    !< point coordinates
      integer,                          intent(inout) :: in        !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon
      integer                                         :: num
      double precision, intent(in)                    :: dmiss
      integer, intent(in)                             :: JINS, NPL
      double precision, intent(in)                    :: xpl(NPL), ypl(NPL), zpl(NPL)

      call dbpinpol_optinside_perpol(xp, yp, 0, 0, in, num, dmiss, JINS, NPL, xpl, ypl, zpl)
      
      end subroutine dbpinpol

      !> The original dbpinpol routine, extended with an optional per-polygon-specified inside-mode.
      !! Used this for checking for many points whether they are inside the global polygons.
      !! Optionally, the global jins=1/other:inside/outside-mode can be replaced by an
      !! inside/outside mode per polygon: that should then be stored as a +1/-1 in the first
      !! zpl(istart) point of each polygon.
      subroutine dbpinpol_optinside_perpol(xp, yp, inside_perpol, iselect, in, numselect, dmiss, JINS, NPL, xpl, ypl, zpl) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN

      use m_alloc

      implicit none

      double precision,                 intent(in)    :: xp, yp        !< point coordinates
      integer,                          intent(in)    :: inside_perpol !< Specify whether or not (1/0) to use each polygon's first point zpl-value as the jins(ide)-option (only 0 or 1 allowed), or use the global JINS variable.
      integer,                          intent(in)    :: iselect       !< use all polygons (0), only first-zpl<0 polygons (-1), or all but first-zpl<0 polygons (1)
      integer,                          intent(inout) :: in            !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon
      integer,                          intent(inout) :: numselect     !< number of polygons of "iselect" type considered

      integer                                         :: MAXPOLY=1000 ! will grow if needed

      double precision, allocatable, save             :: xpmin(:), ypmin(:), xpmax(:), ypmax(:)
      integer,                       save             :: Npoly
      integer,          allocatable, save             :: iistart(:), iiend(:)

      integer                                         :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                         :: istart, iend   ! point to start and and node of a polygon in polygon array respectively
      integer                                         :: ipoly          ! polygon number

      logical                                         :: Linit          ! initialization of polygon bounds, and start and end nodes respectively

      integer :: jins_opt !< The actual used jins-mode (either global, or per poly)
      double precision, intent(in)                    :: dmiss
      integer, intent(in)                             :: JINS, NPL
      double precision, intent(in)                    :: xpl(NPL), ypl(NPL), zpl(NPL)

      numselect = 0

      if ( NPL.eq.0 ) then
         in = 1
         return
      end if

      Linit =  ( in.lt.0 )

      in = 0

      !     initialization
      if ( Linit ) then
         !         write(6,"('dbpinpol: init... ', $)")
         ipoint = 1
         ipoly = 0
         call realloc(xpmin, maxpoly, keepExisting=.false.)
         call realloc(xpmax, maxpoly, keepExisting=.false.)
         call realloc(ypmin, maxpoly, keepExisting=.false.)
         call realloc(ypmax, maxpoly, keepExisting=.false.)
         call realloc(iistart, maxpoly, keepExisting=.false.)
         call realloc(iiend, maxpoly, keepExisting=.false.)

         do while ( ipoint.lt.NPL )
            ipoly = ipoly+1
            if (ipoly > maxpoly) then
               maxpoly = ceiling(maxpoly*1.1)
               call realloc(xpmin, maxpoly, keepExisting=.true.)
               call realloc(xpmax, maxpoly, keepExisting=.true.)
               call realloc(ypmin, maxpoly, keepExisting=.true.)
               call realloc(ypmax, maxpoly, keepExisting=.true.)
               call realloc(iistart, maxpoly, keepExisting=.true.)
               call realloc(iiend, maxpoly, keepExisting=.true.)
            end if

            !           get polygon start and end pointer respectively
            call get_startend(NPL-ipoint+1,xpl(ipoint:NPL),ypl(ipoint:NPL), istart, iend, dmiss)
            istart = istart+ipoint-1
            iend   = iend  +ipoint-1

            if ( istart.ge.iend .or. iend.gt.NPL ) exit ! done

            xpmin(ipoly) = minval(xpl(istart:iend))
            xpmax(ipoly) = maxval(xpl(istart:iend))
            ypmin(ipoly) = minval(ypl(istart:iend))
            ypmax(ipoly) = maxval(ypl(istart:iend))

            iistart(ipoly) = istart
            iiend(ipoly)   = iend

            !           advance pointer
            ipoint = iend+2
         end do   ! do while ( ipoint.lt.NPL .and. ipoly.lt.MAXPOLY )
         Npoly = ipoly

         !         write(6,"('done, Npoly=', I4)") Npoly
      end if

      do ipoly=1,Npoly
         istart = iistart(ipoly)
         iend   = iiend(ipoly)

         !         write(6,"('dbpinpol: ipoly=', I4, ', istart=', I16, ', iend=', I16)") ipoly, istart, iend


         if ( istart.ge.iend .or. iend.gt.NPL ) exit ! done

         if ( iselect.eq.-1 .and. (zpl(istart).eq.DMISS .or.  zpl(istart).ge.0) ) cycle
         if ( iselect.eq. 1 .and. (zpl(istart).ne.DMISS .and. zpl(istart).lt.0) ) cycle

         numselect = numselect+1

         if ( inside_perpol.eq.1 .and. zpl(istart) /= dmiss ) then   ! only if third column was actually supplied
            jins_opt = int(zpl(istart)) ! Use inside-option per each polygon.
         else
            jins_opt = JINS ! Use global inside-option.
         end if

         IF (jins_opt == 1) THEN  ! inside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
               yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
            call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN, jins, dmiss)
            if (jins_opt > 0 .neqv. JINS > 0) then ! PINPOK has used global jins, but polygon asked the exact opposite, so negate the result here.
               IN = 1-in   ! IN-1
            end if

            if ( in.eq.1 ) then
               exit
            end if
            endif
         ELSE                 ! outside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
               yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
            call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN, jins, dmiss)
            if (jins_opt > 0 .neqv. JINS > 0) then ! PINPOK has used global jins, but polygon asked the exact opposite, so negate the result here.
               IN = 1-in   ! IN-1
            end if

            if ( in.eq.1 ) then
               exit ! outside check succeeded, return 'true'.
            end if
            else
               in = 1 ! outside check succeeded (completely outside of polygon's bounding box), return 'true'.
               exit
            endif
         ENDIF
      end do   ! do ipoly=1,Npoly

      return
      end subroutine dbpinpol_optinside_perpol

      !>  get the start and end index of the first enclosed non-DMISS subarray
      subroutine get_startend(num, x, y, jstart, jend, dmiss)

      implicit none

      integer,                          intent(in)  :: num           !< array size
      double precision, dimension(num), intent(in)  :: x, y          !< array coordinates
      integer,                          intent(out) :: jstart, jend  !< subarray indices
      double precision, intent(in)                  :: dmiss

      !      find jstart and jend
      jend = 1
      jstart = jend

      if ( jend.ge.num ) return

      if ( x(jstart+1).eq.dmiss ) jstart = jstart+1
      if ( jstart.ge.num ) return

      do while( x(jstart).eq.dmiss )
         jstart = jstart+1
         if ( jstart.eq.num ) exit
      end do
      if ( x(jstart).eq.dmiss ) return

      jend   = jstart
      if ( jend.lt.num ) then
         do while( x(jend+1).ne.dmiss )
            jend = jend+1
            if ( jend.eq.num ) exit
         end do
      end if

      return
      end subroutine get_startend

   end module geometry_module
