!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
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

! $Id: polygon.f90 52266 2017-09-02 11:24:11Z klecz_ml $
! $HeadURL: https://repos.deltares.nl/repos/ds/branches/dflowfm/20161017_dflowfm_codecleanup/engines_gpl/dflowfm/packages/dflowfm_kernel/src/polygon.f90 $
   module m_tpoly !< tpoly-type polygon/polyline
   implicit none

   type tpoly
       integer                       :: len              !< number of polygon/polyline points
       double precision, allocatable :: x(:), y(:)       !< polygon/polyline point coordinates
       double precision, allocatable :: z(:)             !< polygon/polyline nodal value
       double precision              :: xmin, ymin       !< lower left bounding box coordinates
       double precision              :: xmax, ymax       !< upper right bounding box coordinates
       double precision              :: zmin, zmax       !< polygon/polyline nodal values upper and lower bounds
   end type tpoly

   contains

!  get the number of tpoly-type polygons/polylines
   function isize_tpoly(pli)
      implicit none

      type (tpoly), dimension(:), allocatable, intent(in) :: pli

      integer                                             :: isize_tpoly, num

      if ( allocated(pli) ) then
         num = size(pli)
      else
         num = 0
      end if

      do while ( num.gt.0 )
         if ( .not.allocated(pli(num)%x) .or. .not.allocated(pli(num)%y) .or. .not.allocated(pli(num)%z) ) then
            num = num-1
         else
            exit
         end if
      end do

      isize_tpoly = num

      return
   end function

!  copy and/or add polygon-type polygon/polyline to tpoly-type polygon/polyline
   subroutine pol_to_tpoly(num, pli, keepExisting)
      use m_polygon
      implicit none

      integer,                                     intent(out)   :: num      !< number of polygons/polylines
      type(tpoly),      dimension(:), allocatable, intent(inout) :: pli      !< tpoly-type polygons/polylines
      logical,                           optional, intent(in)    :: keepExisting   !< keep current polygons/polylines and add (.true.) or replace (.false., default)

      integer                                                    :: len      ! polyline length
      integer                                                    :: ipoint, istart, iend
      integer                                                    :: ierror

      ipoint = 1
      iend   = 1
      num    = 0

      if ( present(keepExisting) ) then
         if ( keepExisting ) then
!           get number of existing polygons
            num = isize_tpoly(pli)
         end if
      end if

      do while ( ipoint.lt.NPL .and. iend.lt.NPL)
         call get_startend(NPL-ipoint+1, xpl(ipoint:NPL), ypl(ipoint:NPL), istart, iend)
         istart = istart + ipoint-1
         iend   = iend   + ipoint-1

         len = iend-istart+1

         if ( len.gt.1 .and. iend.le.NPL ) then
            num = num+1
!           reallocate polyline
            if ( num.gt.size(pli) .or. .not.allocated(pli) ) then
               call realloc_tpoly(pli, max(int(1.2d0*dble(num)),1))
            end if
!           fill polyline
!           (re)allocate
            if ( allocated(pli(num)%x) ) deallocate(pli(num)%x)
            if ( allocated(pli(num)%y) ) deallocate(pli(num)%y)
            if ( allocated(pli(num)%z) ) deallocate(pli(num)%z)
            allocate(pli(num)%x(len))
            allocate(pli(num)%y(len))
            allocate(pli(num)%z(len))
            pli(num)%x   = xpl(istart:iend)
            pli(num)%y   = ypl(istart:iend)
            pli(num)%z   = zpl(istart:iend)
            pli(num)%len = len

            call set_props_tpoly(pli(num), ierror)
         end if

         ipoint = iend+2
      end do
   end subroutine pol_to_tpoly


!  copy tpoly-type polygon/polyline to polygon-type polygon/polyline
   subroutine tpoly_to_pol(pli,zval,iselect)
      use m_polygon
      use m_missing
      implicit none

      type(tpoly),      dimension(:), allocatable, intent(in)           :: pli   !< tpoly-type polygons/polylines
      double precision,                            intent(in), optional :: zval  !< only select polygons/polyline with zvalue zval
      integer,                                     intent(in), optional  :: iselect !< only copy iselect polygon/polyline

      integer                                                           :: i, j, len, num, istart, iend
      
      logical                                                           :: Ldoit
      
      double precision, parameter                                       :: dtol = 1d-8

!     get number of existing polygons
      num = isize_tpoly(pli)
      
      istart = 1
      iend = num
      
      if ( present(iselect) ) then
         istart = iselect
         iend = min(iselect,num)
      end if

      do i=istart,iend
         if ( allocated(pli(i)%x) .and. allocated(pli(i)%y) .and. allocated(pli(i)%z) ) then
            len = pli(i)%len
            if ( len.gt.0 ) then
            
               Ldoit = .true.
               if ( present(zval) ) then
                  if ( abs(pli(i)%z(1)-zval) .gt. dtol ) then
                     Ldoit = .false.
                  end if
               end if
            
!              add DMISS if necessary
               if ( NPL.gt.0 .and. Ldoit ) then
                  if ( xpl(NPL).ne.DMISS .and. ypl(NPL).ne.DMISS ) then
                     call increasepol(NPL+1,1)
                     NPL = NPL+1
                     xpl(NPL) = DMISS
                     ypl(NPL) = DMISS
                     zpl(NPL) = DMISS
                  end if
               end if

!              increase polygon array
               call increasepol(NPL+len,1)

!              add polygon points
               do j=1,len
                  xpl(NPL+j) = pli(i)%x(j)
                  ypl(NPL+j) = pli(i)%y(j)
                  zpl(NPL+j) = pli(i)%z(j)
               end do
               NPL = NPL+len
            end if
         end if
      end do

      return
   end subroutine tpoly_to_pol


!> (re)allocate tpoly-type polygon/polyline
   subroutine realloc_tpoly(poly, N)
      implicit none

      type(tpoly), dimension(:), allocatable, intent(inout) :: poly !< polygon/polyline
      integer,                                intent(in)    :: N    !< objective polygon/polyline size

      type(tpoly), dimension(:), allocatable                :: poly_loc

      integer                                               :: i, len, lenx, leny, lenz

      len = 0

   !  make local copy of poly
      if ( allocated(poly) ) then
         len = size(poly)

         if ( N.lt.len ) return  ! no need to realloc

         allocate(poly_loc(len))
         do i=1,len
            if ( allocated(poly(i)%x) .and. allocated(poly(i)%y) .and. allocated(poly(i)%z) ) then
               lenx = size(poly(i)%x)
               leny = size(poly(i)%y)
               lenz = size(poly(i)%z)
               allocate(poly_loc(i)%x(lenx))
               allocate(poly_loc(i)%y(leny))
               allocate(poly_loc(i)%z(lenz))
               poly_loc(i)%x   = poly(i)%x
               poly_loc(i)%y   = poly(i)%y
               poly_loc(i)%z   = poly(i)%z
               poly_loc(i)%len = poly(i)%len
               poly_loc(i)%xmin = poly(i)%xmin
               poly_loc(i)%xmax = poly(i)%xmax
               poly_loc(i)%ymin = poly(i)%ymin
               poly_loc(i)%ymax = poly(i)%ymax
               poly_loc(i)%zmin = poly(i)%zmin
               poly_loc(i)%zmax = poly(i)%zmax
            end if
         end do

!        deallocate
         call dealloc_tpoly(poly)
      end if

!     allocate with new size
      allocate(poly(N))

!     copy old values
      do i=1,len
         if ( allocated(poly_loc(i)%x) .and. allocated(poly_loc(i)%y) .and. allocated(poly_loc(i)%z) ) then
            lenx = size(poly_loc(i)%x)
            leny = size(poly_loc(i)%y)
            lenz = size(poly_loc(i)%z)
            allocate(poly(i)%x(lenx))
            allocate(poly(i)%y(leny))
            allocate(poly(i)%z(lenz))
            poly(i)%x   = poly_loc(i)%x
            poly(i)%y   = poly_loc(i)%y
            poly(i)%z   = poly_loc(i)%z
            poly(i)%len = poly_loc(i)%len
            poly(i)%xmin = poly_loc(i)%xmin
            poly(i)%xmax = poly_loc(i)%xmax
            poly(i)%ymin = poly_loc(i)%ymin
            poly(i)%ymax = poly_loc(i)%ymax
            poly(i)%zmin = poly_loc(i)%zmin
            poly(i)%zmax = poly_loc(i)%zmax
         end if
      end do

!     deallocate local copy
      call dealloc_tpoly(poly_loc)

      return
   end subroutine realloc_tpoly


!> deallocate tpoly-type polygon/polyline
   subroutine dealloc_tpoly(poly)
      implicit none

      type(tpoly), dimension(:), allocatable, intent(inout) :: poly  !< polygon/polyline to be deallocated

      integer                                               :: i

      if ( .not.allocated(poly) ) return

      do i=1,size(poly)
         if ( allocated(poly(i)%x) ) deallocate(poly(i)%x)
         if ( allocated(poly(i)%y) ) deallocate(poly(i)%y)
         if ( allocated(poly(i)%z) ) deallocate(poly(i)%z)
      end do

      deallocate(poly)

      return
   end subroutine dealloc_tpoly


!> set the tpoly-type polygon/polyline properties
   subroutine set_props_tpoly(poly, ierror)
      use m_missing
      implicit none

      type(tpoly), intent(inout) :: poly     !< tpoly-type polygon/polyline
      integer,     intent(out)   :: ierror   !< error (1) or not (0)

      ierror = 1

      if ( .not. allocated(poly%x) .or. .not.allocated(poly%y) .or. .not.allocated(poly%z) ) goto 1234   ! safety

      poly%len = size(poly%x)
      if ( size(poly%y) .ne. poly%len ) goto 1234

      poly%zmin = DMISS
      poly%zmax = DMISS

      poly%xmin = minval(poly%x,mask=poly%x.ne.DMISS)
      poly%xmax = maxval(poly%x,mask=poly%x.ne.DMISS)
      poly%ymin = minval(poly%y,mask=poly%y.ne.DMISS)
      poly%ymax = maxval(poly%y,mask=poly%y.ne.DMISS)
      poly%zmin = minval(poly%z,mask=poly%z.ne.DMISS)
      poly%zmax = maxval(poly%z,mask=poly%z.ne.DMISS)

      ierror = 0
 1234 continue
 
      if ( ierror.ne.0 ) then
         continue
      end if

      return
   end subroutine set_props_tpoly


!> check if point is inside or outside a tpoly-type polygon
   subroutine dbpinpol_tpoly(poly, xp,yp,in)
      use m_missing, only: JINS

      implicit none

      type(tpoly),                      intent(inout) :: poly      !< tpoly-type polygon
      double precision,                 intent(in)    :: xp, yp    !< point coordinates
      integer,                          intent(inout) :: in        !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon, or just the other way around, depending on JINS

      integer                                         :: ierror

      logical                                         :: Linbound

      if ( in.eq. -1 ) then      ! initialization: determine the bounding box of the polygon
         call set_props_tpoly(poly, ierror)
         if ( ierror.ne.0 ) then
            in = 0
            goto 1234
         end if
      end if

      if ( poly%len .eq. 0 ) then
         in = 0
         goto 1234
      end if

      in = 0

!     check if point is in polygon bounding box
      Linbound = ( xp >= poly%xmin .and. xp <= poly%xmax .and. &
                   yp >= poly%ymin .and. yp <= poly%ymax )

      if ( .not.Linbound ) then
         if ( JINS.eq.1 ) then
            in = 0
         else
            in = 1
         end if
         goto 1234   ! done
      end if

      call pinpok(Xp, Yp, poly%len, poly%x, poly%y, in)

 1234 continue

      return
   end subroutine dbpinpol_tpoly
   

! in/outside multiple tpoly-type polygons
   subroutine dbpinpol_tpolies(pols, xp,yp,in,zval)
      use m_missing, only: JINS

      implicit none

      type(tpoly), dimension(:),        intent(inout) :: pols      !< tpoly-type polygons
      double precision,                 intent(in)    :: xp, yp    !< point coordinates
      integer,                          intent(out)   :: in        !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon, or just the other way around, depending on JINS
      double precision, optional,       intent(in)    :: zval      !< only consider polygons with zval z-value
      
      logical                                         :: Ldoit
      
      integer                                         :: i, N, in_loc
      
      double precision, parameter                     :: dtol=1d-8
      
      in = 0
      
      N = size(pols)
      
      do i=1,N
      
         if ( pols(i)%len.lt.1 ) cycle
      
         Ldoit = .true.
         if ( present(zval) ) then
            Ldoit =  abs(pols(i)%z(1)-zval).lt.dtol
         end if
               
         if ( Ldoit ) then
            in_loc = 0  ! do not initialize
            call dbpinpol_tpoly(pols(i), xp,yp,in_loc)
            if (in_loc.eq.1) then   ! switch in/out
               in = 1-in
            end if
         end if
      end do
      
      return
   end subroutine dbpinpol_tpolies


end module m_tpoly

      SUBROUTINE PINPOK(XL, YL, N, X, Y, INSIDE) ! basic subroutine
      USE M_MISSING
      implicit none
      integer :: N, INSIDE
      double precision :: X(N), Y(N), XL, YL

      integer          :: i, i1, i2, np
      double precision :: rechts, x1, x2, y1, y2, rm, rl

      IF (N .LE. 2) THEN
         INSIDE = 1 ; IF (jins .EQ. 0) INSIDE = 1 - INSIDE
      ELSE
         NP = 0
    5    CONTINUE
         NP = NP + 1
         IF (NP .LE. N) THEN
            IF ( X(NP) .NE. dmiss) GOTO 5
         ENDIF
         NP = NP - 1
         INSIDE = 0
         RECHTS = 0
         I = 0
   10    CONTINUE
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

   SUBROUTINE DPINPOK(X, Y, Z, NP, XP, YP, INSIDE)  ! multiple basic + jins (inside=1/outside=0) check
   use m_missing, only: jins
   implicit none
   double precision :: X,Y,Z
   integer :: NP, INSIDE
   double precision :: XP(NP), YP(NP)


   integer                                     :: ipoint         ! points to first part of a polygon-subsection in polygon array
   integer                                     :: istart, iend   ! point to start and and node of a polygon in polygon array respectively

   IF (NP .LE. 2) THEN
      INSIDE = 1
   ELSE

      ipoint = 1

      INSIDE = 0
      do while ( ipoint.lt.NP )
!        get polygon start and end pointer respectively
         call get_startend(NP-ipoint+1,xp(ipoint:NP),yp(ipoint:NP), istart, iend)
         istart = istart+ipoint-1
         iend   = iend  +ipoint-1

         if ( istart.ge.iend .or. iend.gt.NP ) exit ! done
         CALL PINPOK(X, Y, iend-istart+1, XP(istart:iend), YP(istart:iend), INSIDE)

         if ( INSIDE.eq.1 .and. JINS.eq.1 ) exit
         if ( INSIDE.eq.0 .and. JINS.eq.0 ) exit

!        advance pointer
         ipoint = iend+2
      end do   ! do while ( ipoint.lt.NP )
   ENDIF
   RETURN
   END SUBROUTINE DPINPOK


   subroutine dbpinpol(xp,yp,in) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN
   implicit none
      double precision,                 intent(in)    :: xp, yp    !< point coordinates
      integer,                          intent(inout) :: in        !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon

      call dbpinpol_optinside_perpol(xp, yp, 0, in)
   end subroutine dbpinpol

   !> The original dbpinpol routine, extended with an optional per-polygon-specified inside-mode.
   !! Used this for checking for many points whether they are inside the global polygons.
   !! Optionally, the global jins=1/other:inside/outside-mode can be replaced by an
   !! inside/outside mode per polygon: that should then be stored as a +1/-1 in the first
   !! zpl(istart) point of each polygon.
   subroutine dbpinpol_optinside_perpol(xp,yp, inside_perpol, in) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN
      use m_polygon
      use m_missing
      use m_alloc

      implicit none

      double precision,                 intent(in)    :: xp, yp        !< point coordinates
      integer,                          intent(in)    :: inside_perpol !< Specify whether or not (1/0) to use each polygon's first point zpl-value as the jins(ide)-option (only 0 or 1 allowed), or use the global JINS variable.
      integer,                          intent(inout) :: in            !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon

      integer                                         :: MAXPOLY=1000 ! will grow if needed

      double precision, allocatable, save             :: xpmin(:), ypmin(:), xpmax(:), ypmax(:)
      integer,                       save             :: Npoly
      integer,          allocatable, save             :: iistart(:), iiend(:)

      integer                                         :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                         :: istart, iend   ! point to start and and node of a polygon in polygon array respectively
      integer                                         :: ipoly          ! polygon number

      logical                                         :: Linit          ! initialization of polygon bounds, and start and end nodes respectively

      integer :: jins_opt !< The actual used jins-mode (either global, or per poly)

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
            call get_startend(NPL-ipoint+1,xpl(ipoint:NPL),ypl(ipoint:NPL), istart, iend)
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

         if ( inside_perpol.eq.1 .and. zpl(istart) /= dmiss ) then   ! only if third column was actually supplied
            jins_opt = int(zpl(istart)) ! Use inside-option per each polygon.
         else
            jins_opt = JINS ! Use global inside-option.
         end if

         IF (jins_opt == 1) THEN  ! inside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
                yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
               call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN)
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
               call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN)
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
   
   subroutine inwhichpolygon(xp,yp,in) ! ALS JE VOOR VEEL PUNTEN MOET NAGAAN OF ZE IN POLYGON ZITTEN
      use m_polygon
      use m_missing

      implicit none

      integer, parameter                              :: MAXPOLY=1000

      double precision,                 intent(in)    :: xp, yp    !< point coordinates
      integer,                          intent(inout) :: in        !< in(-1): initialization, out(0): outside polygon, out(1): inside polygon

      double precision, dimension(MAXPOLY), save      :: xpmin, ypmin, xpmax, ypmax
      integer,                              save      :: Npoly
      integer,          dimension(MAXPOLY), save      :: iistart, iiend

      integer                                         :: ipoint         ! points to first part of a polygon-subsection in polygon array
      integer                                         :: istart, iend   ! point to start and and node of a polygon in polygon array respectively
      integer                                         :: ipoly          ! polygon number

      logical                                         :: Linit          ! initialization of polygon bounds, and start and end nodes respectively

      if ( NPL.eq.0 ) then
         in = 1
         return
      end if

      Linit =  ( in.lt.0 )

      in = 0

!     initialization
      if ( Linit ) then
         ipoint = 1
         ipoly = 0
         do while ( ipoint.lt.NPL .and. ipoly.lt.MAXPOLY )
            ipoly = ipoly+1

!           get polygon start and end pointer respectively
            call get_startend(NPL-ipoint+1,xpl(ipoint:NPL),ypl(ipoint:NPL), istart, iend)
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
      end if

      do ipoly=1,Npoly
         istart = iistart(ipoly)
         iend   = iiend(ipoly)

         if ( istart.ge.iend .or. iend.gt.NPL ) exit ! done

         IF (JINS == 1) THEN  ! inside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
                yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
               call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN)
               if ( in.eq.1 ) then 
                  in = ipoly 
                  exit
               endif   
            endif
         ELSE                 ! outside polygon
            if (xp >= xpmin(ipoly) .and. xp <= xpmax(ipoly) .and. &
                yp >= ypmin(ipoly) .and. yp <= ypmax(ipoly) ) then
               call PINPOK(Xp, Yp, iend-istart+1, xpl(istart), ypl(istart), IN)
               if ( in.eq.0 ) then 
                   exit ! TODO: HK: should we not set in = ipoly here as well (since outside-check was successfull)
               endif    
            else
               in = ipoly
            endif
         ENDIF
      end do   ! do ipoly=1,Npoly

      return
   end subroutine inwhichpolygon 
   
   


    !> Determines the orientation of a polygon.
    !! iorient = 1: Counter-clockwise, -1: clockwise
    subroutine polorientation(X, Y, MAXPOL, N, iorient)
    implicit none
        double precision, intent(inout) :: X(MAXPOL), Y(MAXPOL) !< Entire polyline coordinate arrays
        integer,          intent(in)    :: N      !< Index of last filled polyline point (npol<=maxpol)
        integer,          intent(in)    :: MAXPOL !< Length of polyline coordinate arrays.
        integer,          intent(out)   :: iorient !< Returns the orientation (1: counter-clockwise, -1: clockwise)

    double precision :: cross, ymin
    integer :: i, i1, i2, i3

    i2 = 1
    ymin = Y(i2)
    do i = 2,N
        ! Find the bottom-rightmost point
        if (y(i) < ymin .or. y(i) == ymin .and. x(i) > x(i2)) then
            i2 = i
            ymin = y(i2)
        end if
    end do

    i1 = mod(i2-2+n, n) + 1 ! Previous point (possibly wrapped around end of array)
    i3 = mod(i2, n) + 1     ! Next point     ( "        " )
    cross = (x(i2)-x(i1))*(y(i3)-y(i2)) - (x(i3)-x(i2))*(y(i2)-y(i1))
    if (cross > 0) then
        iorient = 1
    else
        iorient = -1
    end if
    end subroutine polorientation
    
    
!> determine if point is "inside" (first) polygon (1) or not (0)
   subroutine pinpok3D(xp, yp, N, x, y, inside)
      use m_sferic
      use m_missing
      implicit none
      
      double precision,               intent(in)  :: xp, yp !< point coordinates
      integer,                        intent(in)  :: N      !< polygon size
      double precision, dimension(N), intent(in)  :: x, y   !< polygon coordinates
      integer,                        intent(out) :: inside !< inside (1) or not (0)
      
      double precision, dimension(:), allocatable :: xx, yy, zz
      
      double precision, dimension(3)              :: xiXxip1 ! x_i X x_{i+1}
      double precision, dimension(3)              :: xpXe ! xp X e
      
      double precision                            :: xxp, yyp, zzp
      
      double precision                            :: D, Di
      double precision                            :: xi, eta, zeta
      double precision                            :: lambda
      
      integer                                     :: i, ip1, num
      
      double precision, dimension(3)              :: ee
      
      double precision,               parameter   :: dtol = 1d-8
      
      if ( N.lt.3 ) then
         inside = 0
         if ( jins.ne.1 ) inside = 1-inside
         goto 1234
      end if
      
!     allocate
      allocate(xx(N), yy(N), zz(N))
      
!     get 3D polygon coordinates
      num = 0
      do i=1,N
         if ( x(i).ne.DMISS .and. y(i).ne.DMISS ) then
            num = num+1
            call sphertocart3D(x(i),y(i),xx(num),yy(num),zz(num))
         else if ( num.gt.0 ) then
            exit
         end if
      end do
      
      if ( num.lt.3 ) then
         inside = 0
         if ( jins.ne.1 ) inside=1-inside
         goto 1234  ! no valid polygon found
      end if
      
      call sphertocart3D(xp,yp,xxp,yyp,zzp)
      
!     get test direction: e_lambda
      lambda = xp*dg2rd
      ee = (/ -sin(lambda), cos(lambda), 0d0 /)
      
!     loop over polygon sections
      inside = 0
      do i=1,num
         ip1 = i+1; if ( ip1.gt.num ) ip1=ip1-num
         
         xiXxip1 = (/ yy(i)*zz(ip1) - zz(i)*yy(ip1),   &
                      zz(i)*xx(ip1) - xx(i)*zz(ip1),   &
                      xx(i)*yy(ip1) - yy(i)*xx(ip1) /)
                     
         xpXe = (/ yyp*ee(3) - zzp*ee(2),  &
                   zzp*ee(1) - xxp*ee(3),  &
                   xxp*ee(2) - yyp*ee(1) /)
                     
         D = xiXxip1(1)*ee(1) + xiXxip1(2)*ee(2) + xiXxip1(3)*ee(3)
         
         if ( abs(D).gt.dtol ) then
            Di = 1d0/D
            xi   = -( xpXe(1)*xx(ip1) + xpXe(2)*yy(ip1) + xpXe(3)*zz(ip1) ) * Di
            eta  =  ( xpXe(1)*xx(i)   + xpXe(2)*yy(i)   + xpXe(3)*zz(i)   ) * Di
            zeta = -( xiXxip1(1)*xxp  + xiXxip1(2)*yyp  + xiXxip1(3)*zzp  ) * Di
         else
!           enforce no intersection
            xi   = -1d0
            eta  = -1d0
            zeta = -1d0
         end if
         
         if ( zeta.eq.0d0 ) then
            inside=1
            if ( jins.eq.0 ) inside=1-inside
            goto 1234
         else if ( xi.ge.0d0 .and. eta.gt.0d0 .and. zeta.gt.0d0 ) then
            inside = 1-inside
         end if
                     
      end do
      
      if ( jins.eq.0 ) inside=1-inside
      
 1234 continue      
!     deallocate
      if ( allocated(xx) ) deallocate(xx)
      if ( allocated(yy) ) deallocate(yy)
      if ( allocated(zz) ) deallocate(zz)
      
      return
   end subroutine pinpok3D
