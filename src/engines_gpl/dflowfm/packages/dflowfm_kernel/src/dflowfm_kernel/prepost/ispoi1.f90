      !> Checks whether a point is (almost) one of the polyline points.
      !!
      !! Checks at a radius dcir around all polyline points and sets
      !! input coordinates to the exact polyline point coordinates when
      !! it is found.
      SUBROUTINE ISPOI1( X, Y, N, XL, YL, MV)
      use m_wearelt
      use m_missing
      implicit none
      integer :: i
      integer :: ishot
      integer :: m1
      integer :: m2
      integer :: ns

!     is dit een POLYGpunt?
      integer,          intent(in)    :: N      !< Index of last filled polyline point (npol<=maxpol)
      double precision, intent(in)    :: X(n), Y(n) !< Entire polyline coordinate arrays.
      double precision, intent(inout) :: XL, YL !< x- and y-coordinates of the point to be checked (set to exact point coordinates when found).
      integer,          intent(out)   :: MV     !< The index of the polygon point (if found, otherwise 0)


      integer :: MVOL

      DATA MVOL /0/
      MV    = 0
      ISHOT = 0
      NS    = N
!
  666 CONTINUE
      ! If a previous point was found in a previous call (mvol/=0)
      ! then first search 'nearby' in poly  (500 pts to the left and right)
      ! If this fails (goto 666 with ishot==1), reset search range to entire poly.
      IF (ISHOT .EQ. 0 .AND. MVOL .NE. 0) THEN
         M1    = MAX(1,MVOL - 500)
         M2    = MIN(NS,MVOL + 500)
         ISHOT = 1
      ELSE
         M1    = 1
         M2    = NS
         ISHOT = 0
      ENDIF
!
      DO 10 I = M1,M2
         IF (X(I) .NE. dmiss) THEN
            IF (ABS(XL - X(I)) .LT. RCIR) THEN
               IF (ABS(YL - Y(I)) .LT. RCIR) THEN
                  XL   = X(I)
                  YL   = Y(I)
                  MV   = I
                  MVOL = MV
                  CALL DISPNODE(MV)
                  RETURN
               ENDIF
            ENDIF
         ENDIF
   10 CONTINUE
!
      IF (ISHOT .EQ. 1) GOTO 666
      MVOL = 0
      CALL DISPNODE(MVOL)
      RETURN
      END SUBROUTINE ISPOI1
