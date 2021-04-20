  subroutine crosspoly(xa,ya,xb,yb,xpl,ypl,npl,XM,YM,CRPM,JA,isec, distanceStartPolygon)

  use m_missing
  use m_sferic, only: jsferic, jasfer3D
  use geometry_module, only: crossinbox, dbdistance

  implicit none
  integer                                   :: npl, ja
  integer, intent(out)                      :: isec                 !< crossed polyline section (>0) or not crossed (0)
  !locals
  double precision                          :: xa, xb, ya, yb, xm, ym, crpm
  double precision                          :: xpl(npl), ypl(npl)
  double precision, intent(inout)           :: distanceStartPolygon !< distance from the start point of the polygon

  integer :: jacros
  integer :: k
  integer :: k1
  integer :: k2
  integer :: ku
  double precision :: XP1, YP1, XP2, YP2, sl, sm, XCR, YCR, CRP, currentSegmentLength

  isec = 0
  JA = 0
  distanceStartPolygon = 0.0d0
  DO K = 1,NPL - 1
     KU  = K + 1
     XP1 = XPL(K ) ; YP1 = YPL(K )
     XP2 = XPL(KU) ; YP2 = YPL(KU)
     currentSegmentLength = dbdistance(xp1,yp1,xp2,yp2, jsferic, jasfer3D, dmiss)

     if ( xp1.eq.DMISS .or. yp1.eq.DMISS .or. xp2.eq.DMISS .or. yp2.eq.DMISS ) cycle   ! SPvdP: added

     CALL CROSSinbox (XP1, YP1, XP2, YP2, Xa, Ya, Xb, Yb, jacros, sl, sm, XCR, YCR, CRP, jsferic, dmiss)

     if (jacros == 1) then
        JA = JA+1
        XM = XCR
        YM = YCR
        crpm = crp
        isec = k
        distanceStartPolygon = distanceStartPolygon + currentSegmentLength * sl
        return ! SPvdP: added
     end if
     distanceStartPolygon = distanceStartPolygon + currentSegmentLength
  end do

  end subroutine crosspoly
