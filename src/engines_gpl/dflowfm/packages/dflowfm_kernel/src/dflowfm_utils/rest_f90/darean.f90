 !> Computes the enclosed area and length of a polygon.
 !!
 !! Only the first polygon is considered; whenever a missing value
 !! is encountered, the polygon is 'closed'.
 SUBROUTINE dAREAN( XX, YY, N, DAREA, DLENGTH, DLENMX )
 USE m_missing
 use m_sferic
 use geometry_module, only: dbdistance, get_startend, comp_masscenter

 implicit none
 double precision, intent(in)  :: XX(N), YY(N) !< Polygon points.
 double precision, intent(out) :: DAREA   !< Area enclosed within polygon.
 double precision, intent(out) :: DLENGTH !< Length of polygon contour.
 double precision, intent(out) :: DLENMX  !< Length of longest segment in polygon contour.
 integer,          intent(in)  :: n       !< Nr. of polygon points.

 integer :: i, iu, nend, jstart, jend
 double precision :: DX, DY, Y0, DLE, Y
 double precision :: xcg, ycg
 integer :: jacounterclockwise
   DAREA   = 0d0
   DLENGTH = 0D0
   Y0      = 1d30
   NEND    = 0
   DLENMX  = 0.D0

   call get_startend(N,XX,YY,jstart,jend,dmiss)

   if ( jend.le.jstart ) return

   call comp_masscenter(jend-jstart+1, xx(jstart), yy(jstart), xcg, ycg, darea, jacounterclockwise, jsferic, jasfer3D, dmiss)

   !DO I  = jstart,jend
   !   IF (XX(I) .NE.  dXYMIS) THEN
   !      Y0   = MIN(Y0,YY(I))
   !      NEND = I
   !   ELSE
   !      ! dmiss encountered: end of first polygon.
   !      ! Only compute area for first polygon.
   !      EXIT
   !   ENDIF
   !ENDDO
   !
   DO I = jstart,jend
      IU = I + 1

      if ( iu.gt.jend ) iu=jstart

 !     IF (IU .GT. NEND) IU = 1
 !     IF (JSFERIC .EQ. 0) THEN
 !        DX    = ( XX(IU) - XX(I) )
 !        Y     = 0.5d0*(YY(IU)-Y0) + 0.5d0*(YY(I) -Y0)
 !     ELSE
 !        DX    = ( XX(IU) - XX(I) )*DG2RD*RA*COS( DG2RD*( YY(IU)+YY(I) )/2 )
 !        Y     = RA*DG2RD*(0.5d0*( YY(IU) + YY(I) )-Y0)
 !     ENDIF
 !     DAREA    = DAREA - DX*Y
      DLE      = DBDISTANCE( XX(I), YY(I), XX(IU), YY(IU), jsferic, jasfer3D, dmiss)
      DLENGTH  = DLENGTH + DLE
      DLENMX   = MAX (DLENMX, DLE)

   ENDDO
   !
   !DAREA = ABS(DAREA)
 RETURN
 END SUBROUTINE dAREAN
