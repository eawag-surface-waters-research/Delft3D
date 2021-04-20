      SUBROUTINE MAPPRO(XX,YY,XG,YG,IZONE,NZONE,IHEM,ITYPE,JSFERIC,INIA)
      USE M_MISSING
      !use proj4
      implicit none
      integer :: ierr
      integer :: ini
      integer :: inia
      integer :: itype
      integer :: izone
      integer :: jsferic
      integer :: nzone
      integer :: ihem
!      type(pjf90_proj), save :: proj_latlon, proj_magsirwest
!      double precision, save,pointer :: xp(:), yp(:) ! AvD: temp
      double precision :: A,E
      DOUBLE PRECISION :: XX,YY,XG,YG

      SAVE A,E
      DATA  INI /0/
      IF (INI .EQ. 0) THEN
         CALL SETELLIPS(3) ! WGS84
         INI = 1
!         ierr = pjf90_init_plus(proj_latlon, &
!            '+proj=latlong +datum=WGS84')
!         ierr = pjf90_init_plus(proj_magsirwest, &
!            '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-77.0775079166666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ')
!         ierr = pjf90_init_plus(proj_utm31,
!     +   '+proj=utm +zone=31 +datum=WGS84')
!         ierr = pjf90_init_plus(proj_rdnew,
!     +  '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889'
!     +//' +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m'
!     +//' +no_defs')
!         allocate(xp(1),yp(1))
      ENDIF

      XG = DXYMIS
      YG = DXYMIS
      IF (JSFERIC .EQ. 0) THEN        ! Cartesisch => Spherisch
         IF (ITYPE .EQ. 0) THEN  ! except for itype = 0
            CALL TRAROT(XX,YY,XG,YG)
         ELSE IF (ITYPE .EQ. 1) THEN  ! UTM
!            xp(1) = xx
!            yp(1) = yy
!            ierr = pjf90_transform(proj_magsirwest,proj_latlon, 1, 1, xp, yp, null())
!            xg = xp(1)*RAD_TO_DEG
!            yg = yp(1)*RAD_TO_DEG
            !CALL UTMGEO(XX,YY,XG,YG,IZONE,IERR) ! IZONE = input !TMP disable
            CALL UTMGEO2(XX,YY,XG,YG,IZONE,IHEM,IERR) ! IZONE = input !TMP disable
         ELSE IF (ITYPE .EQ. 2) THEN  ! Amersfoorts
            CALL RDGEO(XX,YY,XG,YG,0)
         ELSE IF (ITYPE .EQ. 3) THEN  ! RD (Ofwel Parijs)
            CALL RDGEO(XX,YY,XG,YG,1)
         ELSE IF (ITYPE .EQ. 4) THEN  ! MERCATOR
            CALL MERCGEO(XX,YY,XG,YG)
         ELSE IF (ITYPE .EQ. -1) THEN  ! AFFINE
            CALL AFFINE(XX,YY,XG,YG,INIA)
         ENDIF
      ELSE IF (JSFERIC .EQ. 1) THEN   ! Spherisch  => Cartesisch
         IF (ITYPE .EQ. 1) THEN       ! UTM
            CALL GEOUTM (XX,YY,XG,YG,IZONE,NZONE,IERR) ! IZONE = output
         ELSE IF (ITYPE .EQ. 2) THEN  ! Amersfoorts
            CALL GEORD(XX,YY,XG,YG,0)
         ELSE IF (ITYPE .EQ. 3) THEN  ! RD (Ofwel Parijs)
            CALL GEORD(XX,YY,XG,YG,1)
         ELSE IF (ITYPE .EQ. 4) THEN  ! MERCATOR
            CALL GEOMERC(XX,YY,XG,YG)
         ELSE IF (ITYPE .EQ. -1) THEN  ! AFFINE
            CALL AFFINE(XX,YY,XG,YG,INIA)
         ENDIF
      ENDIF
      RETURN
      END SUBROUTINE MAPPRO
