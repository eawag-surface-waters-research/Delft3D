!!  Copyright (C)  Stichting Deltares, 2012-2018.
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

module test_geometry_module
    use geometry_module
    use physicalconsts, only: earth_radius
    use ftnunit
    use precision

    implicit none

    private
    public:: tests_geometry_module

    real(fp), parameter :: eps   = 1.0e-6_fp
    real(fp), parameter :: dmiss = -999.0_fp

contains
subroutine tests_geometry_module
    call test( test_inproduct,    'In-product of two vectors (2D/3D)' )
    call test( test_clockwise,    'Orientation of polygons' )
    call test( test_pinpok,       'Points inside/outside a polygon' )
    call test( test_dbdistance,   'Distance between points (sphere or plane)' )
end subroutine tests_geometry_module

subroutine test_inproduct
    real(fp) :: x1, y1, x2, y2
    real(fp), dimension(3) :: a, b
    real(fp) :: inproduct, expected

    integer  :: i
    real(fp), dimension(5,4) :: vector_inproduct = reshape( &
        [ 100.0_fp,    0.0_fp, 100.0_fp,   0.0_fp,  1.0e4_fp, &  ! Parallel vectors
          100.0_fp,  100.0_fp, 100.0_fp, 100.0_fp,  2.0e4_fp, &  ! Parallel vectors
          100.0_fp,    0.0_fp,   0.0_fp, 100.0_fp,  0.0e0_fp, &  ! Perpendicular vectors
          100.0_fp,  100.0_fp, 100.0_fp,-100.0_fp,  0.0e0_fp] &  ! Perpendicular vectors
        , [5,4] )
    real(fp), dimension(7,6) :: vector_inproduct_3d = reshape( &
        [ 100.0_fp,    0.0_fp,   0.0_fp, 100.0_fp,   0.0_fp,   0.0_fp,  1.0e4_fp, &  ! Parallel vectors
          100.0_fp,  100.0_fp,   0.0_fp, 100.0_fp, 100.0_fp,   0.0_fp,  2.0e4_fp, &  ! Parallel vectors
          100.0_fp,  100.0_fp, 100.0_fp, 100.0_fp, 100.0_fp, 100.0_fp,  3.0e4_fp, &  ! Parallel vectors
          100.0_fp,    0.0_fp,   0.0_fp,   0.0_fp, 100.0_fp,   0.0_fp,  0.0e0_fp, &  ! Perpendicular vectors
          100.0_fp,  100.0_fp,   0.0_fp, 100.0_fp,-100.0_fp,   0.0_fp,  0.0e0_fp, &  ! Perpendicular vectors
            0.0_fp,  100.0_fp, 100.0_fp,   0.0_fp,-100.0_fp, 100.0_fp,  0.0e0_fp, &  ! Perpendicular vectors
          100.0_fp,    0.0_fp, 100.0_fp,-100.0_fp,   0.0_fp, 100.0_fp,  0.0e0_fp] &  ! Perpendicular vectors
        , [7,6] )

    !
    ! The 2D routine to test is almost trivial ...
    !
    do i = 1,size(vector_inproduct,2)
        x1 = vector_inproduct(1,i); y1 = vector_inproduct(2,i)
        x2 = vector_inproduct(3,i); y2 = vector_inproduct(4,i)
        expected = vector_inproduct(5,i)

        inproduct = dotp( x1, y1, x2, y2 )

        call assert_comparable( inproduct, expected, eps, "The 2D inproduct should match the expected value" )
    enddo
    !
    ! Ditto the 3D routine to test
    !
    do i = 1,size(vector_inproduct_3d,2)
        a = vector_inproduct_3d(1:3,i)
        b = vector_inproduct_3d(4:6,i)
        expected = vector_inproduct_3d(7,i)

        inproduct = inprod( a, b )

        call assert_comparable( inproduct, expected, eps, "The 3D inproduct should match the expected value" )
    enddo
end subroutine test_inproduct

subroutine test_clockwise
    real(fp), dimension(4) :: x, y
    logical                :: isclockwise

    !
    ! Clockwise polygon
    !
    x = [0.0_fp, 0.0_fp, 1.0_fp, 1.0_fp]; y = [0.0_fp, 1.0_fp, 1.0_fp, 0.0_fp]
    isclockwise = clockwise( x, y )
    call assert_true( isclockwise, "The polygon should be identified as clockwise" )

    !
    ! Anti-clockwise polygon
    !
    x = [0.0_fp, 1.0_fp, 1.0_fp, 0.0_fp]; y = [0.0_fp, 0.0_fp, 1.0_fp, 1.0_fp]
    isclockwise = clockwise( x, y )
    call assert_false( isclockwise, "The polygon should be identified as anti-clockwise" )
end subroutine test_clockwise

subroutine test_pinpok
    real(fp), dimension(6) :: x, y
    real(fp)               :: xl, yl
    logical                :: isclockwise
    integer                :: jins
    integer                :: inside

    !
    ! Basic version: no missing points
    !
    x    = [0.0_fp, 0.0_fp, 1.0_fp, 1.0_fp, 0.0_fp, 0.0_fp]; y = [0.0_fp, 1.0_fp, 1.0_fp, 0.0_fp, 0.0_fp, 0.0_fp]

    xl   = -1.0_fp; yl = -1.0_fp ! Clearly outside
    jins = 1
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 0, "The point should be detected as outside the polygon (0)" )

    jins = 0
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 1, "The point should be detected as outside the polygon (1)" )

    xl   =  0.5_fp; yl =  0.5_fp ! Clearly inside
    jins = 1
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 1, "The point should be detected as inside the polygon (1)" )

    jins = 0
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 0, "The point should be detected as inside the polygon (0)" )

    xl   =  1.0_fp; yl =  0.5_fp ! On the border
    jins = 1
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 1, "A point on the border should be detected as inside the polygon (1)" )

    jins = 0
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 0, "A point on the border should be detected as outside the polygon (0)" )

    !
    ! Extended version: missing points in between
    !
    x    = [0.0_fp, 0.0_fp, dmiss, 1.0_fp, dmiss, 1.0_fp]; y = [0.0_fp, 1.0_fp, dmiss, 1.0_fp, dmiss, 0.0_fp]

    xl   = -1.0_fp; yl = -1.0_fp ! Clearly outside
    jins = 1
    call pinpok( xl, yl, 6, x, y, inside, jins, dmiss )
    call assert_equal( inside, 0, "The point should be detected as outside the polygon (0; extended)" )

    jins = 0
    call pinpok( xl, yl, 6, x, y, inside, jins, dmiss )
    call assert_equal( inside, 1, "The point should be detected as outside the polygon (1; extended)" )

    xl   =  0.5_fp; yl =  0.5_fp ! Clearly inside
    jins = 1
    call pinpok( xl, yl, 6, x, y, inside, jins, dmiss )
    call assert_equal( inside, 0, "The point should be detected as inside the polygon (1; extended)" )

    jins = 0
    call pinpok( xl, yl, 6, x, y, inside, jins, dmiss )
    call assert_equal( inside, 1, "The point should be detected as inside the polygon (0; extended)" )

    xl   =  1.0_fp; yl =  0.5_fp ! On the border
    jins = 1
    call pinpok( xl, yl, 6, x, y, inside, jins, dmiss )
    call assert_equal( inside, 0, "A point on the border should be detected as inside the polygon (1)" )

    jins = 0
    call pinpok( xl, yl, 4, x, y, inside, jins, dmiss )
    call assert_equal( inside, 1, "A point on the border should be detected as inside the polygon (0)" )
end subroutine test_pinpok

subroutine test_dbdistance
    real(fp) :: x1, y1, x2, y2, distance
    integer  :: i, jsferic, jasfer3d

    real(fp), dimension(5,9) :: coords
    real(fp), parameter      :: twopi = 2.0_fp * acos(-1.0_fp)

    !
    ! Note: this cannot be an initialisation expression, because earth_radius is
    !       not defined as a parameter!
    coords = reshape( &
       [  0.0_fp, 0.0_fp , 1.0_fp,  0.0_fp, 1.0_fp,               & ! Plane
          0.0_fp, 0.0_fp,  0.0_fp,  1.0_fp, 1.0_fp,               &
         30.0_fp, 0.0_fp,  0.0_fp, 40.0_fp,50.0_fp,               &
          0.0_fp, 0.0_fp, 90.0_fp,  0.0_fp,0.25_fp*twopi*earth_radius,  & ! Spherical
        -90.0_fp, 0.0_fp, 90.0_fp,  0.0_fp,0.50_fp*twopi*earth_radius,  &
          0.0_fp, 0.0_fp,  0.0_fp, 90.0_fp,0.25_fp*twopi*earth_radius,  &
          0.0_fp,90.0_fp,  0.0_fp,-90.0_fp,0.50_fp*twopi*earth_radius,  &
        -90.0_fp,45.0_fp, 90.0_fp,-45.0_fp,0.50_fp*twopi*earth_radius,  &
          0.0_fp, 0.0_fp,360.0_fp,  0.0_fp,0.00_fp*twopi*earth_radius], [5,9] ) ! The points coindice on the sphere

    !
    ! Distance between points in the plane
    !
    jsferic  = 0
    jasfer3d = 0
    do i = 1,3
        x1 = coords(1,i); y1 = coords(2,i); x2 = coords(3,i); y2 = coords(4,i)
        distance = dbdistance( x1, y1, x2, y2, jsferic, jasfer3d, dmiss )

        call assert_comparable( distance, coords(5,i), eps, "Distance between points in a plane" )
    enddo

    !
    ! Distance between points on a sphere (Earth)
    !
    jsferic  = 1
    jasfer3d = 1
    do i = 4,size(coords,2)
        x1 = coords(1,i); y1 = coords(2,i); x2 = coords(3,i); y2 = coords(4,i)
        distance = dbdistance( x1, y1, x2, y2, jsferic, jasfer3d, dmiss )

        !
        ! To avoid a false warning if the target value is zero, add 1.0 to the two arguments
        !
        call assert_comparable( distance+1.0, coords(5,i)+1.0, eps, "Distance between points on a sphere" )
    enddo
end subroutine test_dbdistance

end module test_geometry_module
