!!  Copyright (C)  Stichting Deltares, 2012-2022.
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

module test_precision_basics
    use precision_basics
    use ftnunit

    implicit none

    private
    public :: tests_precision_basics

contains
subroutine tests_precision_basics
    call test( test_precision_sp, 'Compare single-precision reals' )
    call test( test_precision_dp, 'Compare double-precision reals' )
    call test( test_precision_nan_single, 'Compare single-precision NaN' )
    call test( test_precision_nan_double, 'Compare double-precision NaN' )
    call test( test_precision_inf_single, 'Compare single-precision Inf' )
    call test( test_precision_inf_double, 'Compare double-precision Inf' )
end subroutine tests_precision_basics

subroutine test_precision_sp
    integer, parameter :: wp = kind(1.0)

include "test_precision_body.f90"
end subroutine test_precision_sp

subroutine test_precision_dp
    integer, parameter :: wp = kind(1.0d0)

include "test_precision_body.f90"
end subroutine test_precision_dp

subroutine test_precision_nan_single
    use, intrinsic :: ieee_arithmetic

    real(kind=sp) :: val1, val2
    integer :: r

    val1 = ieee_value(val1, ieee_quiet_nan)
    val2 = ieee_value(val2, ieee_quiet_nan)
    r = comparereal(val1, val2)
    call assert_equal(r, 0, "compare 2 nans")

end subroutine test_precision_nan_single

subroutine test_precision_nan_double
    use, intrinsic :: ieee_arithmetic

    real(kind=hp) :: val1, val2
    integer :: r

    val1 = ieee_value(val1, ieee_quiet_nan)
    val2 = ieee_value(val2, ieee_quiet_nan)
    r = comparereal(val1, val2)
    call assert_equal(r, 0, "compare 2 nans")

end subroutine test_precision_nan_double

subroutine test_precision_inf_single
    use, intrinsic :: ieee_arithmetic

    real(kind=sp) :: val1, val2
    integer :: r

    val1 = ieee_value(val1, ieee_positive_inf)
    val2 = ieee_value(val2, ieee_negative_inf)
    r = comparereal(val1, val2)
    call assert_equal(r, 1, "compare +/- inf")

    val1 = ieee_value(val1, ieee_positive_inf)
    val2 = ieee_value(val2, ieee_positive_inf)
    r = comparereal(val1, val2)
    call assert_equal(r, 0, "compare +/- inf")

end subroutine test_precision_inf_single

subroutine test_precision_inf_double
    use, intrinsic :: ieee_arithmetic

    real(kind=hp) :: val1, val2
    integer :: r

    val1 = ieee_value(val1, ieee_positive_inf)
    val2 = ieee_value(val2, ieee_negative_inf)
    r = comparereal(val1, val2)
    call assert_equal(r, 1, "compare +/- inf")

    val1 = ieee_value(val1, ieee_positive_inf)
    val2 = ieee_value(val2, ieee_positive_inf)
    r = comparereal(val1, val2)
    call assert_equal(r, 0, "compare +/- inf")

end subroutine test_precision_inf_double

end module test_precision_basics
