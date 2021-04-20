!> grow ijc with blocksize to satisfy objective lower- and upperbound
subroutine grow_ijc(lowold, uppold, lowobj, uppobj, init)

 use m_alloc
 use m_grid
 use m_missing

 implicit none

 integer, dimension(2), intent(inout) :: lowold, uppold    !< current array sizes
 integer, dimension(2), intent(in)    :: lowobj, uppobj    !< objective array sizes

 integer                              :: init              !< init=1: set blocksizes to initial values

 integer, dimension(2)                :: lownew, uppnew
 integer                              :: i
! integer, parameter                   :: IMISS = -999999
 integer, parameter                   :: IJCBLOCK = 100    ! block size in ijc

 logical                              :: ldoit

 integer, dimension(2), save          :: blocklow          ! lower blocksizes in ijc
 integer, dimension(2), save          :: blockupp          ! upper blocksizes in ijc
 double precision, parameter          :: FAC = 1.2         ! growfactor of blocksizes

 if ( init.eq. 1 ) then
    blocklow = (/ 1, 1 /)
    blockupp = (/ 1, 1 /)
 end if

 lownew = lowold
 uppnew = uppold

 if ( (lownew(1) .gt. lowobj(1)) .or. (uppnew(1) .lt. uppobj(1)) .or. &
      (lownew(2) .gt. lowobj(2)) .or. (uppnew(2) .lt. uppobj(2)) ) then

    do i=1,2
       do while ( lownew(i) .gt. lowobj(i) )
          lownew(i) = lownew(i) - blocklow(i)
       end do

       do while ( uppnew(i) .lt. uppobj(i) )
          uppnew(i) = uppnew(i) + blockupp(i)
       end do
    end do

    do i=1,2
       if ( lownew(i).ne.lowold(i) )   &
          blocklow(i) = ceiling( dble(blocklow(i)) * FAC )
       if ( uppnew(i).ne.uppold(i) )   &
          blockupp(i) = ceiling( dble(blockupp(i)) * FAC )
    end do

    call realloc(ijc, uppnew, lownew, fill=IMISS)

    lowold = lownew
    uppold = uppnew
 end if

end subroutine grow_ijc
