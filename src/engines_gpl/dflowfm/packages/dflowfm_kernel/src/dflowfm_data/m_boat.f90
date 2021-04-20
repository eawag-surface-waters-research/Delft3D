module m_boat
  implicit none
  double precision, allocatable :: XBOAT (:), YBOAT(:), ZBOAT(:)  ! INLEZEN &
  double precision, allocatable :: XBOOT (:), YBOOT(:), ZBOOT(:)  ! AFBEELDEN LIJNEN
  integer, allocatable          :: NCBOAT(:)                      !
  integer                       :: MXBOAT, MAXBOAT, NCLBOAT
  double precision              :: BLEN = 25, BHEIGHT = 4, BWIDTH = 5, BHPMAX = 1000, BHPPERC = 0.5d0
  integer                       :: KKB(10) = 0
end module m_boat
