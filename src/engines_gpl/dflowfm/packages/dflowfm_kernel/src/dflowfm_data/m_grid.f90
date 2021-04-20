module m_grid
  implicit none
  integer                       :: MET  = 1  ! drawing method
  integer                       :: mc   = 0  , nc   = 0  ! actuele dimensies
  integer                       :: MMAX = 0  , NMAX = 0  , MNMAX = 0 ! array   dimensies
  integer                       :: mch  = 0  , nch  = 0  ! actuele dimensies van hulprooster
  double precision, allocatable :: xc (:,:), yc (:,:), zc (:,:)
  double precision, allocatable :: xch(:,:), ych(:,:), zch(:,:)  ! save/restore
  integer, allocatable          :: ijc(:,:), ijyes(:,:)
end module m_grid
