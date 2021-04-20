module m_statistics
implicit none
 double precision                  :: avedif     !< for now only, cum dif with analytic sol
 double precision                  :: sqadif     !< for now only, cum dif with analytic sol
 double precision                  :: rmsdif     !< for now only, cum dif with analytic sol
 double precision                  :: dmxdif     !< for now only, cum dif with analytic sol
 integer                           :: numdif

 double precision                  :: cumavedif  !< for now only, cum dif with analytic sol
 double precision                  :: cumrmsdif  !< for now only, cum dif with analytic sol
 double precision                  :: cumdmxdif  !< for now only, cum dif with analytic sol
 integer                           :: numcum, npdf
 double precision, allocatable     :: xpdf(:), ypdf(:)
contains
subroutine reset_statistics()
    avedif    = 0d0    ! for now only, cum dif with analytic sol
    sqadif    = 0d0
    rmsdif    = 0d0    ! for now only, cum dif with analytic sol
    dmxdif    = 0d0    ! for now only, cum dif with analytic sol
    numdif    = 0


    cumavedif = 0d0    ! for now only, cum dif with analytic sol
    cumrmsdif = 0d0    ! for now only, cum dif with analytic sol
    cumdmxdif = 0d0    ! for now only, cum dif with analytic sol
    numcum    = 0
    npdf      = 0
end subroutine reset_statistics
end module m_statistics
