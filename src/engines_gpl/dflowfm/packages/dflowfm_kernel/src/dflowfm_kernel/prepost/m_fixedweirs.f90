!> Contains the global data for all fixed weirs.
!! fxw is the array of cross section paths.
module m_fixedweirs
    use m_crspath
    implicit none

    type (tcrspath), allocatable    :: fxw(:)

    integer                         :: nfxw = 0
    integer, allocatable            :: lnfxw(:)              ! Links with fixed weirs (dim=nfxw)
    integer, allocatable            :: nfxwL(:)              ! fixed weirs on links   (dim=Lnx)
    double precision, allocatable   :: csfxw(:)              ! fixed weir direction
    double precision, allocatable   :: snfxw(:)              ! fixed weir direction
    double precision, allocatable   :: crestlxw(:)           ! crest length of a weir
    double precision, allocatable   :: crestlevxw(:)         ! crest level of a weir
    double precision, allocatable   :: shlxw(:)              ! sill height left of a weir
    double precision, allocatable   :: shrxw(:)              ! sill height right of a weir
    double precision, allocatable   :: taludlxw(:)           ! talud left of a weir
    double precision, allocatable   :: taludrxw(:)           ! talud right of a weir
    double precision, allocatable   :: vegxw(:)              ! vegetation code on a weir
    double precision, allocatable   :: weirdte(:)            ! loss coeff
    integer         , allocatable   :: iweirtxw(:)           ! weir type

    double precision                :: sillheightmin    = 0.5d0 ! waqua dams with both sillheights > sillheightmin go to fixedweirs.pli
                                                                ! the rest goes to
contains



!> Deletes all fixed weirs from fxw.
!! Does not free up memory, use m_crspath::deallocCrossSectionPaths for that.
subroutine delFixedWeirs()
    nfxw = 0
    ! Do not reset fxw data, just let it be overwritten later.
end subroutine delFixedWeirs

   end module m_fixedweirs
