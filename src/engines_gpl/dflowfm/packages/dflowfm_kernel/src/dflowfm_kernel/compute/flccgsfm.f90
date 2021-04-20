subroutine flccgsfm(dg, dsc, cgd, cgf, cw, mugf, cgda, cgfa, mugfa)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    double precision, intent(in)   :: cgd
    double precision, intent(out)  :: cgda
    double precision, intent(in)   :: cgf
    double precision, intent(out)  :: cgfa
    double precision, intent(in)   :: cw
    double precision :: dg
    double precision :: dsc
    double precision, intent(in)   :: mugf
    double precision, intent(out)  :: mugfa
!
!
! Local variables
!
    logical                        :: dpsequfm
!
!
!! executable statements -------------------------------------------------------
!
    !
    !=======================================================================
    !                      Deltares
    !                One-Two Dimensional Modelling System
    !                           S O B E K
    !
    ! Subsystem:          Flow Module
    !
    ! Programmer:         J.Brouwer/J.Kuipers
    !
    ! Module:             FLCCGS (FLow Corr. Coefficients for General Structure)
    !
    ! Module description: Correct coefficients for gate flow
    !
    !                     In the formulas for the gate and weir several
    !                     coefficients are applied. To avoid discontinuities
    !                     in the transition from weir to gate flow, the
    !                     correction coefficient cgd should be corrected.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  3 cgd               I  Correction coefficient for drowned gate flow.
    !  7 cgda              O  Adapted correction coefficient for drowned
    !                         gate flow.
    !  4 cgf               I  Correction coefficient for free gate flow.
    !  8 cgfa              O  Adapted correction coefficient for free gate
    !                         flow.
    !  5 cw                I  Correction coefficient for weir flow.
    !  1 dg                I  Gate opening height.
    !  2 dsc               I  Depth at sill or critical depth.
    !  6 mugf              I  Contraction coefficient for free gate flow.
    !  9 mugfa             O  Adapted contraction coefficient for free gate
    !                         flow.
    !=======================================================================
    !
    !     Declaration of parameters:
    !
    !
    !     Logical function
    !
    !
    !     dsc contains ds or dc
    !
    if (.not.dpsequfm(dsc, 0.0D0, 1.D-20)) then
       !
       if (dg/dsc>mugf) then
          mugfa = dg/dsc
       else
          mugfa = mugf
       endif
       !
       if (cgd>cw) then
          if (dpsequfm(dg, 0.0D0, 1.0D-20)) then
             cgda = cgd
          else
             cgda = min(dsc/dg*cw, cgd)
          endif
       else
          cgda = max(dg/dsc*cw, cgd)
       endif
       !
       if (cgf>cw) then
          if (dpsequfm(dg, 0.0D0, 1.0D-20)) then
             cgfa = cgf
          else
             cgfa = min(dsc/dg*cw, cgf)
          endif
       else
          cgfa = max(dg/dsc*cw, cgf)
       endif
    !
    else
       mugfa = mugf
       cgda = cgd
       cgfa = cgf
    endif
end subroutine flccgsfm
