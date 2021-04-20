subroutine flgsareafm(formno, m, husb, velhght, zs, ds, dg, wstr)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    ! use cpluv
    use m_flow, only: au

    implicit none
!
! Global variables
!
    integer, intent(in)            :: formno
    integer, intent(in)            :: m
    double precision, intent(in)   :: dg
    double precision, intent(in)   :: ds
    double precision, intent(in)   :: husb
    double precision, intent(in)   :: velhght
    double precision, intent(in)   :: wstr
    double precision, intent(in)   :: zs
!
!
! Local variables
!
    double precision               :: hs1
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
    ! Programmer:         J.Kuipers
    !
    ! Module:             FLGSAREA (FLow General Structure
    !                               calculate AREA thru structure)
    !
    ! Module description: The area through the general structure will
    !                     be deermined.
    !
    !                     The stage of the flow was already determined.
    !
    !
    ! Parameters:
    ! NR NAME              IO DESCRIPTION
    !  7 dg                I  Gate opening height.
    !  6 ds                I  Water level immediately downstream the gate.
    !  1 formno            I  Flow condition of general structure:
    !                         0 : closed or dry
    !                         1 : free weir flow
    !                         2 : drowned weir flow
    !                         3 : free gate flow
    !                         4 : drowned gate flow
    !  3 husb              I  Upstream water level.
    !  2 m                 I  Grid index of structure
    !  4 velhght           I  Velocity height
    !  8 wstr              I  Width at centre of structure.
    !  5 zs                I  Bed level at centre of structure.
    !=======================================================================
    !     Include Pluvius data space
    !     Declaration of parameters:
    !     Declaration of local variables:
    !
    if (formno==0) then
       !        closed or dry
       !  au(m) = 0.0
       ! kfu(m) = 0
    else
       !
       !        Calculate upstream energy level w.r.t sill
       !
       hs1 = husb + velhght - zs
       ! kfu(m) = 1
       !
       if (formno==1) then
          !           free weir flow
          au(m) = wstr*hs1*2.0D0/3.0D0
       elseif (formno==2) then
          !           drowned weir flow
          au(m) = wstr*ds
       elseif (formno==3) then
          !           free gate flow
          au(m) = wstr*dg
       elseif (formno==4) then
          !           drowned gate flow
          au(m) = wstr*dg
       else
       endif
    endif
end subroutine flgsareafm
