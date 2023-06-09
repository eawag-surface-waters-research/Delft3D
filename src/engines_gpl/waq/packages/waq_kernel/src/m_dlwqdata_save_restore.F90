!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

!     Restore the local persistent variables from the derived type
!
module m_dlwqdata_save_restore

    use delwaq2_data
    use dlwq_data
    use waqmem
    use m_sysi
    use m_sysn
    
    implicit none
    
    !type(delwaq_data), target :: dlwqd                !< derived type for persistent storage
    integer   :: itime
    integer   :: ifflag
    integer   :: iaflag
    integer   :: ibflag
    integer   :: nddim
    integer   :: nvdim
    integer   :: nosss
    integer   :: noqtt
    integer   :: noqt
    integer   :: nopred
    integer   :: ithandl
    logical   :: litrep
    logical   :: ldummy
    integer   :: inwtyp
    integer   :: nowarn
    integer   :: ioptzb
    logical   :: forester
    logical   :: updatr
    integer   :: lleng
    real(kind=kind(1.0d0))      :: tol
    logical   :: lstrec
    integer   :: itimel
          
    contains
  
    subroutine dlwqdata_save(dlwqd)
    
        type(delwaq_data), target, intent(inout) :: dlwqd                !< derived type for persistent storage

        DLWQD%II        =  II
        DLWQD%IN        =  IN
        DLWQD%ITIME     =  ITIME
        DLWQD%IFFLAG    =  IFFLAG
        DLWQD%IAFLAG    =  IAFLAG
        DLWQD%IBFLAG    =  IBFLAG
        DLWQD%NDDIM     =  NDDIM
        DLWQD%NVDIM     =  NVDIM
        DLWQD%NOSSS     =  NOSSS
        DLWQD%NOQTT     =  NOQTT
        DLWQD%NOQT      =  NOQT
        DLWQD%NOPRED    =  NOPRED
        DLWQD%ITIMEL    =  ITIMEL
        DLWQD%LSTREC    =  LSTREC
        DLWQD%ITHANDL   =  ITHANDL
        DLWQD%LITREP    =  LITREP
        DLWQD%LDUMMY    =  LDUMMY
        DLWQD%INWTYP    =  INWTYP

        DLWQD%OTIME     =  OTIME
        DLWQD%DELTIM    =  DELTIM
        DLWQD%TSCALE    =  TSCALE

        DLWQD%NOWARN    =  NOWARN
        DLWQD%IOPTZB    =  IOPTZB
        DLWQD%FORESTER  =  FORESTER
        DLWQD%UPDATR    =  UPDATR

        DLWQD%LLENG     =  LLENG

        DLWQD%TOL       =  TOL

        DLWQD%IWSTKIND  => IWSTKIND
        DLWQD%IEXSEG    => IEXSEG
        DLWQD%IKNMKV    => IKNMKV

        !     DLWQD%GRIDPS    - no need!

        call copy_time_data( dlwqd, .true. )

    end subroutine dlwqdata_save


    subroutine dlwqdata_restore(dlwqd)
    
        type(delwaq_data), target, intent(inout) :: dlwqd                !< derived type for persistent storage
        
        IN        =  DLWQD%IN
        II        =  DLWQD%II
        ITIME     =  DLWQD%ITIME
        IFFLAG    =  DLWQD%IFFLAG
        IAFLAG    =  DLWQD%IAFLAG
        IBFLAG    =  DLWQD%IBFLAG
        NDDIM     =  DLWQD%NDDIM
        NVDIM     =  DLWQD%NVDIM
        NOSSS     =  DLWQD%NOSSS
        NOQTT     =  DLWQD%NOQTT
        NOQT      =  DLWQD%NOQT
        NOPRED    =  DLWQD%NOPRED
        ITIMEL    =  DLWQD%ITIMEL
        ITHANDL   =  DLWQD%ITHANDL
        LITREP    =  DLWQD%LITREP
        LSTREC    =  DLWQD%LSTREC
        LDUMMY    =  DLWQD%LDUMMY
        INWTYP    =  DLWQD%INWTYP

        OTIME     =  DLWQD%OTIME
        DELTIM    =  DLWQD%DELTIM
        TSCALE    =  DLWQD%TSCALE

        NOWARN    =  DLWQD%NOWARN
        IOPTZB    =  DLWQD%IOPTZB
        FORESTER  =  DLWQD%FORESTER
        LLENG     =  DLWQD%LLENG
        UPDATR    =  DLWQD%UPDATR

        TOL       =  DLWQD%TOL

        IWSTKIND  => DLWQD%IWSTKIND
        IEXSEG    => DLWQD%IEXSEG
        IKNMKV    => DLWQD%IKNMKV

        call copy_time_data( dlwqd, .false. )

    end subroutine dlwqdata_restore

end module m_dlwqdata_save_restore