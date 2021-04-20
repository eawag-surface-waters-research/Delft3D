module unstruc_channel_flow
use m_network
implicit none
type(t_network), target              :: network
integer                       :: CSCalculationOption  !< Calculation option for total area computation in 1d

logical,                public :: useVolumeTables                    !< Indicates whether 1d volume tables are useds
double precision,       public :: tableIncrement                     !< Increment for volume tables
logical,                public :: useVolumeTableFile                 !< Write the volume tables to file (or not)
character(len=charln),  public :: volumeTableFile                    !< Name of the table input file

contains


!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_*() instead.
subroutine default_channel_flow()
    CSCalculationOption = CS_TYPE_PREISMAN     !< calculation option for total area computation in 1d
    useVolumeTables     = .false.
    useVolumeTableFile  = .false.
    tableIncrement      = 0.1d0
!call dealloc(network)
end subroutine default_channel_flow

end module unstruc_channel_flow
