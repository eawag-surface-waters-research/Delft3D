!----------------------------------------------------------------------
! subroutines from either net.F90 or rest.F90 that are still needed
!   without the GUI
!----------------------------------------------------------------------
!> Shows a message in a GUI dialog (Interacter only).
!! This routine is supposed to be called from the utility modules,
!! such as gridgeom, as a callback.
!!
!! NOTE: this subroutine is dflowfm's implementation of the MHCallBack::messagebox_iface interface.
subroutine unstruc_guimessage(title, msg, level)
    use unstruc_messages
    implicit none
    character(len=*)    :: title !< Title string
    character(len=*)    :: msg   !< Message string
    integer, intent(in) :: level !< Severity level, use values from the MessageHandling module (e.g., LEVEL_ERROR). Currently not used.

    call qnerror(msg, ' ', ' ')

end subroutine unstruc_guimessage
