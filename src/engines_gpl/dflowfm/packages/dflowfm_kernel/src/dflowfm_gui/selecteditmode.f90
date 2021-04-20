    !> Selects the edit mode for a given keypress code.
    !! Alt-P/-N/-S/-G/-B/-F for the respective modes.
    subroutine selecteditmode(newmode, key)
    implicit none
    integer, intent(inout) :: newmode !< New mode (0 for invalid key presses).
    integer, intent(in)    :: key     !< Key press code

    if      (key == 512+80) then ! Alt+P: Edit Polygon
        newmode = 1
    else if (key == 512+78) then ! Alt+N: Edit Network
        newmode = 2
    else if (key == 512+83) then ! Alt+S: Edit Splines
        newmode = 3
    else if (key == 512+71) then ! Alt+G: Edit Grid
        newmode = 4
    else if (key == 512+66) then ! Alt+B: Edit Samples (bathymetry)
        newmode = 5
    else if (key == 512+70) then ! Alt+F: Edit Flow
        newmode = 6
    end if
    return
    end subroutine selecteditmode
