      !> Draw a highlighted circle at current position.
      !! Highlighted means: blank center, coloured outline.
      subroutine HLCIR(R, icol)
      implicit none
        double precision, intent(in) :: R    !< Radius in world coords.
        integer,          intent(in) :: icol !< Colour number

        call HLCIR2(R, 0, icol)
      end subroutine HLCIR
