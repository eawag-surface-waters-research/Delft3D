      !> Draw a filled circle at current position.
      !! Filled means: one colour for inside, one colour for edge.
      subroutine HLCIR2(R, icolfill, icoledge)
      implicit none
        double precision, intent(in) :: R    !< Radius in world coords.
        integer,          intent(in) :: icolfill !< Colour number for inner fill
        integer,          intent(in) :: icoledge !< Colour number for edge

        CALL IGRFILLPATTERN(4,0,0)
        CALL SETCOL(icolfill)
        CALL CIR(R)
        CALL IGRFILLPATTERN(0,0,0)
        CALL SETCOL(icoledge)
        CALL CIR(R)
        CALL IGRFILLPATTERN(4,0,0)
      end subroutine HLCIR2
