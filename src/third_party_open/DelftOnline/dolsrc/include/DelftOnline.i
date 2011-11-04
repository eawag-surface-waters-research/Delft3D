!--------------------------------------------------------------------------------
!   DelftOnline -- Constant Definitions for Fortran
!
!   Irv.Elshoff@wldelft.nl
!   4 apr 07
!
!   Copyright (C) 2006-2007, WL | Delft Hydraulics
!--------------------------------------------------------------------------------


!-----  DataElement BaseType

integer, parameter ::   DOL_OPAQUE          = 0
integer, parameter ::   DOL_INTEGER         = 1
integer, parameter ::   DOL_REAL            = 2
integer, parameter ::   DOL_DOUBLE          = 3
integer, parameter ::   DOL_DOUBLECOMPLEX   = 4
integer, parameter ::   DOL_COMPLEX         = 5
integer, parameter ::   DOL_LOGICAL         = 6
integer, parameter ::   DOL_CHARACTER       = 7

!-----  Called Function Language

integer, parameter ::   DOL_C               = 1
integer, parameter ::   DOL_FORTRAN         = 2

!-----  Read/Write Access

integer, parameter ::   DOL_IN              = 1
integer, parameter ::   DOL_OUT             = 2
integer, parameter ::   DOL_INOUT           = 3

