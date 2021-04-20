!> Highlights the 'string' field of a user-input form field.
!! Input fields are highlighted automatically, but the string label isn't.
!! This assumes that string field number is always input field number minus 1.
!! Only use this subroutine as the FMUSER argument to IFormEditUser(.., .., FMUSER).
subroutine highlight_form_line(ifield, iexitk)
implicit none
integer, intent(in) :: ifield !< Form field number that lost focus (infoform(3) contains 'next' field).
integer, intent(in) :: iexitk !< 'Exit' key that was used to leave this form field.

integer :: ifieldnext
integer, external :: InfoForm

ifieldnext = InfoForm(3)

! Reset the 'current' field back to defaults (no highlights)
if (ifield > 1) then
    call iformattributen(ifield-1, 0, -1, -1)
    call iformshowfield(ifield-1)
end if
if (ifieldnext > 1) then
    call iformattribute(ifieldnext-1, 'UB', ' ', ' ')
    call iformshowfield(ifieldnext-1)
end if

end subroutine highlight_form_line
