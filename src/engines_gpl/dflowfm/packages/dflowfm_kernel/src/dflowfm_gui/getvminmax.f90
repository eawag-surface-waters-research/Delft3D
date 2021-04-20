  subroutine getvminmax(num,vmin,vmax,v, n)
  use unstruc_display
  use m_missing
  implicit none
  integer             :: n
  integer, intent(in) :: num
  double precision    :: vmin, vmax, v(n)

  if (profmin(num) == dmiss) then
      vmin = 1d9
      vmin = min(vmin, minval(v(1:n)) )
  else
      vmin = profmin(num)
  endif

  if (profmax(num) == dmiss) then
      vmax = -1d9
      vmax = max(vmax, maxval(v(1:n)), vmin+1d-5 )
  else
      vmax = profmax(num)
  endif
  end subroutine getvminmax
