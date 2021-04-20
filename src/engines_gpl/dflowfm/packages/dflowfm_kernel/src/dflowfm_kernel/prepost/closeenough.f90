  subroutine closeenough ( x1,y1,x2,y2,r,ja)

  use m_missing, only: dmiss
  use m_sferic, only: jsferic, jasfer3D
  use geometry_module, only: dbdistance

  implicit none

  double precision :: x1,y1,x2,y2, r2, r
  integer          :: ja

  ja  = 0
  r2  = dbdistance(x1,y1,x2,y2, jsferic, jasfer3D, dmiss)
  if (r2 < r) then
     ja = 1
  endif
  end subroutine
