function dpsequfm(dvar1, dvar2, eps) ! equal within eps?
implicit none
logical                        :: dpsequfm
double precision, intent(in)   :: dvar1, dvar2, eps
dpsequfm = abs(dvar1 - dvar2)<eps
end function dpsequfm
