 subroutine shipcoor(n,sx1,sy1,sx2,sy2)          ! get absolute shipcoordinates in sx2, sy2), input sx1, sy1 : ( 1, -1) = (bow  , portside )
 use m_ship                                      !                                                             (-1,  1) = (stern, starboard)
 implicit none
 double precision :: sx1,sx2,sy1,sy2,css,sns
 integer          :: n
 css = cos(shi(n))     ; sns = sin(shi(n))
 sx2 = shx(n) + sx1*shL(n)*css - sy1*shb(n)*sns  ! square ship
 sy2 = shy(n) + sx1*shL(n)*sns + sy1*shb(n)*css
 end subroutine shipcoor
