   subroutine soulsby( tsig, uorbu, z00, fw, ustw2 )
      use m_sferic, only: pi

      implicit none
      double precision, intent(in ) :: tsig, uorbu, z00
      double precision, intent(out) :: fw, ustw2
      double precision              :: a
      a = uorbu * tsig /2d0/pi
      if( a > 0d0 ) then
         fw = min( 1.39d0 * (a/z00)**(-0.52d0), 0.3d0 )
      else
         fw = 0.3d0
      endif

      ustw2 = 0.5*fw*uorbu**2
   end subroutine soulsby
