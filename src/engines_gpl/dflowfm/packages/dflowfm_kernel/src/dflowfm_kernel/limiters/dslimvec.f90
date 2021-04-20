   !> limited higher-order correction of vector data
   subroutine dslimvec(ds1x, ds1y, ds2x, ds2y, csu, snu, limtyp, dsx, dsy)
      use m_flowparameters
      implicit none

      double precision, intent(in)  :: ds1x, ds1y   !< "voorslope" components
      double precision, intent(in)  :: ds2x, ds2y   !< "naslope" components
      double precision, intent(in)  :: csu, snu     !< orientation vector components
      integer,          intent(in)  :: limtyp       !< limiter type
      double precision, intent(out) :: dsx, dsy     !< correction components

      double precision              :: ds1n, ds1t   !< normal and tangential component, respectively
      double precision              :: ds2n, ds2t   !< normal and tangential component, respectively
      double precision              :: dsn, dst

      double precision, external    :: dslim

      if ( jalimnor.eq.1 ) then
         ds1n =  csu*ds1x + snu*ds1y
         ds1t = -snu*ds1x + csu*ds1y

         ds2n =  csu*ds2x + snu*ds2y
         ds2t = -snu*ds2x + csu*ds2y

         dsn = 0d0
         dst = 0d0

         if (abs(ds2n)  > eps10 .and. abs(ds1n) > eps10) then
             dsn = dslim(ds1n, ds2n, limtyp)
         endif

         if (abs(ds2y)  > eps10 .and. abs(ds1y) > eps10) then
             dst =  dslim(ds1t, ds2t, limtyp)
         endif

         dsx = csu*dsn - snu*dst
         dsy = snu*dsn + csu*dst

      else
         dsx = dslim(ds1x, ds2x, limtyp)
         dsy = dslim(ds1y, ds2y, limtyp)
      end if

      return
   end subroutine dslimvec
