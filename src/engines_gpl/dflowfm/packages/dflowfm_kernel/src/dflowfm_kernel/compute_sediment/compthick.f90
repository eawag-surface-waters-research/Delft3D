   subroutine compthick()
   !!--description-----------------------------------------------------------------
   !
   ! Function:   Compute new thickness of transport and exchange layer
   !
   !!--declarations----------------------------------------------------------------
   use precision
   use bedcomposition_module
   use m_flow, only: hs
   use m_flowgeom, only: ndx
   use unstruc_files, only: mdia
   use message_module
   use m_sediment, only: stmpar
   use m_bedform
   !
   implicit none
   !
   ! The following list of pointer parameters is used to point inside the gdp structure
   !
   integer                             , pointer :: ttlform
   integer                             , pointer :: telform
   real(fp)                            , pointer :: ttlalpha
   real(fp)                            , pointer :: ttlmin
   real(fp), dimension(:)              , pointer :: duneheight
   !
   ! Local variables
   !
   integer                                       :: istat
   integer                                       :: nm
   integer                             , pointer :: iunderlyr
   real(fp)         , dimension(:)     , pointer :: thtrlyr
   character(256)                                :: errorstr
   logical                                       :: error
   !
   !! executable statements -------------------------------------------------------
   !
   ttlalpha    => stmpar%morpar%ttlalpha
   ttlmin      => stmpar%morpar%ttlmin
   ttlform     => stmpar%morpar%ttlform
   telform     => stmpar%morpar%telform
   duneheight  => bfmpar%duneheight
   !
   istat = bedcomp_getpointer_integer(stmpar%morlyr,'IUnderLyr',iunderlyr)
   if (istat/=0) then
      errorstr = 'Memory problem in COMPTHICK'
      call write_error(errorstr, unit=mdia)
      error = .true.
      return
   endif
   !
   select case(iunderlyr)
   case(2)
      !
      istat = bedcomp_getpointer_realfp (stmpar%morlyr,'ThTrLyr'  ,thtrlyr  )
      if (istat/=0) then
         errorstr = 'Memory problem in COMPTHICK'
         call write_error(errorstr, unit=mdia)
         error = .true.
         return
      endif
      !
      ! Determine new transport layer thickness
      !
      select case(ttlform)
      case(2)
         !
         ! proportional to water depth
         !
         do nm = 1, ndx
            thtrlyr(nm) = max(ttlalpha*hs(nm),ttlmin)
         enddo
      case(3)
         !
         ! proportional to dune height
         !
         do nm = 1, ndx
            thtrlyr(nm) = max(ttlalpha*duneheight(nm),ttlmin)
         enddo
         case default
         !
         ! nothing to do: constant in time
         !
      endselect
      !
      ! Determine new exchange layer thickness
      !
      select case(telform)
      case(1)
         case default
      endselect
      case default
      !
      ! No active layers: nothing to do
      !
   endselect
   end subroutine compthick
