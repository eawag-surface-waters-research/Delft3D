!> add tracer to constituents, or get constituents number if tracer already exists
subroutine add_tracer(tracer_name, iconst)
   use m_transport
   use unstruc_messages
   use m_meteo, only: numtracers, trnames
   implicit none

   character(len=*), intent(in)  :: tracer_name  !< tracer name, or '' for default name
   integer,          intent(out) :: iconst       !< constituent number

   character(len=8)              :: str

   integer                       :: ierror, i, itrac

   integer, external             :: findname

   ierror = 1

!  check if tracer already exists, based on tracer name
   iconst = 0
   if ( ITRA1.gt.0 .and. trim(tracer_name).ne.'') then
      iconst = findname(NUMCONST, const_names, tracer_name)

      if ( iconst.ge.ITRA1 .and. iconst.le.ITRAN ) then  ! existing tracer found
         call mess(LEVEL_INFO, 'add_tracer: tracer ' // trim(tracer_name) // ' already exists. No update required.')
         goto 1234
      end if
   end if

!  append tracer
   if ( ITRA1.eq.0) then
      NUMCONST = NUMCONST+1
      ITRA1    = NUMCONST
      ITRAN    = NUMCONST
   else
!     check if tracers are at the back
      if ( iTRAN.ne.NUMCONST ) then
         call mess(LEVEL_ERROR, 'add_tracer: tracer(s) not at the back of the constituents array')
         goto 1234
      end if

      NUMCONST = NUMCONST+1
      ITRAN    = NUMCONST
   end if

!  output tracer number
   iconst = ITRAN

!  reallocate arrays
   call alloc_transport(.true.)

!  set name
   if ( trim(tracer_name).ne.'' ) then
      const_names(ITRAN) = trim(tracer_name)
   else
      write(str,"(I0)") ITRAN-ITRA1+1
      const_names(ITRAN) = 'tracer_'//trim(str)
   end if

   if ( numtracers.gt.0 ) then   ! number of tracers with boundary conditions
!     generate tracer (boundary condition) to constituent
      itrac = findname(numtracers,trnames,trim(tracer_name))
      if ( itrac.gt.0 ) then
         itrac2const(itrac) = iconst
      end if
   end if

   ierror = 0

1234 continue

   return
end subroutine add_tracer
