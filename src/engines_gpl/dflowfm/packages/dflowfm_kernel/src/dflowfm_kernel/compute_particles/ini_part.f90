!> initialize particles
subroutine ini_part(japartfile, partfile, partrelfile, jatracer_loc, starttime_loc, timestep_loc, threeDtype_loc)
   use m_particles
   use m_samples
   use m_flow, only: s1, kmx
   use m_transport, only: constituents, numconst
   use m_flowtimes, only: tstart_user
   use m_missing
   use m_partitioninfo
   use unstruc_messages
   implicit none

   integer,            intent(in) :: japartfile    !< use particle file (1) or not (0)
   character(len=255), intent(in) :: partfile      !< initial particle file
   character(len=255), intent(in) :: partrelfile   !< particle release file
   integer,            intent(in) :: jatracer_loc  !< add tracer (1) or not (0)
   double precision,   intent(in) :: starttime_loc !< start time (>0) or not (0)
   double precision,   intent(in) :: timestep_loc  !< time step (>0) or every computational time step (0)
   integer,            intent(in) :: threeDtype_loc    !< depth averaged (0) or free surface (1)

   integer, dimension(1) :: idum

   integer             :: minp
   logical             :: lexist
   integer             :: iconst
   integer             :: ierror
   integer             :: i

!   if ( jampi.eq.0 ) then
!      call newfil(mfile,'part.xyz')
!   else
!      call newfil(mfile,'part_'//sdmn//'.xyz')
!   end if

!  deallocate
   call dealloc_partmesh()
   call dealloc_partfluxes()
   call dealloc_partrecons()
   call dealloc_particles()
   call dealloc_auxfluxes()
   call dealloc_partparallel()

   Nrpart = 0
   irpart = 0
   Nglob = 0
   NpartOut = 0

   timenext = 0d0
   timelast = DMISS

   jatracer = 0

!  add particle tracer (when tracers are initialized)
   if ( jatracer_loc.eq.1 ) then
      jatracer = 1
   end if

!  start time
   if ( starttime_loc.gt.0d0 ) then
      starttime = starttime_loc
      timenext = starttime
   end if

!  time step
   if ( timestep_loc.gt.0d0 ) then
      timestep = timestep_loc
   end if

!  3D type
   if ( kmx.gt.0 ) then
      threeDtype = threeDtype_loc
   else  ! 2D
      threeDtype = 0
   end if

   if ( japartfile.eq.1 ) then
      japart = 0
      if ( len_trim(partfile).gt.0 ) then
   !     read initial samples from inputfile
         inquire(FILE = trim(partfile), exist = lexist)
         if ( lexist ) then
            call oldfil(minp, partfile)
            call savesam()
            call reasam(minp, 0)
            japart = 1
         else
            call mess(LEVEL_ERROR, 'the specified initial particle locations file could not be found: ', trim(partfile))
         end if
      end if
      if ( len_trim(partrelfile).gt.0 ) then
   !     read initial samples from inputfile
         inquire(FILE = trim(partrelfile), exist = lexist)
         if ( lexist ) then
            call read_particles_release_file(partrelfile)
            japart = 1
         else
            call mess(LEVEL_ERROR, 'the specified particle release file could not be found: ', trim(partfile))
         end if
      end if
   else  ! initialize only
      japart = 1
   end if

   if ( japart.eq.1 ) then
!     set pointers with mesh connectivity etc.
      call part_setmesh()

!     set flux coeffecients
      call comp_fluxcoeffs()

      call realloc_partrecons()

      call reconst_vel_coeffs()

      if ( Ns.gt.0 ) then
         call add_particles(Ns, xs, ys, 0, 1)
         timepart = tstart_user
         NpartOut = NpartOut + Ns

         call delsam(0)
      else
         Npart = 0
      end if

      if ( jatracer.eq.1 ) then
!        REMARK: tracer with particle concentration is ALSO updated by transport module (not necessary)
         call add_tracer(PART_TRACERNAME, part_iconst)
!        compute concentration (overwrite update by transport module)
         call comp_concentration(s1,numconst,part_iconst,constituents)
      end if

      call alloc_auxfluxes()

      call ini_partparallel()
   end if

   return
end subroutine ini_part
