!> add internal tides friction forces to adve
   subroutine add_InternalTidesFrictionForces()
      use m_flowgeom
      use m_flow
      use m_flowtimes
      use unstruc_messages
      use m_partitioninfo
      implicit none

      character(len=256)         :: str

      double precision           :: GradHinUc, dum, Lambda, dfac
      double precision           :: dumx1, dumy1, dumx2, dumy2
      double precision           :: diss

      integer                    :: k, k1, k2, L
      integer                    :: ierror

      double precision, external :: nod2linx, nod2liny

!     compute water depth gradient, based on cell-centered date
      hs = s1-bl
      call comp_gradC(hs, workx, worky)

      !call realloc(plotlin, Ndx, keepExisting=.false.)
      !plotlin = worky

!     compute cell-centered forces
      ierror = 0
      DissInternalTides = 0d0
      do k=1,Ndx
         dum = sqrt(workx(k)**2 + worky(k)**2)
         GradHinUc = workx(k)*ucx(k) + worky(k)*ucy(k)
         workx(k) =  -FrcInternalTides2D(k) * GradHinUc * workx(k)
         worky(k) =  -FrcInternalTides2D(k) * GradHinUc * worky(k)

         if ( hs(k).gt.epshs ) then

            if( ITcap.gt.0d0 ) then
!              limit with ITcap
               diss = -rho(k)*( workx(k) * ucx(k) + worky(k) * ucy(k) )

               if ( diss.gt.ITcap ) then
                  dfac = ITcap/diss

                  workx(k) = dfac * workx(k)
                  worky(k) = dfac * worky(k)
               end if
            end if

!           check time step
!           estimate eigenvalue
            Lambda = FrcInternalTides2D(k) * dum**2 / hs(k)
            if ( Lambda*dts.gt.1d0 ) then
               dfac = 1d0 / (Lambda*dts)
 !              write(str, "('k = ', I8, ': gamma ||grad H||^2 / H = ', E15.5, ' > 1/Delta t =', E15.5, ', H=', E15.5, ', ||grad H||=', E15.5, ', gamma=', E15.5, ', reduce factor=', E15.5)") k, Lambda, 1d0/dts, hs(k), dum, FrcInternalTides2D(k), dfac
 !              call mess(LEVEL_WARN, trim(str))
!               ierror = 1

               workx(k) = dfac * workx(k)
               worky(k) = dfac * worky(k)
            end if

            DissInternalTidesPerArea(k) = -rho(k)*( workx(k) * ucx(k) + worky(k) * ucy(k) )

!           add to total internal tides dissipation rate
            if ( jampi.eq.1 ) then
               if ( idomain(k).ne.my_rank ) cycle
            end if

            if ( k.le.Ndxi ) then   ! do not add fictitious boundary nodes
               DissInternalTides = DissInternalTides + DissInternalTidesPerArea(k) * ba(k)
            end if
         end if
      end do
      if ( ierror.eq.1 ) then
         call mess(LEVEL_ERROR, 'add_InternalTidesFrictionForces: time step too large')
      end if

!     interpolate to faces, project in face-normal direction, divide by water depth and add to adve
      do L=1,Lnx
         if ( hu(L).gt.0d0 ) then
            k1 = ln(1,L)
            k2 = ln(2,L)

!            adve(L) = adve(L) - huvli(L) * (  &
!              (acL(L)*workx(k1) + (1d0-acL(L))*workx(k2)) * csu(L) +   &
!              (acL(L)*worky(k1) + (1d0-acL(L))*worky(k2)) * snu(L) )

            dumx1 = nod2linx(L,1,workx(k1),worky(k1))
            dumy1 = nod2liny(L,1,workx(k1),worky(k1))

            dumx2 = nod2linx(L,2,workx(k2),worky(k2))
            dumy2 = nod2liny(L,2,workx(k2),worky(k2))

            adve(L) = adve(L) - huvli(L) * (  &
              (acL(L)*dumx1 + (1d0-acL(L))*dumx2) * csu(L) +   &
              (acL(L)*dumy1 + (1d0-acL(L))*dumy2) * snu(L) )

         end if
      end do

      return
   end subroutine add_InternalTidesFrictionForces
