   !> apply sediment boundary conditions
   subroutine apply_sediment_bc()
   use m_flowgeom
   use m_flow, only: q1
   use m_meteo
   use m_transport, only: ised1, numconst, constituents, ifrac2const
   use m_sediment, only: sedtot2sedsus
   use sediment_basics_module
   use m_fm_erosed
   implicit none

   integer :: j, kb, ki, L, ll, iconst, k, kk, Lb, Lt, LLL

   ! New approach: default Neumann, unless time series available
   !
      ! Find sand fractions
      do ll=1,lsed    ! sediment-fraction index
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(ll))==SEDTYP_NONCOHESIVE_SUSPENDED) then
            j=ll+ISED1-1 ! constituent index
            do LLL=Lnxi+1,Lnx
               call getLbotLtop(LLL,Lb,Lt)
               if (Lt<Lb) cycle
               do L=Lb,Lt
                  kb = ln(1,L); ki = ln(2,L)
                  constituents(j,kb) = constituents(j,ki)
               end do
            end do
         end if
      end do
   !
      ! From time series bnd, or 0d0
      do ll = 1, numfracs
         iconst = ifrac2const(ll)
         if (iconst==0) cycle
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(iconst-ISED1+1))==SEDTYP_NONCOHESIVE_SUSPENDED) then
            do k=1,nbndsf(ll)
               LLL = bndsf(ll)%k(3,k)
               call getLbotLtop(LLL,Lb,Lt)
               if (Lt<Lb) cycle
               if ( hu(LLL)>0d0 ) then
                  do L=Lb,Lt
                     kb = ln(1,L); ki = ln(2,L)
                     kk = kmxd*(k-1)+L-Lb+1
                     if ( q1(L)>0 ) then  ! inflow
                        constituents(iconst,kb) = bndsf(ll)%z(kk)
                     else                    ! outflow
                        constituents(iconst,kb) = constituents(iconst,ki)
                     end if
                  end do
               else
                  !                 set other values (e.g. dry links)
                  do L=Lb,Lb+kmxL(LLL)-1
                     kb = ln(1,L)
                     constituents(iconst,kb) = 0d0
                  end do
               end if
            end do
         end if
      end do
   !

   !
      ! Find mud fractions
      do ll=1,lsed    ! sediment-fraction index
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(ll))==SEDTYP_COHESIVE) then
            j=ll+ISED1-1 ! constituent index
            do LLL=Lnxi+1,Lnx
               call getLbotLtop(LLL,Lb,Lt)
               if (Lt<Lb) cycle
               do L=Lb,Lt
                  kb = ln(1,L); ki = ln(2,L)
                  constituents(j,kb) = constituents(j,ki)
               end do
            end do
         end if
      end do
   !
      ! From time series bnd, or 0d0
      do ll = 1, numfracs
         iconst = ifrac2const(ll)   ! allow for combo equilibrium/dirichlet bc concentrations
         if (iconst==0) cycle
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(iconst-ISED1+1))==SEDTYP_COHESIVE) then
            do k=1,nbndsf(ll)
               LLL = bndsf(ll)%k(3,k)
               call getLbotLtop(LLL,Lb,Lt)
               if (Lt<Lb) cycle
               do L=Lb,Lt
                  kb = ln(1,L); ki = ln(2,L)
                  kk =  kmxd*(k-1)+L-Lb+1
                  if ( q1(L)>0 ) then     ! inflow
                     constituents(iconst,kb) = bndsf(ll)%z(k)
                  else                    ! outflow
                     constituents(iconst,kb) = constituents(iconst,ki)
                  end if
               end do
            end do
         end if
      end do
   !
   end subroutine apply_sediment_bc
