   ! Interpolate flownode-based vector (sx,sy) to edge-based vector (sxx, syy)
   subroutine fm_upwbed(lsedtot, sx, sy, sxtot, sytot, e_sn, e_st)
   use m_flowgeom
   use m_flow
   use unstruc_messages
   use m_sediment, only: stmpar, jabndtreatment  ! debug
   use sediment_basics_module
   use m_fm_erosed, only: link1, link1sign
   implicit none

   integer,                                  intent(in)  :: lsedtot        !< number of sediment fractions
   double precision, dimension(Ndx,lsedtot), intent(in)  :: sx, sy         !< cell (flownode)-based quantity
   double precision, dimension(Ndx,lsedtot), intent(in)  :: sxtot, sytot   !< cell (flownode)-based fluxes
   double precision, dimension(Lnx,lsedtot), intent(out) :: e_sn, e_st     !< edge (flowlink)-based quantity, normal and tangential components

   double precision                                      :: sutot1, sutot2

   integer                                               :: k1, k2, Lf, l, lnxlnxi
   logical                                               :: pure1d_mor
   logical                                               :: upwindbedload

   upwindbedload = stmpar%morpar%mornum%upwindbedload
   pure1d_mor = stmpar%morpar%mornum%pure1d
   !if ( laterallyaveragedbedload ) then
   !   call mess(LEVEL_ERROR, 'upwbed: laterally averaged bedload not supported')
   !end if

   if (jabndtreatment==0) then
      lnxlnxi = lnx
   else if (jabndtreatment==1) then
      lnxlnxi = lnxi
   end if

   ! internal flowlinks (and boundary flowlinks if jabndtreatment==0 -- default)
   do Lf=1,Lnxlnxi
      !     check if flowlink is active and if it connects two active sediment flownodes
      if ( hu(Lf)>epshu ) then
         !        find left and right neighboring flownodes
         k1 = ln(1,Lf)
         k2 = ln(2,Lf)

         do l=1,lsedtot
            if (stmpar%sedpar%sedtyp(l) == SEDTYP_COHESIVE) cycle   ! conform with d3d

            if (pure1d_mor .and. abs(kcu(Lf)) == 1) then
               ! on 1D links use the x-component which is the full vector
               if (link1(k1) == Lf) then
                   sutot1 =  sxtot(k1,l)
               else
                   sutot1 = link1sign(k1) * sxtot(k1,l)
               endif
               if (link1(k2) == Lf) then
                   sutot2 =  sxtot(k2,l)
               else
                   sutot2 = link1sign(k2) * sxtot(k2,l)
               endif

               if (upwindbedload) then
                   ! upwind approximation
                   if ( sutot1>0d0 .and. sutot2>0d0 ) then
                      e_sn(Lf,l) =  sx(k1,l)
                   else if ( sutot1<0d0 .and. sutot2<0d0 ) then
                      e_sn(Lf,l) =  sx(k2,l)
                   else
                      e_sn(Lf,l) =  0.5d0*(sx(k1,l)+sx(k2,l))
                   end if
               else
                   ! central approximation
                   e_sn(Lf,l) =  0.5d0*(sx(k1,l)+sx(k2,l))
               end if
               e_st(Lf,l) = 0d0
            else
               ! project the fluxes in flowlink direction
               sutot1 =  csu(Lf)*sxtot(k1,l) + snu(Lf)*sytot(k1,l)
               sutot2 =  csu(Lf)*sxtot(k2,l) + snu(Lf)*sytot(k2,l)

               if (upwindbedload) then
                   ! upwind approximation
                   if ( sutot1>0d0 .and. sutot2>0d0 ) then
                      e_sn(Lf,l) =  csu(Lf)*sx(k1,l) + snu(Lf)*sy(k1,l)
                   else if ( sutot1<0d0 .and. sutot2<0d0 ) then
                      e_sn(Lf,l) =  csu(Lf)*sx(k2,l) + snu(Lf)*sy(k2,l)
                   else
                      e_sn(Lf,l) =  csu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + snu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
                   end if
               else
                   ! central approximation
                   e_sn(Lf,l) =  csu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + snu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
               end if
               e_st(Lf,l) = -snu(Lf)*0.5d0*(sx(k1,l)+sx(k2,l)) + csu(Lf)*0.5d0*(sy(k1,l)+sy(k2,l))
            end if
         end do
      else
         do l=1,lsedtot
            e_sn(Lf,l) = 0d0
            e_st(Lf,l) = 0d0
         end do
      end if
   end do

   if (jabndtreatment==1) then
      ! boundary flowlinks processed separately
      do Lf=Lnxi+1,Lnx
         if ( hu(Lf)>epshu .and. u1(Lf)<=0d0) then
            ! find left and right neighboring flownodes
            k1 = ln(1,Lf)  ! boundary node
            k2 = ln(2,Lf)  ! internal node

            do l=1,lsedtot
               if (pure1d_mor .and. kcu(Lf) == -1) then
                   if (link1(k2) == Lf) then
                       e_sn(Lf,l) = sx(k2,l)
                   else
                       e_sn(Lf,l) = link1sign(k2) * sx(k2,l) ! TODO: check
                   endif
                   e_st(Lf,l) = 0d0
               else
                   e_sn(Lf,l) =  csu(Lf)*sx(k2,l) + snu(Lf)*sy(k2,l)
                   e_st(Lf,l) = -snu(Lf)*sx(k2,l) + csu(Lf)*sy(k2,l)
               end if
            end do
         end if
      end do
   end if
   end subroutine fm_upwbed
