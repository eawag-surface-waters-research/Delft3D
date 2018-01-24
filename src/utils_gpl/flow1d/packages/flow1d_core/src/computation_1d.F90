module m_computation_1d
!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2018.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
!  $Id$
!  $HeadURL$
!-------------------------------------------------------------------------------
   
   implicit none
   
   private
   
   public cu0ru0_1d
   public curu_1d
   
   contains
   
   subroutine cu0ru0_1d(network, l, s1m1, s1m2, dpu, u1, bu0, cu0, ru0) 
      use m_network
      use m_pump
      
      type(t_network), intent(in)    :: network
      integer, intent(in) :: l 
      double precision, intent(in)   :: s1m1
      double precision, intent(in)   :: s1m2
      double precision, intent(in)   :: dpu
      double precision, intent(in)   :: u1
      double precision, intent(out)  :: bu0
      double precision, intent(out)  :: cu0
      double precision, intent(out)  :: ru0
      
      type(t_administration_1d)      :: adm
      type(t_structure), pointer     :: pstru
      logical, external :: is_1D_inflowline
 
      adm = network%adm
      
      if (adm%hasStructures) then
         select case (network%sts%struct(adm%lin2str(l))%st_type)
         case (ST_WEIR)
            ! CU and RU for structures are computed in SUBSTITUTE_U
         case (ST_PUMP)
            pstru => network%sts%struct(adm%lin2str(l))
            call PrepareComputePump(pstru%pump, s1m1, s1m2)
         case (ST_ORIFICE)
            ! CU and RU for structures are computed in SUBSTITUTE_U
         case (ST_CULVERT)
         end select
      elseif (.not. is_1D_inflowline(l)) then
         call u1d(network, l, s1m1, s1m2, dpu, u1, bu0, cu0, ru0)
      endif
   end subroutine cu0ru0_1d

   subroutine curu_1d(network, l, s1m1, s1m2, u1, u0, q, dpu, bu0, cu0, ru0, dt, kf, cu, ru) 

      use m_network
      use m_Weir
      use m_CrossSections
      use m_Culvert
      use m_Pump
      use m_Orifice
      
      type(t_network), intent(inout)    :: network
      integer, intent(in)               :: l
      double precision, intent(in)      :: s1m1
      double precision, intent(in)      :: s1m2
      !double precision, intent(in)     :: dpu
      double precision, intent(inout)   :: u0
      double precision, intent(inout)   :: u1
      double precision, intent(inout)   :: q
      double precision, intent(in)   :: dt
      integer, intent(inout)         :: kf
      double precision, intent(in)   :: dpu
      double precision, intent(in)   :: bu0
      double precision, intent(in)   :: cu0
      double precision, intent(in)   :: ru0
      double precision, intent(out)  :: cu
      double precision, intent(out)  :: ru
     
      type(t_administration_1d)  :: adm
      type(t_structure), pointer :: pstru
      double precision  :: faci
      double precision  :: deltx
      double precision  :: hh
      double precision  :: wlev
      double precision  :: dads
      double precision  :: czz
      double precision  :: areadown
      double precision  :: wetp
      double precision  :: flowW
      double precision  :: dum
      integer           :: loc
      integer           :: nl, ml
      logical, external :: is_1D_inflowline
      
      double precision           :: cmu_dummy
      double precision           :: bob1
      double precision           :: bob2
      type(t_CrossSection)       :: crossDown
      
      adm = network%adm
      
      faci=1.0d0/(1.0d0+dt*bu0)
      loc = adm%lin2local(l)
      
      if (adm%lin2local(l) < 0) then
         ! not a 1d link or a 1d2d connection that has no structure attached
         kf = 0
         return
      endif
      
      if (adm%hasStructures) then
        pstru => network%sts%struct(adm%lin2str(l))
        if (is_1D_inflowline(l)) then
           deltx = 1d0
        else
           nl = adm%lin2ibr(l)
           ml = adm%lin2point(l)
           deltx = network%brs%branch(nl)%dx(ml)
        endif
    
        select case (pstru%st_type)
        case (ST_WEIR) 
          call ComputeWeir(pstru%weir, cu, ru, adm%au_1d(loc), dads, kf,  &
                            s1m1, s1m2, q, q, u1, u0,deltx, dt)
        case (ST_CULVERT) 
          if (s1m1 > s1m2) then 
            hh        = s1m2 + dpu
            wlev      = s1m2
            crossDown = network%crs%cross(adm%line2cross(l)%c2)
          else
            hh        = s1m1 + dpu
            wlev      = s1m1
            crossDown = network%crs%cross(adm%line2cross(l)%c1)
          endif
          
          bob1 = getBob(network%crs%cross(adm%line2cross(l)%c1))
          bob2 = getBob(network%crs%cross(adm%line2cross(l)%c2))
          
          call GetCSParsFlow(crossDown, hh, u1, czz, areaDown, wetP, flowW, dum)
                             
          !call ComputeCulvert(pstru%culvert, cu, ru, adm%au_1d(loc), dads, kf, s1m1, s1m2, q, q, u1, u0, deltx, dt, areaDown)


          call  ComputeCulvert(pstru%culvert, cu, ru, adm%au_1d(loc), dads, kf,          &
                               cmu_dummy, s1m1, s1m2, q, q, u1, u0, deltx, dt,            &
                               bob1, bob2, areadown, .true.)

          
          
         case (ST_PUMP)
          ! Pass a message if the pump switches on
          call ComputePump(pstru%pump, cu, ru, u1, q, adm%au_1d(loc))
          if (pstru%pump%discharge > 0) then
             kf = 1
          endif
        case (ST_ORIFICE)
           call ComputeOrifice(pstru%orifice, cu, ru, adm%au_1d(loc), dads, kf, s1m1, s1m2, q, q,   &
                       & u1, u0, deltx, dt)
        end select
      else
         cu=faci*dt*cu0
         ru=faci*(u0+dt*ru0)
      endif

   end subroutine curu_1d
   
   subroutine u1d(network, l, s1m1, s1m2, dpu, u1, bu0, cu0, ru0) !, cu, ru, compcuru)
      use m_network
      use m_globalParameters
      implicit none
  
      type(t_network), intent(in)    :: network
      integer, intent(in) :: l 
      double precision, intent(in)   :: s1m1
      double precision, intent(in)   :: s1m2
      double precision, intent(in)   :: dpu
      double precision, intent(in)   :: u1
      double precision, intent(out)  :: bu0
      double precision, intent(out)  :: cu0
      double precision, intent(out)  :: ru0
      !double precision, intent(out)  :: cu
      !double precision, intent(out)  :: ru
      !logical, intent(in)            :: compcuru
  
      type(t_administration_1d)     :: adm
      integer :: loc
      integer :: nl, ml
      double precision :: advec, diae, diai
      double precision :: hrd, dx
  
      advec=0.0
      diae=0.0
      diai=0.0 !dia
  
      hrd=max(thresholdDry,max(s1m1,s1m2)+dpu)
  
      adm = network%adm
      
      loc = adm%lin2local(l)
      nl  = adm%lin2ibr(l)
      ml  = adm%lin2point(l)
  
      dx = network%brs%branch(nl)%dx(ml)
      bu0=max(abs(u1),0.1d0)*gravity*adm%au_1d(loc)*adm%au_1d(loc)/(adm%conv_1d(loc)**2)
      bu0=bu0*max(hrd,1.0d-2)/max(hrd,1.0d-6)
      cu0=gravity/dx
      ru0=-diae*u1-advec

   end subroutine u1d

   
   
end module m_computation_1d   