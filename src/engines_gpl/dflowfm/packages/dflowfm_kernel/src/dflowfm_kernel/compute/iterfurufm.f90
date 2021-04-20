logical function iterfurufm(m, su, sd, ustru, cu, rhsc, dxdt, lambda)

   !=======================================================================
   !                       Deltares
   !                One-Two Dimensional Modelling System
   !                           S O B E K
   !
   ! Subsystem:          Flow Module
   !
   ! Programmer:         Guus Stelling
   !
   ! Module:             iterfurufm (ITERFURU)
   !
   ! Module description: coefficients for momentum equation in wet weir point
   !
   !
   !     update information
   !     person                    date
   !
   !
   !
   !     Include Pluvius data space
   !
   ! use m_GlobalParameters
   ! use cpluv
   use m_strucs
   use m_flow
   use m_flowgeom, only : dx

   implicit none
!
! Global variables
!
!
   integer, intent(in)              :: m
   double precision, intent(in)     :: ustru, lambda
   double precision, intent(in)     :: cu
   double precision, intent(in)     :: rhsc
   double precision                 :: su   ! not s(up) but s(k1)
   double precision                 :: sd   ! not s(do) but s(k2), see switch in calling routine
   double precision                 :: dxdt
!
! Local variables
!
!
   double precision, parameter      :: relax = 0d0
   double precision                 :: bu
   double precision                 :: du, Cz
   double precision                 :: u1mi, dxfrL

!
!! executable statements -------------------------------- -----------------------
!
   dxfrL = 0d0
   if (lambda == 0) then        ! if structure defined friction == 0, use standard friction
      if (kmx == 0) then
         dxfrL = dx(m)*cfuhi(m)
      else if (frcu(m) > 0d0 ) then
         call getcz(hu(m), frcu(m), ifrcutp(m), Cz, m)      ! standard Chezy coeff
         dxfrl = dx(m)*ag/(Cz*Cz*hu(m))
      endif
   endif

   bu    = dxdt + (1.0 + relax + dxfrL)*ustru
   du    = (strucalfa*q1(m)/max(au(m),1d-4) + (1-strucalfa)*u0(m))*dxdt + relax*ustru*u1(m) + rhsc
   fu(m) = cu/bu
   ru(m) = du/bu
   u1mi  = u1(m)
   u1(m) = ru(m) + fu(m)*(su - sd)
   if (relax == 0.0) then
      iterfurufm = .false.
   else if (abs(u1mi - u1(m))>1.0D-6) then
      iterfurufm = .true.
   else
      iterfurufm = .false.
   endif
end function iterfurufm
