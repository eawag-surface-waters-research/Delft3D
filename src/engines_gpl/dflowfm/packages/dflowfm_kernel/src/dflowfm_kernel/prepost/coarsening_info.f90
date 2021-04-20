!> netcell-based cell-coarsening information
double precision function coarsening_info(k)

   use m_netw
   use m_missing
   use m_sferic, only: jsferic, jasfer3D, dtol_pole
   use gridoperations

   implicit none

   integer                    :: k   !< netcell number

   integer, parameter         :: NMAX = 100  !< maximum of directly or indirectly connected netcells

   integer                    :: ndirect, nindirect
   integer, dimension(NMAX)   :: kdirect, kindirect
   integer, dimension(2,nmax) :: kne

   double precision           :: xc, yc
   double precision           :: area, area_tot, Darea
   double precision           :: funct

   double precision           :: area_opt = 1d5

   integer                    :: kk

   coarsening_info = DMISS

   call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)

   if ( ndirect.lt.1 .or. nindirect.lt.1 ) return

   area_tot = 0d0
   funct    = 0d0

   call getcellsurface(k,area,xc,yc)
   area_tot = area_tot + area
   funct    = funct    - (area-area_opt)**2
   do kk=1,ndirect
      call getcellsurface(kdirect(kk),area,xc,yc)
      area_tot = area_tot + area
      funct    = funct    - (area-area_opt)**2
   end do

!  compute the area increase of the indirectly connected cells
   Darea = area_tot / dble(nindirect)

!  compute the change in the functional
   do kk=1,nindirect
      call getcellsurface(kindirect(kk),area,xc,yc)
      funct = funct - (area-area_opt)**2
      area = area + Darea
      funct = funct + (area-area_opt)**2
   end do

   coarsening_info = -funct
   if ( coarsening_info.le.0d0 ) coarsening_info = DMISS

   return
end function
