      subroutine AGECART    ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(  1) ! I  Array of pointers in pmsa to get and store the data
      integer increm(  1) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt(  1)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) watersrc    ! I  Source of water to be traced                       (g/m3)
      real(4) ageprod     ! F  production of waterage                             (d)
      integer Iageprod    !    Pointer to the production of waterage
!
!*******************************************************************************
!
      ipnt        = ipoint
      Iageprod    = 1
!
      do 9000 iseg = 1 , noseg
!
         watersrc       = pmsa( ipnt(  1) )
!
!   *****     Insert your code here  *****
!
         ageprod    = watersrc
!
!   *****     End of your code       *****
!
         fl  ( Iageprod    ) = ageprod
!
         Iageprod    = Iageprod    + noflux
         ipnt        = ipnt        + increm
!
 9000 continue
!
      return
      end subroutine
