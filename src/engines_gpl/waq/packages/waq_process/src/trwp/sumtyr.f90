      subroutine SUMTYR     ( pmsa   , fl     , ipoint , increm, noseg , &                            
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &                            
                              noq3   , noq4   )
      use m_dhkmrk
                                                       
                                                                                                      
!                                                                                                     
!*******************************************************************************                      
!                                                                                                     
      IMPLICIT NONE                                                                                   
!                                                                                                     
!     Type    Name         I/O Description                                                            
!                                                                                                     
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library     
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time               
      integer ipoint( *)  ! I  Array of pointers in pmsa to get and store the data                    
      integer increm( *)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying 
      integer noseg       ! I  Number of computational elements in the whole model schematisation     
      integer noflux      ! I  Number of fluxes, increment in the fl array                            
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces     
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use              
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid  
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward    
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)     
!                                                                                                     
!*******************************************************************************                      
!                                                                                                     
!     Type    Name         I/O Description                                        Unit                
!                                                                                                     
!     support variables
      integer iseg,ikmrk1
      
!     output items
      real(4) sumtrwp
      real(4) sumtrwpsed
      real(4) sumsusp
      
      integer           :: ipnt(500)  
      integer,parameter :: ip_ntrwp = 1
      integer,parameter :: ip_nim = 2
      integer,parameter :: ip_lastsingle = 2
    
      integer :: ntrwp, itrwp, nspm, ispm, nitem, offset, itel
    
      ntrwp = pmsa(ipoint(ip_ntrwp))
      nspm = pmsa(ipoint(ip_nim))
      nitem = ip_lastsingle+ntrwp+2*ntrwp*nspm+nspm+3
!
      ipnt(1:nitem) = ipoint(1:nitem)
      
      ! calculate sums of TRWPs in water and sediments 

      ! loop to accumulate fractions and to aggregate all water layers with aquatic sediments
      do iseg = 1,noseg
          call dhkmrk(1,iknmrk(iseg),ikmrk1)
          if (ikmrk1.eq.1) then

            sumtrwp = 0.0 ! g/m3
            sumtrwpsed = 0.0 ! g/m2
            sumsusp = 0.0 ! g/m2
            
            offset = ip_lastsingle
            do itrwp = 1,ntrwp
                sumtrwp = sumtrwp + pmsa(ipnt(offset+itrwp))
            enddo

            itel = 0
            offset = ip_lastsingle+ntrwp
            do itrwp = 1,ntrwp
            do ispm = 1,nspm
                itel = itel + 1
                sumtrwp    = sumtrwp    + pmsa(ipnt(offset           +itel))
                sumtrwpsed = sumtrwpsed + pmsa(ipnt(offset+ntrwp*nspm+itel))
            enddo
            enddo
            
            offset = ip_lastsingle+ntrwp+2*ntrwp*nspm
            do ispm = 1,nspm
                sumsusp = sumsusp + pmsa(ipnt(offset+ispm))
            enddo
            offset = ip_lastsingle+ntrwp+2*ntrwp*nspm+nspm
            pmsa(ipnt(offset+1)) = sumtrwp
            pmsa(ipnt(offset+2)) = sumtrwpsed
            pmsa(ipnt(offset+3)) = sumsusp
            
          endif
          ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
      enddo
      

      return
      end
