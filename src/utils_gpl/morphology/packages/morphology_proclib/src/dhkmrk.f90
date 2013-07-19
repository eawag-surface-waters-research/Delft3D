subroutine dhkmrk ( iknmrk , kenmrk , knmrki )
!
!     DELFT HYDRAULICS     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june  1994 by Jan van Beek
!
!     FUNCTION            : utility that evaluates a feature from
!                           the "feature" integer
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     IKNMRK  INTEGER     1       INPUT   Index of feature
!     KENMRK  INTEGER     1       INPUT   feature
!     KNMRKI  INTEGER     1       OUTPUT  evaluated feature
!
    integer iknmrk, kenmrk, knmrki
!
!   Local
!
    integer dhimis
!
    if ( iknmrk .eq. 1 ) then
       knmrki = mod(kenmrk,10)
    elseif ( iknmrk .eq. 2 ) then
       knmrki = kenmrk / 10
       knmrki = mod(knmrki,10)
    elseif ( iknmrk .le. 0 .or. iknmrk .gt. 9 ) then
       dhimis = -999.
       knmrki = dhimis
    else
       knmrki = kenmrk / 10**(iknmrk-1)
       knmrki = mod(knmrki,10)
    endif

    return
end subroutine
