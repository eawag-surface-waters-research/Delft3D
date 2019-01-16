module m_dictype

! ------------------------------------------------------------------------------
!   Class:      TDict
!   Purpose:    Implements a key-value dictionary, (character to character)
!   Summary:    Key-value pairs can be added, retrieved, 
!               the existance of a key-value pair can be established, by key and value substring
!               The dictionary can be traversed element by element using the nnext method
!               The implementation is based on a linked list data structure
!   Descendand: None
!   Parent:     None
! ------------------------------------------------------------------------------

   type TDict
      integer                     ::  nelements = 0
      type(TDictElement), pointer ::  elements => null()
   contains
      procedure, pass :: add   => TDict_add             ! Insert a new element 
      procedure, pass :: uniq  => TDict_uniq            ! Add key-value pairs unique
      procedure, pass :: get   => TDict_get             ! Retrieve a value by key
      procedure, pass :: match => TDict_match           ! Match a key-value combination
      procedure, pass :: nnext => TDict_nnext           ! Fetch the next in line
   end type TDict

   interface TDict
      module procedure :: TDict_constructor
   end interface TDict

   type TDictElement                                    ! TODO: can I make this type private ?
      character(len=:), allocatable   :: key
      character(len=:), allocatable   :: val
        type(TDictElement), pointer   :: next => null()
   end type TDictElement


contains

type (TDict) function TDict_constructor()
   implicit none
   integer :: n
   n = TDict_constructor%add('key','val')
   
end function TDict_constructor

! ------------------------------------------------------------------------------
!   Method:     uniq    
!   Purpose:    append if key does not exists, replace value if key found (first occurrence)
!   Summary:    when using only uniq from the start of the dict, the keys will be unique.
!   Arguments:  newkey  -  key to be written
!               newval  -  value to be written
! ------------------------------------------------------------------------------
function TDict_uniq(self,newkey,newval) result (nn)
   implicit none
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: newkey
   character(len=*), intent(in)               :: newval
   character(len=:), allocatable              :: oldval
   type (TDictElement), pointer               :: elmptr => null() 
   integer                                    :: nn
   

   oldval = self%match(newkey,'',elmptr)
   if (associated(elmptr)) then
      elmptr%val = newval
      nn = self%nelements
   else
      nn = self%add(newkey,newval)
   endif
end function TDict_uniq

! ------------------------------------------------------------------------------
!   Method:     add     
!   Purpose:    stores a new key-value pair in the dictionary (inserted at position 0)
!   Summary:    The new element is inserted in the beginning of the linked list
!   Arguments:  newkey  -  key to be written
!               newval  -  value to be written
! ------------------------------------------------------------------------------
function TDict_add(self,newkey,newval) result (nn)
   implicit none
   integer                                    :: nn
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: newkey
   character(len=*), intent(in)               :: newval
   type (TDictElement), pointer               :: newkv  

   allocate(newkv)
   newkv%key = newkey
   newkv%val = newval
   newkv%next => self%elements
   self%elements => newkv
   self%nelements = self%nelements + 1
   nn = self%nelements
end function TDict_add

! ------------------------------------------------------------------------------
!   Method:     nnext     
!   Purpose:    traverse the linked list, retrieving all pairs
!   Summary:    facilitates a loop over all dictionary elements like this:
!   Example:    do while (my_dict%nnext(keystr,valstr,elmptr))
!                  print *, keystr,'=',valstr
!               enddo
!               so that all elements can be processed in succession
!   Arguments:  keystr  -  returned key
!               valstr  -  returned value
!               newkv   -  in : pointer to the element to be visted
!                          out: pointer to the succeeding element
! ------------------------------------------------------------------------------
function TDict_nnext(self,keystr,valstr,newkv) result (success)
   implicit none
   logical                                     :: success
   class(TDict), intent(inout)                 :: self
   character(len=:), allocatable, intent(out)  :: keystr
   character(len=:), allocatable, intent(out)  :: valstr
   type (TDictElement), pointer                :: newkv  
   if (.not.associated(newkv)) then
      newkv => self%elements
   else
      newkv => newkv%next
   endif
   keystr = newkv%key
   valstr = newkv%val
   success = associated(newkv%next)
end function TDict_nnext
   
! ------------------------------------------------------------------------------
!   Method:     match     
!   Purpose:    traverse the linked list, scanning for matching key-value pair
!   Summary:    returns the value of the first element matching given key and given substring of the value.  
!   Example:    my_dict%match('LOCATION','DA') matches key='LOCATION', value='DAAR', 'DATA', ... etc
!   Arguments:  keystr  -  key to match
!               substr  -  substring to match
!               newkv   -  pointer to the matching element
! ------------------------------------------------------------------------------
function TDict_match(self,keystr,substr,newkv) result (strout)
   implicit none
   character(len=:), allocatable              :: strout
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: keystr
   character(len=*), intent(in)               :: substr
   type (TDictElement), pointer               :: newkv  

   newkv => self%elements
   do while ((newkv%key /= keystr .or. index(newkv%val,substr) == 0) &
                                  .and. associated(newkv%next))
      newkv => newkv%next
   enddo
   if (newkv%key == keystr .and. index (newkv%val,substr) > 0) then
      strout = newkv%val
   else
      newkv => null()
   endif
end function

! ------------------------------------------------------------------------------
!   Method:     get     
!   Purpose:    Gets the value given the key (first match of the key) 
!               NB. The FIRST match will be in this implementation be the LAST added
!   Summary:    The name says it all ...
!   Example:    my_dict%get('LOCATION')
!   Arguments:  keystr  -  key to match
! ------------------------------------------------------------------------------
function TDict_get(self,keystr) result (strout)
   implicit none
   character(len=:), allocatable              :: strout
   class(TDict), intent(inout)                :: self
   character(len=*), intent(in)               :: keystr

   type (TDictElement), pointer               :: newkv  
   newkv => self%elements
   do while (newkv%key /= keystr .and. associated(newkv%next))
      newkv => newkv%next
   enddo
   if (newkv%key == keystr) then
      strout = newkv%val
   endif
end function

end module m_dictype
