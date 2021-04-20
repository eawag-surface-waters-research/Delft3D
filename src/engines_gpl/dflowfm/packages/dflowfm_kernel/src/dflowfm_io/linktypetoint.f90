   !> Parses a link type/mesh contact's type string into an integer
   !! that can be used to compare agains kn(3,:) codes.
   !!
   !! Currently supported names: internal, lateral, embedded, longitudinal, streetInlet, roofGutterPipe, all.
   function linkTypeToInt(linkTypeString) result (res)
   use string_module, only: str_tolower
   use m_inquire_flowgeom
      character(len=*), intent(in) :: linkTypeString  !< Type value as given in input file.
      integer                      :: res             !< The returned link type integer code. (3/4/5/7). -1 for unknown type.

      select case(str_tolower(trim(linkTypeString)))
      case('internal', 'lateral', 'embedded')
         res = IFLTP_1D2D_INT
      case('longitudinal')
         res = IFLTP_1D2D_LONG
      case('streetinlet')
         res = IFLTP_1D2D_STREET
      case('roofgutterpipe')
         res = IFLTP_1D2D_ROOF
      case('all') ! Special type to support selecting any link type
         res = IFLTP_ALL
      case default
         res = -1
      end select

   end function linkTypeToInt
