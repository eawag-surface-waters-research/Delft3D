module m_bedform
   use m_bedform_data
   !
   ! bedform prediction related
   !
   logical                           :: bfm_included   !< Include bedforms
   type(bedformpar_type), target     :: bfmpar         !< Bedform related parameters
   !
end module m_bedform
