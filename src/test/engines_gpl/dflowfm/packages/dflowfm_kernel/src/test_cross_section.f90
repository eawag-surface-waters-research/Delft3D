!!  Copyright (C)  Stichting Deltares, 2012-2019.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module test_cross_sections
    use ftnunit
    use m_CrossSections


    implicit none
    double precision, parameter :: eps        = 1.0d-4
    logical                     :: hysteresis = .false.

contains

subroutine tests_cross_sections
    call test( test_circular_cross_section,     'Tests circular cross section' )
    call test( test_rectangular_cross_section,  'Tests rectangular cross section' )
    call test( test_egg_type_cross_section,     'Tests egg type cross section' )
    call test( test_tabulated_cross_section,    'Tests tabulated cross section' )
    call test( test_yz_cross_section,           'Tests yz type cross section' )
end subroutine tests_cross_sections

subroutine test_circular_cross_section
   use m_network
   use m_CrossSections
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer, parameter      :: n_crs_def = 5
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision        :: refdata(8,21)
   
   data refdata /0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002   , &
                 5.232189061598301D-004, 7.745966611618105D-002, 6.232189039246560D-004, 8.745966611618104D-002, 6.232189039246560D-004, 8.745966611618104D-002, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 1.450597565418631D-003, 0.105830051428804, 1.650597560948283D-003, 0.115830051428804, 1.650597560948283D-003, 0.115830051428804, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 2.609755597739188D-003, 0.124899958894225, 2.909755591033665D-003, 0.134899958894225, 2.909755591033665D-003, 0.134899958894225, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 3.930782911662094D-003, 0.138564063573127, 4.330782902721397D-003, 0.148564063573127, 4.330782902721397D-003, 0.148564063573127, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 5.368097146546678D-003, 0.148323967330787, 5.868097116744356D-003, 0.158323967330787, 5.868097116744356D-003, 0.158323967330787, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 6.886749312332053D-003, 0.154919333155753, 7.486749298921008D-003, 0.164919333155753, 7.486749298921008D-003, 0.164919333155753, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 8.457273026002063D-003, 0.158745078738970, 9.157273028982294D-003, 0.168745078738970, 9.157273028982294D-003, 0.168745078738970, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 1.005309620538504D-002, 0.160000000000000, 1.085309618750365D-002, 0.170000000000000, 1.085309618750365D-002, 0.170000000000000, 0.000000000000000D+000, 0.000000000000000D+000                  , &
                 1.164891938925597D-002, 0.158745079640108, 1.254891935051295D-002, 0.168745079640108, 1.255309583285601D-002, 0.170000000000000, 4.176482343061158D-006, 1.254920359892442D-003                  , &
                 1.321944253948841D-002, 0.154919336926267, 1.421944247988377D-002, 0.164919336926267, 1.425309547820838D-002, 0.170000000000000, 3.365299832460908D-005, 5.080663073733199D-003                  , &
                 1.473809530598015D-002, 0.148323970224139, 1.583809530001969D-002, 0.158323970224139, 1.595309639015944D-002, 0.170000000000000, 1.150010901397568D-004, 1.167602977586144D-002                  , &
                 1.617540957576887D-002, 0.138564067702658, 1.737540954894678D-002, 0.148564067702658, 1.765309603551181D-002, 0.170000000000000, 2.776864865650271D-004, 2.143593229734175D-002                  , &
                 1.749643670591410D-002, 0.124899967603473, 1.879643665823039D-002, 0.134899967603473, 1.935309568086417D-002, 0.170000000000000, 5.566590226337848D-004, 3.510003239652724D-002                  , &
                 1.865559543332596D-002, 0.105830051090877, 2.005559543928642D-002, 0.115830051090877, 2.105309659281523D-002, 0.170000000000000, 9.975011535288160D-004, 5.416994890912268D-002                  , &
                 1.958297336695773D-002, 7.745969924283107D-002, 2.108297327755076D-002, 8.745969924283106D-002, 2.275309497156890D-002, 0.170000000000000, 1.670121694018135D-003, 8.254030075716894D-002        , &
                 2.010619298286150D-002, 1.000000000000000D-002, 2.170619294709871D-002, 1.004784159602634D-002, 2.445309588351996D-002, 0.170000000000000, 2.746902936421249D-003, 0.159952158403974             , &
                 2.010619298297468D-002, 1.000000000000000D-002, 2.180619300085607D-002, 1.000000000000000D-002, 2.615309679547103D-002, 0.170000000000000, 4.346903794614953D-003, 0.160000000000000             , &
                 2.010619298297468D-002, 1.000000000000000D-002, 2.190619290548864D-002, 1.000000000000000D-002, 2.785309517422469D-002, 0.170000000000000, 5.946902268736048D-003, 0.160000000000000             , &
                 2.010619298297468D-002, 1.000000000000000D-002, 2.200619295913282D-002, 1.000000000000000D-002, 2.955309608617576D-002, 0.170000000000000, 7.546903127042936D-003, 0.160000000000000             , &
                 2.010619298297468D-002, 1.000000000000000D-002, 2.210619286376539D-002, 1.000000000000000D-002, 3.125309446492942D-002, 0.170000000000000, 9.146901601164031D-003, 0.160000000000000             /
   
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   call assert_equal( n_crs_def, network%CSDefinitions%count, 'Returned number of cross sections incorrect' )
   
   cross => network%crs%cross(1)
   
   
   do i = 1, 21
      dpt = (i-1)*0.01
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call GetCSParsTotal(cross, dpt, plusArea,  plusWidth,  CS_TYPE_PLUS, hysteresis)
      call GetCSParsTotal(cross, dpt, minArea,   minWidth,   CS_TYPE_MIN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )
      call assert_comparable( plusArea  , refdata(5,i), eps, "plusArea   is not correct" )
      call assert_comparable( plusWidth , refdata(6,i), eps, "plusWidth  is not correct" )
      call assert_comparable( minArea   , refdata(7,i), eps, "minArea    is not correct" )
      call assert_comparable( minWidth  , refdata(8,i), eps, "minWidth   is not correct" )

      continue
   enddo
   
end subroutine test_circular_cross_section

subroutine test_rectangular_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer, parameter      :: n_crs_def = 3
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision        :: refdata(8,25)
   
   data refdata /0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002        , &
                 1.999999955296517D-003, 0.200000000000000, 1.999999955296517D-003, 0.200000000000000, 1.999999955296517D-003, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.999999910593033D-003, 0.200000000000000, 3.999999910593033D-003, 0.200000000000000, 3.999999910593033D-003, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 5.999999865889550D-003, 0.200000000000000, 5.999999865889550D-003, 0.200000000000000, 5.999999865889550D-003, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 7.999999821186066D-003, 0.200000000000000, 7.999999821186066D-003, 0.200000000000000, 7.999999821186066D-003, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 9.999999403953554D-003, 0.200000000000000, 9.999999403953554D-003, 0.200000000000000, 9.999999403953554D-003, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.199999973177910D-002, 0.200000000000000, 1.199999973177910D-002, 0.200000000000000, 1.199999973177910D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.400000005960465D-002, 0.200000000000000, 1.400000005960465D-002, 0.200000000000000, 1.400000005960465D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.599999964237213D-002, 0.200000000000000, 1.599999964237213D-002, 0.200000000000000, 1.599999964237213D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.799999922513962D-002, 0.200000000000000, 1.799999922513962D-002, 0.200000000000000, 1.799999922513962D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.999999880790711D-002, 0.200000000000000, 1.999999880790711D-002, 0.200000000000000, 1.999999880790711D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.199999988079071D-002, 0.200000000000000, 2.199999988079071D-002, 0.200000000000000, 2.199999988079071D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.399999946355820D-002, 0.200000000000000, 2.399999946355820D-002, 0.200000000000000, 2.399999946355820D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.599999904632569D-002, 0.200000000000000, 2.599999904632569D-002, 0.200000000000000, 2.599999904632569D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.800000011920929D-002, 0.200000000000000, 2.800000011920929D-002, 0.200000000000000, 2.800000011920929D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.999999821186066D-002, 0.200000000000000, 2.999999821186066D-002, 0.200000000000000, 2.999999821186066D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.199999928474426D-002, 0.200000000000000, 3.199999928474426D-002, 0.200000000000000, 3.199999928474426D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.400000035762787D-002, 0.200000000000000, 3.400000035762787D-002, 0.200000000000000, 3.400000035762787D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.599999845027924D-002, 0.200000000000000, 3.599999845027924D-002, 0.200000000000000, 3.599999845027924D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.799999952316285D-002, 0.200000000000000, 3.799999952316285D-002, 0.200000000000000, 3.799999952316285D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.999999761581421D-002, 0.200000000000000, 3.999999761581421D-002, 0.200000000000000, 3.999999761581421D-002, 0.200000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 4.000100005000001D-002, 1.000000000000000D-002, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005        , &
                 4.000100005000001D-002, 1.000000000000000D-002, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005        , &
                 4.000100005000001D-002, 1.000000000000000D-002, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005        , &
                 4.000100005000001D-002, 1.000000000000000D-002, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005, 4.000100005000001D-002, 1.000000000000000D-005        /
   
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(2)
   
   do i = 1, 25
      dpt = (i-1)*0.01
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call GetCSParsTotal(cross, dpt, plusArea,  plusWidth,  CS_TYPE_PLUS, hysteresis)
      call GetCSParsTotal(cross, dpt, minArea,   minWidth,   CS_TYPE_MIN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )
      call assert_comparable( plusArea  , refdata(5,i), eps, "plusArea   is not correct" )
      call assert_comparable( plusWidth , refdata(6,i), eps, "plusWidth  is not correct" )
      call assert_comparable( minArea   , refdata(7,i), eps, "minArea    is not correct" )
      call assert_comparable( minWidth  , refdata(8,i), eps, "minWidth   is not correct" )

      continue
   enddo
   
end subroutine test_rectangular_cross_section

subroutine test_egg_type_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer, parameter      :: n_crs_def = 3
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision        :: refdata(8,25)
   
   data refdata /0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002        , &
                 1.118238118519718D-003, 7.999999932944774D-002, 1.318238114049370D-003, 8.999999932944773D-002, 1.318238114049370D-003, 8.999999932944773D-002, 0.000000000000000D+000, 0.000000000000000D+000        , &
                 3.001630699382438D-003, 0.107543100490580, 3.401630690441742D-003, 0.117543100490580, 3.401630690441742D-003, 0.117543100490580, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 5.390512100106582D-003, 0.130659965041610, 5.990512086695537D-003, 0.140659965041610, 5.990512086695537D-003, 0.140659965041610, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 8.202279516104631D-003, 0.149909081833885, 9.002279498223237D-003, 0.159909081833885, 9.002279498223237D-003, 0.159909081833885, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.136374684802399D-002, 0.165685420734553, 1.236374678841934D-002, 0.175685420734553, 1.236374678841934D-002, 0.175685420734553, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.480845715906485D-002, 0.178273290307780, 1.600845713224276D-002, 0.188273290307780, 1.600845713224276D-002, 0.188273290307780, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 1.847480202673402D-002, 0.187877538511298, 1.987480203269449D-002, 0.197877538511298, 1.987480203269449D-002, 0.197877538511298, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.230464219307269D-002, 0.194642748930474, 2.390464215730990D-002, 0.204642748930474, 2.390464215730990D-002, 0.204642748930474, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 2.624223421878257D-002, 0.198665180848380, 2.804223414129653D-002, 0.208665180848380, 2.804223414129653D-002, 0.208665180848380, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.023333854973315D-002, 0.200000000000000, 3.223333843052387D-002, 0.210000000000000, 3.223333843052387D-002, 0.210000000000000, 0.000000000000000D+000, 0.000000000000000D+000                       , &
                 3.420650864770922D-002, 0.195959179909324, 3.640650863578829D-002, 0.205959179909324, 3.643334068357944D-002, 0.210000000000000, 2.683204779115045D-005, 4.040820090675828D-003                       , &
                 3.801456591250638D-002, 0.183303032480681, 4.041456585886220D-002, 0.193303032480681, 4.063333980739116D-002, 0.210000000000000, 2.187739485289553D-004, 1.669696751931873D-002                       , &
                 4.146834744123478D-002, 0.160000014305113, 4.406834734586735D-002, 0.170000014305113, 4.483333893120288D-002, 0.210000000000000, 7.649915853355296D-004, 3.999998569488700D-002                       , &
                 4.430629020224818D-002, 0.119999996821086, 4.710629021416911D-002, 0.129999996821086, 4.903334118425844D-002, 0.210000000000000, 1.927050970089335D-003, 8.000000317891426D-002                       , &
                 4.594130039215089D-002, 1.000000000000000D-002, 4.894130021333695D-002, 1.000000000000000D-002, 5.323333717882632D-002, 0.210000000000000, 4.292036965489368D-003, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 4.914130032062531D-002, 1.000000000000000D-002, 5.743333943188188D-002, 0.210000000000000, 8.292039111256574D-003, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 4.934130042791367D-002, 1.000000000000000D-002, 6.163334168493747D-002, 0.210000000000000, 1.229204125702379D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 4.954130023717881D-002, 1.000000000000000D-002, 6.583333767950533D-002, 0.210000000000000, 1.629203744232652D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 4.974130034446717D-002, 1.000000000000000D-002, 7.003333993256089D-002, 0.210000000000000, 2.029203958809372D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 4.994130015373231D-002, 1.000000000000000D-002, 7.423333592712879D-002, 0.210000000000000, 2.429203577339648D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 5.014130026102067D-002, 1.000000000000000D-002, 7.843333818018434D-002, 0.210000000000000, 2.829203791916368D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 5.034130036830903D-002, 1.000000000000000D-002, 8.263334043323992D-002, 0.210000000000000, 3.229204006493089D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 5.054130017757416D-002, 1.000000000000000D-002, 8.683333642780779D-002, 0.210000000000000, 3.629203625023363D-002, 0.200000000000000                  , &
                 4.594130039215089D-002, 1.000000000000000D-002, 5.074130028486253D-002, 1.000000000000000D-002, 9.103333868086336D-002, 0.210000000000000, 4.029203839600083D-002, 0.200000000000000                  /
                                                                                                                                                                                                                                                                                                                                                                                                                 
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(3)
   
   do i = 1, 25
      dpt = (i-1)*0.02
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call GetCSParsTotal(cross, dpt, plusArea,  plusWidth,  CS_TYPE_PLUS, hysteresis)
      call GetCSParsTotal(cross, dpt, minArea,   minWidth,   CS_TYPE_MIN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )
      call assert_comparable( plusArea  , refdata(5,i), eps, "plusArea   is not correct" )
      call assert_comparable( plusWidth , refdata(6,i), eps, "plusWidth  is not correct" )
      call assert_comparable( minArea   , refdata(7,i), eps, "minArea    is not correct" )
      call assert_comparable( minWidth  , refdata(8,i), eps, "minWidth   is not correct" )

      continue
   enddo
   
end subroutine test_egg_type_cross_section

subroutine test_tabulated_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer, parameter      :: n_crs_def = 3
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision        :: refdata(4,25)
   
   data refdata /0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002         , &
                 61.5000009387732, 157.500000111759, 63.0000009834766, 165.000000223517                                 , &
                 126.000001966953, 165.000000223517, 132.000002145767, 180.000000447035                                 , &
                 193.500008225441, 172.500000894070, 207.000009298325, 195.000001788139                                 , &
                 264.000004291534, 180.000000447035, 288.000005006790, 210.000000894070                                 , &
                 337.500000000000, 187.500000000000, 375.000000000000, 225.000000000000                                 , &
                 414.000018596649, 195.000001788139, 468.000022888184, 240.000003576279                                 , &
                 493.499990344048, 202.499999105930, 566.999987840652, 254.999998211861                                 , &
                 576.000010013580, 210.000000894070, 672.000012874603, 270.000001788139                                 , &
                 661.500031113625, 217.500002682209, 783.000040769577, 285.000005364418                                 , &
                 750.000000000000, 225.000000000000, 900.000000000000, 300.000000000000                                 , &
                 844.000023365021, 245.000004768372, 1076.00005531311, 580.000066757202                                 , &
                 946.000050544740, 265.000009536743, 1364.00016403200, 860.000133514404                                 , &
                 1055.00007867813, 275.000000000000, 1750.00028610229, 1000.00000000000                                 , &
                 1164.99997377396, 275.000000000000, 2149.99990463257, 1000.00000000000                                 , &
                 1275.00000000000, 275.000000000000, 2550.00000000000, 1000.00000000000                                 , &
                 1385.00002622604, 275.000000000000, 2950.00009536743, 1000.00000000000                                 , &
                 1495.00005245209, 275.000000000000, 3350.00019073486, 1000.00000000000                                 , &
                 1605.00007867813, 275.000000000000, 3750.00028610229, 1000.00000000000                                 , &
                 1714.99997377396, 275.000000000000, 4149.99990463257, 1000.00000000000                                 , &
                 1825.00000000000, 275.000000000000, 4550.00000000000, 1000.00000000000                                 , &
                 1935.00015735626, 275.000000000000, 4950.00057220459, 1000.00000000000                                 , &
                 2045.00005245209, 275.000000000000, 5350.00019073486, 1000.00000000000                                 , &
                 2154.99994754791, 275.000000000000, 5749.99980926514, 1000.00000000000                                 , &
                 2265.00010490417, 275.000000000000, 6150.00038146973, 1000.00000000000                                 /
                                                                                                                                                                                                                   
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(4)
   
   do i = 1, 25
      dpt = (i-1)*0.4
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )

      continue
   enddo
   
end subroutine test_tabulated_cross_section

subroutine test_yz_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer, parameter      :: n_crs_def = 3
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision        :: refdata(4,25)
   
   data refdata /0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002     , &
                 11.2000003337860, 56.0000008344650, 11.2000003337860, 56.0000008344650                             , &
                 44.8000013351441, 112.000001668930, 44.8000013351441, 112.000001668930                             , &
                 100.800008010864, 168.000006675720, 100.800008010864, 168.000006675720                             , &
                 179.200005340576, 224.000003337860, 179.200005340576, 224.000003337860                             , &
                 280.000000000000, 280.000000000000, 280.000000000000, 280.000000000000                             , &
                 397.866696166993, 309.333340326945, 397.866696166993, 309.333340326945                             , &
                 527.466650517782, 338.666663169861, 527.466650517782, 338.666663169861                             , &
                 668.800017547607, 368.000003496806, 668.800017547607, 368.000003496806                             , &
                 821.866723505657, 397.333343823751, 821.866723505657, 397.333343823751                             , &
                 986.666666666667, 426.666666666667, 986.666666666667, 426.666666666667                             , &
                 1163.20004348755, 456.000006993612, 1163.20004348755, 456.000006993612                             , &
                 1351.46675923665, 485.333347320557, 1351.46675923665, 485.333347320557                             , &
                 1550.00014305115, 500.000000000000, 1550.00014305115, 500.000000000000                             , &
                 1749.99995231628, 500.000000000000, 1749.99995231628, 500.000000000000                             , &
                 1950.00000000000, 500.000000000000, 1950.00000000000, 500.000000000000                             , &
                 2150.00004768372, 500.000000000000, 2150.00004768372, 500.000000000000                             , &
                 2350.00009536743, 500.000000000000, 2350.00009536743, 500.000000000000                             , &
                 2550.00014305115, 500.000000000000, 2550.00014305115, 500.000000000000                             , &
                 2749.99995231628, 500.000000000000, 2749.99995231628, 500.000000000000                             , &
                 2950.00000000000, 500.000000000000, 2950.00000000000, 500.000000000000                             , &
                 3150.00028610229, 500.000000000000, 3150.00028610229, 500.000000000000                             , &
                 3350.00009536743, 500.000000000000, 3350.00009536743, 500.000000000000                             , &
                 3549.99990463257, 500.000000000000, 3549.99990463257, 500.000000000000                             , &
                 3750.00019073486, 500.000000000000, 3750.00019073486, 500.000000000000                             /
                                                                                                                                                                                                                                                                                                                                                                                                                 
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(5)
   cross%frictionSectionsCount = 1
   allocate(cross%frictionSectionID(1))     
   allocate(cross%frictionSectionFrom(1)) 
   allocate(cross%frictionSectionTo(1))   
   allocate(cross%frictionTypePos(1)  )
   allocate(cross%frictionValuePos(1) )
   allocate(cross%frictionTypeNeg(1)  )
   allocate(cross%frictionValueNeg(1) )
   cross%frictionSectionFrom(1) = 0d0
   cross%frictionSectionTo(1)   = 500d0
   cross%frictionTypePos(1)     = 1
   cross%frictionValuePos(1)    = 45d0
   cross%frictionTypeNeg(1)     = 1
   cross%frictionValueNeg(1)    = 45d0
   
   call CalcConveyance(cross)

   do i = 1, 25
      dpt = (i-1)*0.4
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )

      continue
   enddo

   deallocate(cross%frictionSectionID)     
   deallocate(cross%frictionSectionFrom) 
   deallocate(cross%frictionSectionTo)   
   deallocate(cross%frictionTypePos  )
   deallocate(cross%frictionValuePos )
   deallocate(cross%frictionTypeNeg  )
   deallocate(cross%frictionValueNeg )

end subroutine test_yz_cross_section

subroutine test_cross_section_helper(network, cross_section_definition_file)
   use m_readCrossSections
   use m_network
   
   type(t_network),     intent(inout)        :: network
   character(len=256),  intent(in   )        :: cross_section_definition_file

   integer           :: i
   type(t_crossSection),pointer :: pcrs
   type(t_CSType)      ,pointer :: pcsDef
   
   call readCrossSectionDefinitions(network, cross_section_definition_file)
   call realloc(network%crs)
   do i = 1, network%csdefinitions%count
      network%crs%count = network%crs%count + 1
      pcsDef            => network%CSDefinitions%cs(i)
      pcrs              => network%crs%cross(i)
      pcrs%csid         = pcsDef%id
      pcrs%iTabDef      = i
      pcrs%tabDef       => pcsDef
      call setparsCross(pcsDef, pcrs)
   enddo
   
end subroutine test_cross_section_helper
end module test_cross_sections
