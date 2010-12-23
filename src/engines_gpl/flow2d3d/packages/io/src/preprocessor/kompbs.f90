subroutine kompbs(l         )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!!--description-----------------------------------------------------------------
!
!    Function: SIMULATION OF EXTERNAL KOMPBES-FILE
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    character(80), dimension(234), intent(out) :: l
                                   !!  Array with tidal components
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    l(1) = 'SA                 1                            '
    l(2) = 'SSA                2                            '
    l(3) = 'MSM          1  1 -2                  1 1       '
    l(4) = 'MM           1 -1                     1 1       '
    l(5) = 'MSF          2    -2                  1 1       '
    l(6) = 'MS0          2    -2    -2  2         1 6       '
    l(7) = 'MF           2          -2            1 2       '
    l(8) = 'KO0          2          -2  1 -2-10   1 3119    '
    l(9) = 'MK0          2          -2  2   -11   120       '
    l(10) = 'SNU          3  1 -4    -2  2         1 6       '
    l(11) = 'SN           3 -1 -2    -2  2         1 6       '
    l(12) = 'MSTM         3  1 -2    -2            1 2       '
    l(13) = 'MFM          3 -1       -2            1 2       '
    l(14) = '2SM          4    -4    -4  4         2 6       '
    l(15) = 'MSQM         4    -2    -2            1 2       '
    l(16) = 'MQM          4 -2       -2            1 2       '
    l(17) = '2SMN         5 -1 -4    -4  4         2 6       '
    l(18) = '2OK1      1 -4     1     4 -2 -1+10   2 3119    '
    l(19) = '2Q1       1 -4  2  1     2 -1  1      1 3       '
    l(20) = 'NJ1       1 -4  2  1     2 -1  1      1 41 6    '
    l(21) = 'SIGMA1    1 -4     3     2 -1  1      1 3       '
    l(22) = 'MUK1      1 -4     3     2 -2   +10   1 6119    '
    l(23) = 'NUJ1      1 -4     3     2 -1  1      1 41 6    '
    l(24) = 'Q1        1 -3  1  1     2 -1  1      1 3       '
    l(25) = 'NK1       1 -3  1  1     2 -2  1+10   1 6119    '
    l(26) = 'RO1       1 -3 -1  3     2 -1  1      1 3       '
    l(27) = 'NUK1      1 -3 -1  3     2 -2 +1+10   1 6119    '
    l(28) = 'O1        1 -2     1     2 -1  1      1 3       '
    l(29) = 'TAU1      1 -2     3       -1 -1      1 4       '
    l(30) = 'MP1       1 -2     3     2 -2 -1      1 6       '
    l(31) = 'M1B       1 -1 -1  1     2 -1 -1      1 3       '
    l(32) = 'M1C       1 -1     1     1 -1         112       '
    l(33) = 'M1A       1 -1  1  1       -1 -1      1 4       '
    l(34) = 'M1        1 -1  1  1       -1 -1-12   121       '
    l(35) = 'NO1       1 -1  1  1       -1 -1      1 31 6    '
    l(36) = 'CHI1      1 -1 -1 +3       -1 -1      1 4       '
    l(37) = 'LP1       1 -1 -1  3     2 -2  1-13   122       '
    l(38) = 'PI1       1       -2 +1        1                '
    l(39) = 'TK1       1       -2  1        1+10   119       '
    l(40) = 'P1        1       -1           1                '
    l(41) = 'SK1       1       -1           1+10   119       '
    l(42) = 'S1        1                                     '
    l(43) = 'K1        1        1          -1-10   119       '
    l(44) = 'MO1       1        1       -1 -1      1 31 6    '
    l(45) = 'SP1       1        1          -1                '
    l(46) = 'PSI1      1        2 -1       -1                '
    l(47) = 'RP1       1        2 -1        1                '
    l(48) = 'FI1       1        3          -1                '
    l(49) = 'KP1       1        3          -1-11   120       '
    l(50) = 'THETA1    1  1  1 -1       -1 -1      1 4       '
    l(51) = 'LABDAO1   1  1  1 -1       -1  1      1 31 6    '
    l(52) = 'J1        1  1 -1  1       -1 -1      1 4       '
    l(53) = 'MQ1       1  1 -1  1       -1 -1      1 31 6    '
    l(54) = '2PO1      1  2    -3    -2  1  1      1 3       '
    l(55) = 'SO1       1  2    -1    -2  1 -1      1 3       '
    l(56) = 'OO1       1  2     1    -2 -1 -1      1 5       '
    l(57) = '2KO1      1  2     1    -2  1  1-10-101 3219    '
    l(58) = 'UPSILON1  1  3 -1  1    -2 -1  1      1 5       '
    l(59) = 'KQ1       1  3 -1  1    -2  1 -1-11   1 3120    '
    l(60) = '2MN2S2    2 -7  1  6     6 -6         3 6       '
    l(61) = '3MKS2     2 -6     4     6 -6   +11   3 6120    '
    l(62) = '2NS2      2 -6  2  4     4 -4         2 6       '
    l(63) = '3MS2      2 -6     6     6 -6         3 6       '
    l(64) = 'OQ2       2 -5  1  2     4 -2  2      2 3       '
    l(65) = 'MNK2      2 -5  1  2     4 -4   +11   2 6120    '
    l(66) = 'EPSILON2  2 -5  1  4     2 -2         1 6       '
    l(67) = 'MNS2      2 -5  1  4     4 -4         2 6       '
    l(68) = '2ML2S2    2 -5 -1  6     6 -6  2-13   2 6122    '
    l(69) = 'MNUS2     2 -5 -1  6     4 -4         2 6       '
    l(70) = 'MNK2S2    2 -5  1  6     4 -4  0-11   2 6120    '
    l(71) = '2MS2K2    2 -4           4 -4   +11+112 6220    '
    l(72) = 'O2        2 -4     2     4 -2  2      2 3       '
    l(73) = 'NLK2      2 -4     2     4 -4  2+11-131 6120122 '
    l(74) = '2MK2      2 -4     2     4 -4   +11   1 6120    '
    l(75) = '2N2       2 -4  2  2     2 -2         1 6       '
    l(76) = 'MU2       2 -4     4     2 -2         1 6       '
    l(77) = '2MS2      2 -4     4     4 -4         2 6       '
    l(78) = 'SNK2      2 -3  1        2 -2   +11   1 6120    '
    l(79) = 'NA2       2 -3  1  1  1                         '
    l(80) = 'N2        2 -3  1  2     2 -2         1 6       '
    l(81) = 'KQ2       2 -3  1  2     2 -1   -10   1 3119    '
    l(82) = 'NB2       2 -3  1  3 -1                         '
    l(83) = 'NU2       2 -3 -1  4     2 -2         1 6       '
    l(84) = '3MSN2     2 -3  1  6     4 -4         4 6       '
    l(85) = '2KN2S2    2 -3  1  6     2 -2   -11-111 6220    '
    l(86) = 'OP2       2 -2           2 -1  2      1 3       '
    l(87) = 'MSK2      2 -2           2 -2   +11   1 6120    '
    l(88) = 'GAMMA2    2 -2  2        2 -2  2      1 6       '
    l(89) = 'ALFA2     2 -2     1     2 -2  2      1 6       '
    l(90) = 'MPS2      2 -2     1     2 -2  1      1 6       '
    l(91) = 'MA2       2 -2     1                            '
    l(92) = 'M2        2 -2     2     2 -2         1 6       '
    l(93) = 'KO2       2 -2     2     2 -1   -10   1 3119    '
    l(94) = 'MSP2      2 -2     3     2 -2 -1      1 6       '
    l(95) = 'MB2       2 -2     3                            '
    l(96) = 'DELTA2    2 -2     4       -2  0      1 7       '
    l(97) = 'MKS2      2 -2     4     2 -2   -11   1 6120    '
    l(98) = 'M2(KS)2   2 -2     6     2 -2   -11-111 6220    '
    l(99) = '2SN(MK)2  2 -1  1 -2            +11   2 6120    '
    l(100) = 'LABDA2    2 -1  1        2 -2  2      1 6       '
    l(101) = 'SNM2      2 -1  1                     2 6       '
    l(102) = '2MN2      2 -1 -1  2     2 -2         3 6       '
    l(103) = 'L2        2 -1 -1  2     2 -2  2-13   122       '
    l(104) = 'L2A       2 -1 -1  2     2 -2  2      1 6       '
    l(105) = 'L2B       2 -1  1  2       -2         1 7       '
    l(106) = '2SK2      2       -2            +11   120       '
    l(107) = 'T2        2       -1  1                         '
    l(108) = 'S2        2                                     '
    l(109) = 'KP2       2                     -10   119       '
    l(110) = 'R2        2        1 -1        2                '
    l(111) = 'K2        2        2            -11   120       '
    l(112) = 'MSNU2     2  1  1 -2                            '
    l(113) = 'MSN2      2  1 -1                     2 6       '
    l(114) = 'ZETA2     2  1  1          -2         1 7       '
    l(115) = 'ETA2      2  1 -1  2       -2         1 7       '
    l(116) = 'KJ2       2  1 -1  2       -1 -2-10   1 4119    '
    l(117) = 'MKN2      2  1 -1  2            -11   2 6120    '
    l(118) = '2KM(SN)2  2  1 -1  4            -11-112 6220    '
    l(119) = '2SM2      2  2    -2    -2  2         1 6       '
    l(120) = 'SKM2      2  2          -2  2   -11   1 6120    '
    l(121) = '2MS2N2    2  2 -2                     2 6       '
    l(122) = '2SNU2     2  3  1 -4    -2  2         1 6       '
    l(123) = '2SN2      2  3 -1 -2    -2  2         1 6       '
    l(124) = 'SKN2      2  3 -1       -2  2   -11   1 6120    '
    l(125) = 'MQ3       3 -5  1  3     4 -3  1      1 31 6    '
    l(126) = 'NO3       3 -5  1  3     4 -3  1      1 31 6    '
    l(127) = 'MO3       3 -4     3     4 -3  1      1 31 6    '
    l(128) = '2MK3      3 -4     3     4 -4  1+10   2 6119    '
    l(129) = '2MP3      3 -4     5     4 -4 -1      2 6       '
    l(130) = 'M3        3 -3     3     3 -3         117       '
    l(131) = 'NK3       3 -3  1  3     2 -2 -1-10   1 6119    '
    l(132) = 'SO3       3 -2     1     2 -1  1      1 3       '
    l(133) = 'MP3       3 -2     1     2 -2  1      1 6119    '
    l(134) = 'MK3       3 -2     3     2 -2 -1-10   1 6119    '
    l(135) = 'SP3       3       -1           1                '
    l(136) = '2MQ3      3 -1 -1  3     2 -3 -1      1 32 6    '
    l(137) = 'SK3       3        1          -1-10   119       '
    l(138) = '2SO3      3  2    -1    -2  1 -1      1 3       '
    l(139) = 'K3        3        3          -1-10-11119120    '
    l(140) = '4MS4      4 -8     8     8 -8         4 6       '
    l(141) = '2MNS4     4 -7  1  6     6 -6         3 6       '
    l(142) = '3MK4      4 -6     4     6 -6   +11   3 6120    '
    l(143) = 'MNLK4     4 -6     4     6 -6  2+11-132 6120122 '
    l(144) = '3MS4      4 -6     6     6 -6         3 6       '
    l(145) = 'MSNK4     4 -5  1  2     4 -4   +11   2 6120    '
    l(146) = 'MN4       4 -5  1  4     4 -4         2 6       '
    l(147) = 'MNU4      4 -5 -1  6     4 -4         2 6       '
    l(148) = '2MLS4     4 -5 -1  6     6 -6  2-13   2 6122    '
    l(149) = '2MSK4     4 -4     2     4 -4   +11   2 6120    '
    l(150) = 'M4        4 -4     4     4 -4         2 6       '
    l(151) = '2MKS4     4 -4     6     4 -4   -11   2 6120    '
    l(152) = 'SN4       4 -3  1  2     2 -2         1 6       '
    l(153) = '3MN4      4 -3 -1  4     4 -4         4 6       '
    l(154) = '2SMK4     4 -2           2 -2   +11   1 6120    '
    l(155) = 'MS4       4 -2     2     2 -2         1 6       '
    l(156) = 'MK4       4 -2     4     2 -2   -11   1 6120    '
    l(157) = '2SNM4     4 -1  1                     2 6       '
    l(158) = '2MSN4     4 -1 -1  2     2 -2         3 6       '
    l(159) = 'SL4       4 -1 -1  2     2 -2  2-13   122       '
    l(160) = 'S4        4                                     '
    l(161) = 'SK4       4        2            -11   120       '
    l(162) = '2SMN4     4  1 -1                     2 6       '
    l(163) = '3SM4      4  2    -2    -2  2         1 6       '
    l(164) = '2SKM4     4  2          -2  2   -11   1 6120    '
    l(165) = 'MNO5      5 -7  1  5     6 -5  1      1 32 6    '
    l(166) = '3MK5      5 -6     5     6 -6  1+10   3 6119    '
    l(167) = '3MP5      5 -6     7     6 -6 -1      3 6       '
    l(168) = 'M5        5 -5  1  5     4 -5 -1-12   2 6121    '
    l(169) = 'MNK5      5 -5  1  5     4 -4 -1-10   2 6119    '
    l(170) = '2MP5      5 -4     3     4 -4  1      2 6       '
    l(171) = 'MSO5      5 -4     3     4 -3         1 31 6    '
    l(172) = '3MO5      5 -4     5     4 -5 -1      1 33 6    '
    l(173) = 'MSK5      5 -2     3     2 -2 -1-10   1 6119    '
    l(174) = '3KM5      5 -2     5     2 -2 -3-14   1 6319    '
    l(175) = '2(MN)S6   6-10  2  8     8 -8         4 6       '
    l(176) = '3MNS6     6 -9  1  8     8 -8         4 6       '
    l(177) = '4MK6      6 -8     6     8 -8   +11   4 6120    '
    l(178) = '2NM6      6 -8  2  6     6 -6         3 6       '
    l(179) = '4MS6      6 -8     8     8 -8         4 6       '
    l(180) = '2MSNK6    6 -7  1  4     6 -6   +11   3 6120    '
    l(181) = '2MN6      6 -7  1  6     6 -6         3 6       '
    l(182) = '2MNU6     6 -7 -1  8     6 -6         3 6       '
    l(183) = '3MSK6     6 -6     4     6 -6   +11   3 6120    '
    l(184) = 'M6        6 -6     6     6 -6         3 6       '
    l(185) = 'MSN6      6 -5  1  4     4 -4         2 6       '
    l(186) = 'MNK6      6 -5  1  6     4 -4   -11   2 6120    '
    l(187) = '4MN6      6 -5 -1  6     6 -6         5 6       '
    l(188) = 'MKNU6     6 -5 -1  8     4 -4   -11   2 6120    '
    l(189) = '2(MS)K6   6 -4     2     4 -4   +11   2 6120    '
    l(190) = '2MS6      6 -4     4     4 -4         2 6       '
    l(191) = '2MK6      6 -4     6     4 -4   -11   2 6120    '
    l(192) = '2SN6      6 -3  1  2     2 -2         1 6       '
    l(193) = '3MSN6     6 -3 -1  4     4 -4         4 6       '
    l(194) = 'MKL6      6 -3 -1  6     4 -4  2-11-131 6120122 '
    l(195) = '2SM6      6 -2     2     2 -2         1 6       '
    l(196) = 'MSK6      6 -2     4     2 -2   -11   1 6120    '
    l(197) = 'S6        6                                     '
    l(198) = '2MNO7     7 -9  1  7     8 -7  1      1 33 6    '
    l(199) = '2NMK7     7 -8  2  7     6 -6 -1-10   3 6119    '
    l(200) = 'M7        7 -7  1  7     6 -7 -1-12   3 6121    '
    l(201) = '2MSO7     7 -6     5     6 -5  1      1 32 6    '
    l(202) = 'MSKO7     7 -4     5     4 -3  1-11   1 31 6120 '
    l(203) = '2(MN)8    8-10  2  8     8 -8         4 6       '
    l(204) = '3MN8      8 -9  1  8     8 -8         4 6       '
    l(205) = '3MNKS8    8 -9  1 10     8 -8   -11   4 6120    '
    l(206) = 'M8        8 -8     8     8 -8         4 6       '
    l(207) = '2MSN8     8 -7  1  6     6 -6         3 6       '
    l(208) = '2MNK8     8 -7  1  8     6 -6   -11   3 6120    '
    l(209) = '3MS8      8 -6     6     6 -6         3 6       '
    l(210) = '3MK8      8 -6     8     6 -6   -11   3 6120    '
    l(211) = '2SNM8     8 -5  1  4     4 -4         2 6       '
    l(212) = 'MSNK8     8 -5  1  6     4 -4   -11   2 6120    '
    l(213) = '2(MS)8    8 -4     4     4 -4         2 6       '
    l(214) = '2MSK8     8 -4     6     4 -4   -11   2 6120    '
    l(215) = '3SM8      8 -2     2     2 -2         1 6       '
    l(216) = '2SMK8     8 -2     4     2 -2   -11   1 6120    '
    l(217) = 'S8        8                                     '
    l(218) = '2(MN)K9   9-10  2  9     8 -8 -1-10   4 6119    '
    l(219) = '3MNK9     9 -9  1  9     8 -8 -1-10   4 6119    '
    l(220) = '4MK9      9 -8     9     8 -8 -1-10   4 6119    '
    l(221) = '3MSK9     9 -6     7     6 -6 -1-10   3 6119    '
    l(222) = '4MN10    10-11  1 10    10-10         5 6       '
    l(223) = 'M10      10-10    10    10-10         5 6       '
    l(224) = '3MSN10   10 -9  1  8     8 -8         4 6       '
    l(225) = '4MS10    10 -8     8     8 -8         4 6       '
    l(226) = '2(MS)N10 10 -7  1  6     6 -6         3 6       '
    l(227) = '2MNSK10  10 -7  1  8     6 -6   -11   3 6120    '
    l(228) = '3M2S10   10 -6     6     6 -6         3 6       '
    l(229) = '4MSK11   11 -8     9     8 -8 -1-10   4 6119    '
    l(230) = 'M12      12-12    12    12-12         6 6       '
    l(231) = '4MSN12   12-11  1 10    10-10         5 6       '
    l(232) = '5MS12    12-10    10    10-10         5 6       '
    l(233) = '3MNKS12  12 -9  1 10     8 -8   -11   4 6120    '
    l(234) = '4M2S12   12 -8     8     8 -8         4 6       '
end subroutine kompbs
