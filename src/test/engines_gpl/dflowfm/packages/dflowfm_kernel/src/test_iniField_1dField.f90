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
module test_ini_Field_1dField
    use ftnunit
    use precision

    implicit none
    real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
subroutine tests_iniField_1dField
    call test( test_iniField1dField, 'Tests the reading and interpolation of 1dField file via a iniField file.' )
    call test( test_iniField1dField_waterdepth, 'Tests iniField file with waterdepths.' )
    call test( test_iniField1dField_waterlevel, 'Tests iniField file with waterlevels.' )
end subroutine tests_iniField_1dField
!
!
!==============================================================================
subroutine test_iniField1dField
    use gridoperations
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use unstruc_channel_flow
    use m_network
    use m_inquire_flowgeom
    use dfm_error
    use m_partitioninfo, only: jampi
    use unstruc_files
    use ifport
    !
    ! Externals
    integer, external :: flow_modelinit
    !
    ! Locals 
    integer                   :: i, j, k, ibr
    integer                   :: istat, ierr
    double precision          :: refs1_br4(6)
    double precision          :: refs1_br10
    double precision          :: refs1_other
    
    double precision          :: chai
    type(t_branch), pointer   :: pbr
    character(len=256)        :: brId
    integer                   :: checkibr(5)
    ! reference: initial water levels
    data refs1_br4 /8.0,8.0,8.935625, 10.115884705882353,10.925428235294117,11.0/
    data refs1_br10 /5.0 /
    data refs1_other /10.0/
    
    ! branchIdx of branches that to be checked if the values are equal to the global value refs1_other
    data checkibr /19, 29, 40, 50, 57 /
    !
    ! Body
    jampi = 0
    kmax  = 2
    lmax  = 2
    numk  = 0
    !call inidat()
    
    call increaseNetw(kmax, lmax)

    call resetFullFlowModel()
    !
    istat = CHANGEDIRQQ("IniField1dField")
    call loadModel('Flow1D.mdu')
    istat = flow_modelinit()
    istat = CHANGEDIRQQ("..")
    
    ! check initial waterlevel s1 on branch 4
    ibr = 4
    pbr => network%brs%branch(ibr)
    brId = pbr%id
    j = 1
    do i =1, pbr%gridPointsCount
       chai = pbr%gridPointsChainages(j)
       ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
       if (ierr == DFM_NOERR) then
          if (chai < 300.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 1 incorrect' )
             j = j + 1
          else if (chai >= 300.0 .and. chai < 500.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 1 incorrect' )
             j = j + 1
          else if (chai >= 500.0 .and. chai < 1350.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 1 incorrect' )
             j = j + 1
          else if (chai >= 1350.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 1 incorrect' )
          end if
       else
          write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
          call err_flush() 
       end if
    end do
    
    ! on branch 10
    ibr = 10
    pbr => network%brs%branch(ibr)
    brId = pbr%id
    do i =1, pbr%gridPointsCount
       chai = pbr%gridPointsChainages(i)
       ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
       if (ierr == DFM_NOERR) then
          call assert_comparable(s1(k), refs1_br10, eps, 'initial waterlevel on branch 4 incorrect' )
       else
          write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
          call err_flush() 
       end if
    end do
    
    ! on selected branches, check the 2nd flownodes on each of them
    do j = 1, size(checkibr)
       ibr = checkibr(j)
       pbr => network%brs%branch(ibr)
       brId = pbr%id
       chai = pbr%gridPointsChainages(2)
       ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
       if (ierr == DFM_NOERR) then
          call assert_comparable(s1(k), refs1_other, eps, 'initial waterlevel on other branches incorrect' )
       else
          write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
          call err_flush() 
       end if
      
    end do
    
    
end subroutine test_iniField1dField
!
!
!==============================================================================
subroutine test_iniField1dField_waterdepth
    use gridoperations
    use m_cell_geometry, only: ndx
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use unstruc_channel_flow
    use m_network
    use m_inquire_flowgeom
    use dfm_error
    use m_partitioninfo, only: jampi
    use unstruc_files
    use ifport
    !
    ! Externals
    integer, external :: flow_modelinit
    !
    ! Locals  
    integer                                     :: i
    integer                                     :: istat, ierr
    double precision, dimension(:), allocatable :: refs1
    !
    ! Body
    jampi = 0
    kmax  = 2
    lmax  = 2
    numk  = 0
    allocate(refs1(168), stat=istat)
    refs1 = 0.0d0
    !call inidat()
    
    call increaseNetw(kmax, lmax)

    call resetFullFlowModel()
    !
    istat = CHANGEDIRQQ("IniField1dField_waterdepth")
    call loadModel('dflow1d.mdu')
    istat = flow_modelinit()
    istat = CHANGEDIRQQ("..")
    
    do i = 1, ndx
       call assert_comparable(s1(i), refs1(i), eps, 'initial waterlevel on branch 1 incorrect' )
    end do
    
    deallocate(refs1, stat=istat)
end subroutine test_iniField1dField_waterdepth
!
!
!==============================================================================
subroutine test_iniField1dField_waterlevel
    use gridoperations
    use m_cell_geometry, only: ndx
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use unstruc_channel_flow
    use m_network
    use m_inquire_flowgeom
    use dfm_error
    use m_partitioninfo, only: jampi
    use unstruc_files
    use ifport
    !
    ! Externals
    integer, external :: flow_modelinit
    !
    ! Locals  
    integer                                     :: i
    integer                                     :: istat, ierr
    double precision                            :: deltas
    double precision, dimension(:), allocatable :: refs1
    !
    ! Body
    jampi = 0
    kmax  = 2
    lmax  = 2
    numk  = 0
    allocate(refs1(168), stat=istat)
    refs1 = 5.0d0
    !
    ! Branch 2
    do i = 9, 21
        refs1(i) = 7.0d0
    end do
    !
    ! Branch 3
    deltas = 3.0d0/22.0d0
    do i = 22, 44
        refs1(i) = 8.0d0 - dble(i-22) * deltas
    end do
    !
    ! Branch 4
    deltas = 3.0d0/42.0d0
    do i = 45, 88
        refs1(i) = 8.0d0 - dble(i-45) * deltas
    end do
    !
    ! Branch 5
    deltas = 3.0d0/80.0d0
    do i = 89, 168
        refs1(i) = 8.0d0 - dble(i-89) * deltas
    end do
    !call inidat()
    
    call increaseNetw(kmax, lmax)

    call resetFullFlowModel()
    !
    istat = CHANGEDIRQQ("IniField1dField_waterlevel")
    call loadModel('dflow1d.mdu')
    istat = flow_modelinit()
    istat = CHANGEDIRQQ("..")
    
    do i = 1, ndx
       call assert_comparable(s1(i), refs1(i), eps, 'initial waterlevel is incorrect' )
    end do
    
    deallocate(refs1, stat=istat)
end subroutine test_iniField1dField_waterlevel

end module test_ini_Field_1dField
