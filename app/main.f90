  program main
    use state_mod
    use room_mod, only : room_t
    implicit none

    type(state_t) :: st
    type(state_t), allocatable :: sol(:)
    integer :: i0, tot

    write(*,'(a)',advance='no') 'Test case (0) or real case (1) ?'
    read(*,*) i0
    select case(i0)
    case(0)
      ! test data
!     ST % ROOM(1) = room_t([1,2], 1)
!     ST % ROOM(2) = room_t([4,3], 2)
!     ST % ROOM(3) = room_t([3,2], 3)
!     ST % ROOM(4) = room_t([1,4], 4)
      ST % ROOM(1) = room_t([1,4,4,2], 1)
      ST % ROOM(2) = room_t([4,2,3,3], 2)
      ST % ROOM(3) = room_t([3,1,2,2], 3)
      ST % ROOM(4) = room_t([1,3,1,4], 4)
    case(1)
      ! real input
 !    ST % ROOM(1) = room_t([3,2], 1)
 !    ST % ROOM(2) = room_t([3,2], 2)
 !    ST % ROOM(3) = room_t([1,4], 3)
 !    ST % ROOM(4) = room_t([1,4], 4)
      ST % ROOM(1) = room_t([3,4,4,2], 1)
      ST % ROOM(2) = room_t([3,2,3,2], 2)
      ST % ROOM(3) = room_t([1,1,2,4], 3)
      ST % ROOM(4) = room_t([1,3,1,4], 4)
    case default
      stop 'nothing to do'
    end select

    call st % print()
    call find_best(st, 0, tot, sol)
    do i0=1,size(sol)
      call sol(i0) % print()
    enddo
    print '("Total cost = ",i0,"  Sub called = ",i0)', tot, count_calls
    print '("Moves = ",i0)', size(sol)
    print '("Correct answer? ",l1)', &
    &   tot==46721 .or. tot==44169 .or. tot==10411 .or. tot==12521

  end program main
