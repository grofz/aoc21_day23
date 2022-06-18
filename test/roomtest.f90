  program test_room
    use state_mod
    implicit none

    type(state_t) :: ST
    integer :: item, i0, i1, tot, cost
    character(len=2) arr
    logical :: isvalid

    ! test data
!   ST % ROOM(1) = room_t([1,2], 1)
!   ST % ROOM(2) = room_t([4,3], 2)
!   ST % ROOM(3) = room_t([3,2], 3)
!   ST % ROOM(4) = room_t([1,4], 4)
!   ST % ROOM(1) = room_t([1,4,4,2], 1)
!   ST % ROOM(2) = room_t([4,2,3,3], 2)
!   ST % ROOM(3) = room_t([3,1,2,2], 3)
!   ST % ROOM(4) = room_t([1,3,1,4], 4)

    ! real input
 !  ST % ROOM(1) = room_t([3,2], 1)
 !  ST % ROOM(2) = room_t([3,2], 2)
 !  ST % ROOM(3) = room_t([1,4], 3)
 !  ST % ROOM(4) = room_t([1,4], 4)
    ST % ROOM(1) = room_t([3,4,4,2], 1)
    ST % ROOM(2) = room_t([3,2,3,2], 2)
    ST % ROOM(3) = room_t([1,1,2,4], 3)
    ST % ROOM(4) = room_t([1,3,1,4], 4)


    !rooms(1) = room_t([integer :: ],1)
    !rooms(2) = room_t([1,1],2)

    call ST % print()
    goto 100
    tot = 0
    do
      call ST % print()
      print *, '$',tot,'$'
      print *
      write(*,'(a)', advance ='no') 'your move? '
      read(*,*) arr,i0,i1
      print *

      select case(arr)
      case('DI','di')
        call ST % move_direct(i0,isvalid,cost)
      case('TO','to')
        call ST % move_to_hall(i0,i1,isvalid,cost)
      case('FR','fr')
        call ST % move_from_hall(i0,isvalid,cost)
      case('EN', 'en')
        exit
      case default
        cycle
      end select
      if (isvalid) then
        print *, 'cost move = ',cost
        tot = tot + cost
      else
        print *, 'move is invalid'
      endif
    end do

    100 continue
    call find_best(ST, 0, tot)
    print *, 'tot = ',tot, count_calls
    print *, tot==46721 .or. tot==44169 .or. tot==10411 .or. tot==12521
      

  end program
