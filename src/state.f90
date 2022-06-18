  module state_mod
    use hall_mod
    use room_mod
    implicit none
    type state_t
      type(hall_t) :: hall
      type(room_t) :: room(4)
    contains
      procedure :: issolved => state_issolved
      procedure :: print => state_print
      procedure :: move_direct, move_to_hall, move_from_hall
      procedure :: isvalid => state_isvalid
    end type

    integer, parameter :: ROOM_MET(*) = [2,4,6,8]
    integer, parameter :: HALL_MET(*) = [0,1,3,5,7,9,10]

integer, save :: best_result = huge(best_result), count_calls = 0

  contains

    pure logical function state_isvalid(this)
      class(state_t), intent(in) :: this

      integer :: i, j, cnt(4)

      cnt = 0
      do i=1, HALL_SIZE
        associate(ind => this % hall % items(i))
          if (ind == 0) cycle
          cnt(ind) = cnt(ind) + 1
        end associate
      end do
      do i=1,4
        do j=1, this % room(i) % n
          associate(ind => this % room(i) % items(j))
            if (ind == 0) error stop 'isvalid error'
            cnt(ind) = cnt(ind) + 1
          end associate
        end do
      end do
      state_isvalid = count(cnt==ROOM_SIZE)==4
    end function



    subroutine move_direct(this, ifrom, valid, cost)
      class(state_t), intent(inout) :: this
      integer, intent(in) :: ifrom
      logical, intent(out) :: valid
      integer, intent(out) :: cost

      integer :: item, steps1, steps2, steps3, ito

      cost = 0
      valid = .false.

      ! must be able to remove from source room to continue
      if (this % room(ifrom) % isdone()) return
      item = this % room(ifrom) % top()
      ito = item

      ! must be able to push to destination room to continue
      if (.not. this % room(ito) % canpush(item)) return

      ! route must be free to continue
      if (.not. this % hall % able_through(ifrom,ito)) return

      ! move is valid, make it and compute the cost
      valid = .true.
      call this % room(ifrom) % pop(item, steps1)
      call this % room(ito) % push(item, steps2)
      !steps3 = 2*abs(ifrom-ito)
      steps3 = abs(ROOM_MET(ifrom)-ROOM_MET(ito))
      cost = (steps1+steps2+steps3)*(10**(item-1))

      if (.not. this % isvalid()) error stop 'move_direct - screwed'
    end subroutine move_direct



    subroutine move_to_hall(this, ifrom, ito, valid, cost)
      class(state_t), intent(inout) :: this
      integer, intent(in) :: ifrom, ito
      logical, intent(out) :: valid
      integer, intent(out) :: cost

      integer :: item, steps1, steps2

      cost = 0
      valid = .false.

      ! must be able to remove from source room to continue
      if (this % room(ifrom) % isdone()) return
      item = this % room(ifrom) % top()

      ! route must be free to continue
      if (.not. this % hall % able_to(ifrom,ito)) return

      ! move is valid, make it and compute the cost
      valid = .true.
      call this % room(ifrom) % pop(item, steps1)
      if (this % hall % items(ito) /= 0) error stop 'move to hall - occupied'
      this % hall % items(ito) = item
      steps2 = abs(ROOM_MET(ifrom) - HALL_MET(ito))
      cost = (steps1+steps2)*(10**(item-1))

      if (.not. this % isvalid()) error stop 'move_to_hall - screwed'
    end subroutine move_to_hall



    subroutine move_from_hall(this, ifrom, valid, cost)
      class(state_t), intent(inout) :: this
      integer, intent(in) :: ifrom
      logical, intent(out) :: valid
      integer, intent(out) :: cost

      integer :: item, ito, steps1, steps2

      cost = 0
      valid = .false.

      ! item must be present
      item = this % hall % items(ifrom)
      if (item == 0) return
      ito = item

      ! must be able to push to destination room to continue
      if (.not. this % room(ito) % canpush(item)) return

      ! route must be free to continue
      this % hall % items(ifrom) = 0
      if (.not. this % hall % able_from(ifrom,ito)) then
        this % hall % items(ifrom) = item
        return
      endif

      ! move is valid, make it and compute the cost
      valid = .true.
      call this % room(ito) % push(item, steps1)
      steps2 = abs(ROOM_MET(ito) - HALL_MET(ifrom))
      cost = (steps1+steps2)*(10**(item-1))

      if (.not. this % isvalid()) error stop 'move_from_hall - screwed'
    end subroutine move_from_hall
      


    subroutine state_print(this)
      class(state_t), intent(in) :: this

      character(len=HALL_SIZE) :: hall
      character(len=ROOM_SIZE) :: rooms(4)

      integer :: i, j

      do i=1,HALL_SIZE
        hall(i:i) = ch(this%hall%items(i))
      end do

      do j=1,4
      do i=1,ROOM_SIZE
        rooms(j)(i:i) = ch(this%room(j)%items(i))
      end do
      end do

      print '(a2,1x,a1,1x,a1,1x,a1,1x,a2)', &
       hall(1:2),hall(3:3),hall(4:4),hall(5:5),hall(6:7)
      do i=ROOM_SIZE,1,-1
        print '(2x,a1,1x,a1,1x,a1,1x,a1)', &
          rooms(1)(i:i), rooms(2)(i:i), rooms(3)(i:i), rooms(4)(i:i)
      end do
      print *
    contains
      function ch(i)
        character(len=1) :: ch
        integer, intent(in) :: i

        if (i<0 .or. i>4) error stop 'invalid value in print'
        if (i==0) then
          ch = '.'
        else
          ch = achar(i+64)
        end if
      end function

    end subroutine state_print



    logical function state_issolved(this)
      class(state_t), intent(in) :: this
      logical :: isdone(4), isfull(4)
      integer :: i

      isfull = this % room(:) % n == ROOM_SIZE
      do i = 1, 4
        isdone(i) = this % room(i) % isdone()
      end do
      state_issolved = all(isfull) .and. all(isdone)
    end function





    recursive subroutine find_best(state,lev,cost)
      type(state_t), intent(in) :: state
      integer, intent(in) :: lev
      type(state_t)  :: wrkstate, state0
      integer, intent(out) :: cost

      integer :: i, j, bestcost,wrkcost,newcost,cost0
      logical :: res, changed

 count_calls = count_calls + 1

      
      cost0 = 0
      state0 = state

      ! try all optimal moves
      do
        changed = .false.

        ! clean the hallway if possible
        do i=1,HALL_SIZE
          call state0 % move_from_hall(i,res,wrkcost)
          if (.not. res) cycle ! ignore invalid moves
          cost0 = cost0 + wrkcost
          changed = .true.
        end do

        ! make all possible direct moves
        do i=1,4
          call state0 % move_direct(i,res,wrkcost)
          if (.not. res) cycle ! ignore invalid moves
          cost0 = cost0 + wrkcost
          changed = .true.
        end do

        if (.not. changed) exit
      end do

      if (state0 % issolved()) then
        ! if state is final - return
 if (cost0 < best_result) then
   print *, 'targetfound', cost0, lev
   best_result = cost0
 endif
        cost = cost0
        return
      end if
      
      ! test moving to hallway
      bestcost = huge(bestcost)/2
      do i=1,4
        if (state0 % room(i) % isdone()) cycle
        do j=1,HALL_SIZE
          wrkstate = state0
          call wrkstate % move_to_hall(i,j,res,wrkcost)
          if (.not. res) cycle ! ignore invalid moves

          ! recursively explore all other moves
          call find_best(wrkstate,lev+1,newcost)

          ! keep track of the best solution so far
          if (bestcost > newcost + cost0 + wrkcost) then
            bestcost = cost0 + wrkcost + newcost
          end if
        end do
      end do
      cost = bestcost
    end subroutine


  end module state_mod
