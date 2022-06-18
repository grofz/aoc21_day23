  module room_mod
    implicit none

    !integer, parameter :: ROOM_SIZE = 2
    integer, parameter :: ROOM_SIZE = 4

    type room_t
      !private
      integer :: items(ROOM_SIZE) = 0
      integer :: n = 0
      integer :: id
    contains
      procedure :: isdone     !F isdone()
      procedure :: canpush    !F canpush(item)
      procedure :: push       !S push(item,steps)
      procedure :: pop        !S pop(item,steps)
      procedure :: top        !F top()
    end type room_t

    interface room_t
      module procedure room_init
    end interface 

  contains

    function room_init(arr,id) result(this)
      integer, intent(in) :: arr(:)
      integer, intent(in) :: id
      type(room_t) :: this

      if (size(arr) > ROOM_SIZE) error stop 'init - too many items'
      this % items(1:size(arr)) = arr
      this % n = size(arr)
      this % id = id
    end function



    pure logical function isdone(this)
      class(room_t), intent(in) :: this
!
! Room is done if contains only its designated occupants
!    
      isdone = .true.
      if (this%n == 0) return
      if (any(this % items(1:this%n) /= this %id)) isdone = .false.
    end function



    pure logical function canpush(this, item)
      class(room_t), intent(in) :: this
      integer, intent(in) :: item
!
! Can push "item" to room only if there is space, "item" is designated occupant
! and no "visiting" occupants are present
!
      canpush = this % isdone()
      if (this % n == ROOM_SIZE) canpush = .false.
      if (item /= this % id) canpush = .false.
    end function



    pure integer function top(this) result(item)
      class(room_t), intent(in) :: this
      if (this % n == 0) then
        item = 0
      else
        item = this % items(this % n)
      end if
    end function



    subroutine push(this, item, steps)
      class(room_t), intent(inout) :: this
      integer, intent(in) :: item
      integer, intent(out) :: steps

      if (.not. this % canpush(item)) error stop 'push - not allowed'
      steps = ROOM_SIZE - this % n
      this % n = this % n + 1
      this % items(this % n) = item
    end subroutine



    subroutine pop(this, item, steps)
      class(room_t), intent(inout) :: this
      integer, intent(out) :: item, steps

      if (this % n == 0) error stop 'pop - empty room'
      if (this % isdone()) error stop 'pop - no need to pop'
      item = this % items(this % n)
      this % items(this % n) = 0
      steps = ROOM_SIZE - this % n + 1
      this % n = this % n - 1
    end subroutine

  end module room_mod
