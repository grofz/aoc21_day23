  module hall_mod
    implicit none

    real, parameter :: ENTR(*) = [2.5, 3.5, 4.5, 5.5]
    integer, parameter :: HALL_SIZE = 7

    type hall_t
      integer :: items(HALL_SIZE) = 0
    contains
      procedure :: able_through, able_to, able_from
    end type hall_t


  contains

    logical function able_through(this,room_src,room_dst)
      class(hall_t), intent(in) :: this
      integer, intent(in) :: room_src, room_dst
      real :: s1, s2
      s1 = ENTR(room_src)
      s2 = ENTR(room_dst)
      able_through = is_free_path(this%items,s1,s2)
    end function

    logical function able_to(this,room_src,dst)
      class(hall_t), intent(in) :: this
      integer, intent(in) :: room_src, dst
      real :: s1, s2
      s1 = ENTR(room_src)
      s2 = dst
      able_to = is_free_path(this%items,s1,s2)
    end function

    logical function able_from(this,src,room_dst)
      class(hall_t), intent(in) :: this
      integer, intent(in) :: room_dst, src
      real :: s1, s2
      s1 = src
      s2 = ENTR(room_dst)
      able_from = is_free_path(this%items,s1,s2)
    end function

    logical function is_free_path(hall, s1, s2)
      integer, intent(in) :: hall(:)
      real, intent(in) :: s1, s2
      integer :: lef, rig

      ! if path A - B is free, so is B - A
      ! we test the path from left to right
      lef = ceiling(min(s1, s2))
      rig = floor(max(s1, s2))
      is_free_path = .true.
      if (any(hall(lef:rig) /= 0)) is_free_path = .false.
    end function
  end module hall_mod
