subroutine snap_candidates_q(target, candidates, max_snap_dist, snap, candidates__len_, target__len_) bind(c)
  use iso_c_binding, only: c_int, c_ptrdiff_t
  implicit none

  ! manifest start
  ! sizes
  integer(c_ptrdiff_t), intent(in), value :: target__len_
  integer(c_ptrdiff_t), intent(in), value :: candidates__len_

  ! args
  integer(c_int), intent(in) :: target(target__len_)
  integer(c_int), intent(in) :: candidates(candidates__len_)
  integer(c_int), intent(in) :: max_snap_dist
  integer(c_int), intent(out) :: snap(target__len_)

  ! locals
  integer(c_int) :: ld
  integer(c_int) :: nc
  integer(c_int) :: x
  integer(c_int) :: right
  integer(c_int) :: ilo
  integer(c_int) :: rd
  integer(c_int) :: step
  integer(c_int) :: mid
  integer(c_int) :: i
  integer(c_int) :: nt
  integer(c_int) :: ihi
  integer(c_int) :: left
  ! manifest end


  nt = size(target)
  nc = size(candidates)
  snap = 0
  ilo = 1_c_int
  do i = 1_c_int, nt, sign(1, nt-1_c_int)
    x = target(i)
    if ((x < candidates(ilo))) then
      step = 1_c_int
      do
        ihi = ilo
        ilo = (ilo - step)
        if ((ilo <= 1_c_int)) then
          ilo = 1_c_int
          exit
        end if
        if ((x >= candidates(ilo))) then
          exit
        end if
        step = (step * 2_c_int)
      end do
    else
      step = 1_c_int
      ihi = min((ilo + 1_c_int), nc)
      do while ((ihi < nc) .and. (x >= candidates(ihi)))
        ilo = ihi
        step = (step * 2_c_int)
        ihi = min((ilo + step), nc)
      end do
    end if
    do while (((ihi - ilo) > 1_c_int))
      mid = int(floor(real((ilo + ihi)) / real(2_c_int)))
      if ((x < candidates(mid))) then
        ihi = mid
      else
        ilo = mid
      end if
    end do
    left = candidates(ilo)
    if ((ilo < nc)) then
      right = candidates((ilo + 1_c_int))
    else
      right = candidates(nc)
    end if
    ld = abs((x - left))
    rd = abs((x - right))
    if ((ld <= max_snap_dist) .or. (rd <= max_snap_dist)) then
      if ((rd < ld)) then
        snap(i) = right
      else
        snap(i) = left
      end if
    else
      snap(i) = x
    end if
  end do
end subroutine
