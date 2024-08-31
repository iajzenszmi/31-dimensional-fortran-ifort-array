          program Array31D
            implicit none
          integer, parameter :: N = 2
          integer, dimension(N**31) :: array
            integer :: indices(31)
             integer :: i, j, idx, value, pos
  
         ! Initialize array
         do i = 1, N**31
          array(i) = 0
          end do

          ! Function to calculate linear index
          idx = linear_index(indices, N)

           ! Insert value at a specific position (update)
             pos = 1234  ! Example position
                value = 42  ! Value to insert
            call insert(array, pos, value)

            ! Delete value at a specific position
            pos = 5678
           call delete(array, pos)
           print *, i, j, idx, value, pos, indices
             contains

        ! Function to calculate the linear index from 31D indices
          integer function linear_index(indices, N)
          integer, intent(in) :: indices(31), N
           integer :: i, index
           index = 1
           do i = 1, 31
           index = index + (indices(i) - 1) * N**(i-1)
           end do
          linear_index = index
         end function linear_index

          ! Subroutine to insert or update a value
          subroutine insert(arr, pos, val)
             integer, dimension(:), intent(inout) :: arr
             integer, intent(in) :: pos, val
             arr(pos) = val
             end subroutine insert

             ! Subroutine to delete a value (set it to zero)
             subroutine delete(arr, pos)
             integer, dimension(:), intent(inout) :: arr
             integer, intent(in) :: pos
             arr(pos) = 0
             end subroutine delete

             end program Array31D
