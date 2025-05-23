program test_insert_flatten

  use iso_fortran_env, only: stderr => error_unit
  use nf, only: network, input, conv, maxpool, flatten, dense, reshape

  implicit none

  type(network) :: net
  logical :: ok = .true.

  net = network([ &
    input(3, 32, 32), &
    dense(10) &
  ])

  if (.not. net % layers(2) % name == 'flatten') then
    ok = .false.
    write(stderr, '(a)') 'flatten layer inserted after input3d.. failed'
  end if

  net = network([ &
    input(3, 32, 32), &
    conv(filters=1, kernel_width=3, kernel_height=3), &
    dense(10) &
  ])

  !call net % print_info()

  if (.not. net % layers(3) % name == 'flatten') then
    ok = .false.
    write(stderr, '(a)') 'flatten layer inserted after conv2d.. failed'
  end if

  net = network([ &
    input(3, 32, 32), &
    conv(filters=1, kernel_width=3, kernel_height=3), &
    maxpool(pool_width=2, stride=2), &
    dense(10) &
  ])

  if (.not. net % layers(4) % name == 'flatten') then
    ok = .false.
    write(stderr, '(a)') 'flatten layer inserted after maxpool.. failed'
  end if

  net = network([ &
    input(4), &
    reshape(1, 2, 2), &
    dense(4) &
  ])

  if (.not. net % layers(3) % name == 'flatten') then
    ok = .false.
    write(stderr, '(a)') 'flatten layer inserted after reshape.. failed'
  end if

  if (ok) then
    print '(a)', 'test_insert_flatten: All tests passed.'
  else
    write(stderr, '(a)') 'test_insert_flatten: One or more tests failed.'
    stop 1
  end if

end program test_insert_flatten