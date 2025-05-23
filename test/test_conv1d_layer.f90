program test_conv1d_layer

    use iso_fortran_env, only: stderr => error_unit
    use nf, only: conv, input, layer
    use nf_input2d_layer, only: input2d_layer
  
    implicit none
  
    type(layer) :: conv1d_layer, input_layer
    integer, parameter :: filters = 32, kernel_size=3
    real, allocatable :: sample_input(:,:), output(:,:)
    real, parameter :: tolerance = 1e-7
    logical :: ok = .true.
  
    conv1d_layer = conv(filters, kernel_size)
  
    if (.not. conv1d_layer % name == 'conv1d') then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer has its name set correctly.. failed'
    end if
  
    if (conv1d_layer % initialized) then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer should not be marked as initialized yet.. failed'
    end if
  
    if (.not. conv1d_layer % activation == 'relu') then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer defaults to relu activation.. failed'
    end if
  
    input_layer = input(3, 32)
    call conv1d_layer % init(input_layer)
  
    if (.not. conv1d_layer % initialized) then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer should now be marked as initialized.. failed'
    end if
  
    if (.not. all(conv1d_layer % input_layer_shape == [3, 32])) then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer input layer shape should be correct.. failed'
    end if
  
    if (.not. all(conv1d_layer % layer_shape == [filters, 30])) then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer input layer shape should be correct.. failed'
    end if
  
    ! Minimal conv1d layer: 1 channel, 3x3 pixel image;
    allocate(sample_input(1, 3))
    sample_input = 0
  
    input_layer = input(1, 3)
    conv1d_layer = conv(filters, kernel_size)
    call conv1d_layer % init(input_layer)
  
    select type(this_layer => input_layer % p); type is(input2d_layer)
      call this_layer % set(sample_input)
    end select
  
    call conv1d_layer % forward(input_layer)
    call conv1d_layer % get_output(output)
  
    if (.not. all(abs(output) < tolerance)) then
      ok = .false.
      write(stderr, '(a)') 'conv1d layer with zero input and sigmoid function must forward to all 0.5.. failed'
    end if
  
    if (ok) then
      print '(a)', 'test_conv1d_layer: All tests passed.'
    else
      write(stderr, '(a)') 'test_conv1d_layer: One or more tests failed.'
      stop 1
    end if
  
end program test_conv1d_layer
