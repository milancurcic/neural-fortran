submodule(nf_reshape_layer_generalized) nf_reshape_layer_generalized_submodule

  use nf_base_layer, only: base_layer

  implicit none

contains

  pure module function reshape_layer_cons(output_shape) result(res)
    integer, intent(in) :: output_shape(:)
    type(reshape_generalized_layer) :: res
    allocate(res % output_shape(size(output_shape)))
    res % output_shape = output_shape
  end function reshape_layer_cons

  pure module subroutine backward(self, input, gradient)
    class(reshape_generalized_layer), intent(in out) :: self
    real, intent(in) :: input(:)
    real, intent(in) :: gradient(..)  ! Assumed-rank gradient

    ! Handle different ranks of gradient using SELECT RANK
    select rank (gradient)
      rank default
        error stop "Unsupported gradient rank in reshape layer"
      rank (0)
        self % gradient = [gradient]
      rank (1)
        self % gradient = gradient
      rank (2)
        self % gradient = reshape(gradient, [size(gradient)])
      rank (3)
        self % gradient = reshape(gradient, [size(gradient)])
    end select

  end subroutine backward

  pure module subroutine forward(self, input)
    class(reshape_generalized_layer), intent(in out) :: self
    real, intent(in) :: input(:)
    integer :: i
  
    ! Ensure output is allocated
    if (.not. allocated(self % output)) then
        allocate(self % output(size(input)))  ! Flattened storage
    end if

    ! Copy elements manually (assuming Fortran column-major order)
    do i = 1, size(input)
        self % output(i) = input(i)
    end do
  end subroutine forward

  module subroutine init(self, input_shape)
    class(reshape_generalized_layer), intent(in out) :: self
    integer, intent(in) :: input_shape(:)
    
    self % input_shape = input_shape
    
    ! Allocate gradient buffer based on input size
    allocate(self % gradient(product(input_shape)))
    self % gradient = 0
    
    ! Allocate output buffer based on output_shape
    allocate(self % output(product(self % output_shape)))
    self % output = 0
  
  end subroutine init

end submodule nf_reshape_layer_generalized_submodule