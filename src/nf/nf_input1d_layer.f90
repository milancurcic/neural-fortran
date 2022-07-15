module nf_input1d_layer

  !! This module provides the `input1d_layer` type.

  use nf_base_layer, only: base_layer

  implicit none

  private
  public :: input1d_layer

  type, extends(base_layer) :: input1d_layer

    real, allocatable :: output(:)
    real, allocatable :: batch_output(:,:)

  contains

    procedure :: init

    procedure, private :: set_sample
    procedure, private :: set_batch

    generic :: set => set_sample, set_batch

  end type input1d_layer

  interface input1d_layer
    pure module function input1d_layer_cons(output_size) result(res)
      !! Create a new instance of the 1-d input layer.
      !! Only used internally by the `layer % init` method.
      integer, intent(in) :: output_size
        !! Size of the input layer
      type(input1d_layer) :: res
        !! 1-d input layer instance
    end function input1d_layer_cons
  end interface input1d_layer

  interface set

    pure module subroutine set_sample(self, values)
      !! Set values of one 1-d sample on this layer.
      class(input1d_layer), intent(in out) :: self
        !! Layer instance
      real, intent(in) :: values(:)
        !! Values to set
    end subroutine set_sample

    pure module subroutine set_batch(self, values)
      !! Set values of a batch of 1-d samples on this layer.
      class(input1d_layer), intent(in out) :: self
        !! Layer instance
      real, intent(in) :: values(:,:)
        !! Values to set; last dimension is the batch
    end subroutine set_batch

  end interface set

  interface

    module subroutine init(self, input_shape)
      !! Only here to satisfy the language rules
      !! about deferred methods of abstract types.
      !! This method does nothing for this type and should not be called.
      class(input1d_layer), intent(in out) :: self
      integer, intent(in) :: input_shape(:)
    end subroutine init

  end interface

end module nf_input1d_layer
