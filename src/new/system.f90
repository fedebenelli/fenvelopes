module fenvelopes__system
    use yaeos, only: pr, ArModel
    integer :: nc
    real(pr), allocatable :: z(:)
    class(ArModel), allocatable :: model
end module