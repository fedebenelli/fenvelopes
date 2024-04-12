program main
    use constants, only: pr
    use yaeos, only: ArModel, CubicEoS
    use io_nml, only: read_system

    class(ArModel), allocatable :: chisito
    integer :: nc
    real(pr), allocatable :: z(:)

    call read_system("infiles/case2.nml", nc, z, chisito)

    select type(chisito)
    class is(CubicEoS)
        print *, chisito%components%tc
    end select
end program