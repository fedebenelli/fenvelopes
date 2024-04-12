module io_nml
   !! Namelist I/O Module
   !!
   !! Module that holds all the I/O routines to read from a setup file
   !! and setup the models and parameters included included in the thermodynamic
   !! routines
   !!
   !! ```fortran
   !!
   !!  ! Namelist based input file
   !!  ! =========================
   !!  !
   !!  ! Units:
   !!  !  - Pressure: bar
   !!  !  - Temperature: K
   !!  !  - Volume: L
   !!  ! =========================
   !!  
   !!  
   !!  &nml_setup
   !!      nc=3,                    ! Number of components
   !!      model="PR78",            ! SRK PR76 PR78 RKPR
   !!      mixrule="ClassicVdW"     ! ClassicVdW
   !!  /
   !!  
   !!  &nml_composition
   !!      names="C4" "C20" "H2O"
   !!      spec="critical"          ! critical or parameters
   !!      z=0.16 0.04 0.8
   !!  /
   !!  
   !!  &nml_classicvdw
   !!      kij(1, :)=0   0   0.5
   !!      kij(2, :)=0   0   0.5
   !!      kij(3, :)=0.5 0.5 0
   !!  /
   !!  
   !!  &nml_critical
   !!      ! Critical constants
   !!      tc=425.2 782.0 647.3
   !!      pc=38.0 14.6 220.89
   !!      w=0.1928 0.816 0.344
   !!  /
   !!
   use constants, only: pr
   use io, only: str
   implicit none
   
   integer :: nunit_input
   character(len=50) :: model, mixrule
   character(len=254) :: path_to_file
   character(len=50) :: spec
   character(len=50), allocatable :: names(:)

   private

   public :: read_system

contains
   
   subroutine read_system(filepath, nc, z, syst)
      use yaeos, only: ArModel, PengRobinson76, PengRobinson78, SoaveRedlichKwong
      integer, intent(out) :: nc !! Number of components
      real(pr), allocatable :: z(:) !! System composition
      class(ArModel), allocatable :: syst !! System/model
      character(len=*) :: filepath

      ! Critical constants
      real(pr), allocatable :: tc(:), pc(:), w(:)
      ! Combining parameters
      real(pr), allocatable :: kij(:, :), lij(:, :)

      namelist /nml_setup/ nc, model, mixrule
      namelist /nml_critical/ tc, pc, w
      namelist /nml_composition/ names, z, spec
      namelist /nml_classicvdw/ kij, lij

      open (newunit=nunit_input, file=trim(adjustl(filepath)))
         read (nunit_input, nml=nml_setup)
         allocate(names(nc), z(nc), tc(nc), pc(nc), w(nc), kij(nc, nc), lij(nc, nc))

         ! Read general composition
         read(nunit_input, nml=nml_composition); rewind(nunit_input)

         ! Read critical parameters
         read(nunit_input, nml=nml_critical); rewind(nunit_input)
        
         ! Read mixing rule parameters
         select case (mixrule)
         case("ClassicVdW")
            kij = 0
            lij = 0
            read(nunit_input, nml=nml_classicvdw)
         end select
         rewind(nunit_input)
         
         ! Setup model
         select case(model)
         case("SRK")
            syst = SoaveRedlichKwong(tc, pc, w, kij, lij)
         case("PR76")
            syst = PengRobinson76(tc, pc, w, kij, lij)
         case("PR78")
            syst = PengRobinson78(tc, pc, w, kij, lij)
         end select
         rewind(nunit_input)
      close (nunit_input)
   end subroutine

   ! subroutine write_system(file_unit)
   !    ! Write read system into a file
   !    integer, intent(in), optional :: file_unit
   !    integer :: i

   !    ! if (.not. present(file_unit)) file_unit=

   !    character(len=20) :: fmt_pure
   !    character(len=20) :: fmt_names
   !    fmt_pure = "(xG,"//adjustl(trim(str(nc)))//"F10.4)"
   !    fmt_names = "(8x, "//adjustl(trim(str(nc)))//"(A8))"

   !    write (file_unit, *) "====================="
   !    write (file_unit, *) "General System: "
   !    write (file_unit, *) "---------------------"
   !    write (file_unit, *) "Model: ", model
   !    write (file_unit, *) "MixingRule: ", mixrule
   !    write (file_unit, *) "Names: ", (trim(names(i))//" ", i=1, nc)
   !    write (file_unit, "(xA,2x,*(F8.6, 2x))") "Z:", z

   !    write (file_unit, *) "===================="
   !    write (file_unit, *) "Critical Constants: "
   !    write (file_unit, *) "--------------------"
   !    write (file_unit, fmt_pure) "Tc: ", tc
   !    write (file_unit, fmt_pure) "Pc: ", pc
   !    write (file_unit, fmt_pure) "w : ", w
   !    write (file_unit, *) "===================="
   !    write (file_unit, *) "EoS Parameters: "
   !    write (file_unit, *) "--------------------"
   !    write (file_unit, fmt_pure) "ac:", ac
   !    write (file_unit, fmt_pure) "b :", b
   !    write (file_unit, fmt_pure) "k :", k

   !    write (file_unit, *) "===================="
   !    write(file_unit, *), "Mixing Rules: "
   !    write(file_unit, *), "--------------------"

   !    write(file_unit, *) "Kij: "

   !    write(file_unit, fmt_names) names
   !    do i = 1, nc
   !       write (file_unit, fmt_pure) str(i), kij(i, :)
   !    end do
   !    write(file_unit, *) "lij: "
   !    write(file_unit, fmt_names) names
   !    do i = 1, nc
   !       write (file_unit, fmt_pure) str(i), lij(i, :)
   !    end do
   ! end subroutine
end module
