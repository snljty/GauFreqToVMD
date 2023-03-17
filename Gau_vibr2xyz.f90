program main
    implicit none

    integer :: argc
    character(len=256) :: argv0
    character(len=64), dimension(:), allocatable :: argv
    integer :: iarg

    logical, external :: check_whether_linear

    integer, parameter :: stdin_unit = 5, stdout_unit = 6, stderr_unit = 0
    integer, parameter :: exit_success = 0, exit_failure = 1

    integer(kind=4), parameter :: max_element_number = 118
    character(kind=1,len=2), parameter :: element_name(max_element_number) = &
        (/   "H ", "He", "Li", "Be", "B ", "C ", "N ", &
             "O ", "F ", "Ne", "Na", "Mg", "Al", "Si", &
             "P ", "S ", "Cl", "Ar", "K ", "Ca", "Sc", &
             "Ti", "V ", "Cr", "Mn", "Fe", "Co", "Ni", &
             "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", &
             "Kr", "Rb", "Sr", "Y ", "Zr", "Nb", "Mo", &
             "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", &
             "Sn", "Sb", "Te", "I ", "Xe", "Cs", "Ba", &
             "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", &
             "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", &
             "Lu", "Hf", "Ta", "W ", "Re", "Os", "Ir", &
             "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", &
             "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", &
             "U ", "Np", "Pu", "Am", "Cm", "Bk", "Cf", &
             "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", &
             "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", &
             "Nh", "Fl", "Mc", "Lv", "Ts", "Og" /)
    integer, parameter :: num_dims = 3

    character(len=256) :: ifl_name, ofl_name
    integer :: ifl_unit, ofl_unit
    integer :: status

    character(len=256) :: line
    integer :: char_pos
    character(len=20), parameter :: coordinates_locator = "Standard orientation"

    integer :: num_atoms
    integer, dimension(:), allocatable :: atom_index
    double precision, dimension(:, :), allocatable :: atom_coords
    double precision, dimension(:), allocatable :: freq
    double precision, dimension(:, :, :), allocatable :: vibr_coords
    logical :: is_linear
    integer :: num_vibrs
    integer :: index_center, atom_other_info

    integer :: index_mode, index_mode_last_this_line
    integer :: num_intervals
    integer, parameter :: num_intervals_default = 20
    double precision :: coeff
    double precision :: move_scale
    double precision, parameter :: move_scale_default = 1.d0

    integer :: i ! mostly used as the index of atom, sometimes just a counter of lines
    integer :: index_interval

    ! acquire command arguments
    call get_command_argument(0, value = argv0)
    argc = command_argument_count()
    allocate(argv(argc))
    do iarg = 1, argc
        call get_command_argument(iarg, value = argv(iarg))
    end do

    ! if argv(1) provided, use it as ifl_name
    ifl_name = ""
    if (argc >= 1) then
        ifl_name = argv(1)
    end if

    ! release memory of command arguments
    deallocate(argv)

    ! get input file name
    if (ifl_name /= "") then
        char_pos = index(ifl_name, ".", back = .true.)
        if (char_pos /= 0) then
            line(:) = ifl_name(char_pos:)
        else
            line(:) = ""
        end if
        if (line /= ".out" .and. line /= ".log") then
            write(stderr_unit, '(a, a, a)') &
                "Error! The suffix of the input file should be "".out"" or "".log"", but got """, trim(line), """."
        else
            open(newunit = ifl_unit, file = ifl_name, status = "old", action = "read", iostat = status)
            if (status == 0) then
                goto 100
            end if
            write(stderr_unit, '(a, a, a)') "Error! Cannot Open """, trim(ifl_name), """ for reading."
        end if
    end if
    do while (.true.)
        write(*, '(a)') "Input Gaussian freq task output file name:"
        read(*, '(a)') ifl_name
        char_pos = index(ifl_name, ".", back = .true.)
        if (char_pos /= 0) then
            line(:) = ifl_name(char_pos:)
        else
            line(:) = ""
        end if
        if (line /= ".out" .and. line /= ".log") then
            write(stderr_unit, '(a, a, a)') &
                "Error! The suffix of the input file should be "".out"" or "".log"", but got """, trim(line), """."
            cycle
        end if
        open(newunit = ifl_unit, file = ifl_name, status = "old", action = "read", iostat = status)
        if (status == 0) then
            exit
        end if
        write(stderr_unit, '(a, a, a)') "Error! Cannot Open """, trim(ifl_name), """ for reading."
    end do
100 continue

    ! check whether this is a Gaussian output file
    do while (.true.)
        read(ifl_unit, '(a)', iostat = status) line
        if (status /= 0) then
            write(stderr_unit, "(a)") "Error! This file does not seem to be a Gaussian output file."
            close(ifl_unit)
            stop exit_failure
        end if
        if (index(line, "Gaussian") /= 0) then
            exit
        end if
    end do

    ! get number of atoms
    do while (.true.)
        read(ifl_unit, '(a)') line
        char_pos = index(line, "NAtoms=")
        if (char_pos /= 0) then
            read(line(char_pos + len("NAtoms="):), *) num_atoms
            exit
        end if
    end do
    allocate(atom_index(num_atoms))
    allocate(atom_coords(num_dims, num_atoms))

    ! get coordinates of the last frame
    do while (.true.)
        read(ifl_unit, '(a)', iostat = status) line
        if (status /= 0) then
            exit
        end if
        if (index(line, coordinates_locator) /= 0) then
            do i = 1, 4
                read(ifl_unit, '(a)') line
            end do
            do i = 1, num_atoms
                read(ifl_unit, '(a)') line
                read(line, *) index_center, atom_index(i), atom_other_info, atom_coords(:, i)
            end do
        end if
    end do
    rewind(ifl_unit)

    ! test
    ! open(new_unit = ofl_unit, file = "tmp.xyz", status = "replace", action = "write")
    ! write(ofl_unit, '(i0)') num_atoms
    ! write(ofl_unit, '(a, 1x, i0)') "Mode", index_mode
    ! do i = 1, num_atoms
    !     write(ofl_unit, '(2x, a2, 3(7x,f11.6))') element_name(atom_index(i)),  coords(:, i)
    ! end do
    ! close(ofl_unit)

    ! check whether the molecule is linear
    is_linear = check_whether_linear(num_atoms, atom_index, atom_coords)
    if (is_linear) then
        num_vibrs = 3 * num_atoms - 5
    else
        num_vibrs = 3 * num_atoms - 6
    end if
    allocate(freq(num_vibrs), vibr_coords(num_dims, num_atoms, num_vibrs))

    ! read normal coordinates
    do while(.true.)
        read(ifl_unit, '(a)', iostat = status) line
        if (status /= 0) then
            write(stderr_unit, '(a)') "Error! This file does not seem to be a freq task output."
            close(ifl_unit)
            deallocate(atom_index, atom_coords, vibr_coords)
            stop exit_failure
        end if
        if (index(line, "and normal coordinates") /= 0) then
            exit
        end if
    end do
    do index_mode = 1, num_vibrs, 3
        index_mode_last_this_line = index_mode + 2
        if (index_mode_last_this_line > num_vibrs) then
            index_mode_last_this_line = num_vibrs
        end if
        do while (.true.)
            read(ifl_unit, '(a)') line
            char_pos = index(line, "Frequencies --")
            if (char_pos /= 0) then
                read(line(char_pos + len("Frequencies --"):), *) freq(index_mode:index_mode_last_this_line)
            end if
            if (index(line, "Atom  AN") /= 0) then
                exit
            end if
        end do
        do i = 1, num_atoms
            read(ifl_unit, '(a)') line
            read(line, *) index_center, atom_other_info, vibr_coords(:, i, index_mode:index_mode_last_this_line)
        end do
    end do
    close(ifl_unit)

    ! test
    ! open(new_unit = ofl_unit, file = "tmp2.xyz", status = "replace", action = "write")
    ! do index_mode = 1, num_vibrs
    !     write(ofl_unit, '(i0)') num_atoms
    !     write(ofl_unit, '(a, 1x, i0)') "Mode", index_mode
    !     do i = 1, num_atoms
    !         write(ofl_unit, '(1x, a2, 3(2x,f5.2))') element_name(atom_index(i)),  vibr_coords(:, i, index_mode)
    !     end do
    ! end do
    ! close(ofl_unit)

    ! get index of the interested normal mode
    write(*, '(a, a, a)') "Loaded file """, trim(ifl_name), """ successfully."
    write(*, '(a, i0, a)') "There are total ", num_atoms, " atoms."
    if (is_linear) then
        write(*, '(a)') "This is a linear molecule."
    else
        write(*, '(a)') "This is a non-linear molecule."
    end if
    write(*, '(a, i0, a)') "There are total ", num_vibrs, " normal modes."
    write(*, '()')
    do while (.true.)
        write(*, '(a)') "Input the index of normal modes:"
        write(*, '(a)') "Input a blank line to exit."
        read(*, '(a)') line
        if (line == "") then
            exit
        end if
        read(line, *) index_mode
        if (index_mode <= 0 .or. index_mode > num_vibrs) then
            write(stderr_unit, '(a, i0)') "Error! The index should be between 1 and ", num_vibrs
            cycle
        end if
        line(:) = ifl_name
        char_pos = index(line, ".", back = .true.)
        if (char_pos /= 0) then
            line(char_pos:) = ""
        end if
        write(ofl_name, '(a, a, i4.4, a)') trim(line), "_mode", index_mode, ".xyz"

        ! get the number of intervals on each sides
        do while (.true.)
            write(*, '(a)') "Input the number of intervals on each sides."
            write(*, '(a, i0, a)') "Press <Enter> directly to use the default value ", num_intervals_default, "."
            read(*, '(a)') line
            if (line == "") then
                num_intervals = num_intervals_default
                exit
            else
                read(line, *, iostat = status) num_intervals
                if (status == 0 .and. num_intervals > 0) then
                    exit
                end if
            end if        
            write(stderr_unit, '(a)') "Error! The number of intervals must be positive."
        end do

        ! get the scale factor of the normal mode
        do while (.true.)
            write(*, '(a)') "Input a scale factor of the amplitude."
            write(*, '(a, f0.1, a)') "Press <Enter> directly to use the default value ", move_scale_default, "."
            read(*, '(a)') line
            if (line == "") then
                move_scale = move_scale_default
                exit
            else
                read(line, *, iostat = status) move_scale
                if (status == 0 .and. move_scale > 0.d0) then
                    exit
                end if
            end if
            write(stderr_unit, '(a)') "Error! The scale factor of the amplitude must be positive."
        end do

        ! write multi-frame structures along vibration path
        open(newunit = ofl_unit, file = ofl_name, status = "replace", action = "write")
        do index_interval = 0, num_intervals
            coeff = real(index_interval, kind = 8) / real(num_intervals, kind = 8)
            write(ofl_unit, '(i0)') num_atoms
            write(ofl_unit, '(2x, a, f10.4, a, 1x, a, i3, a, i2)') "frequency = ", freq(index_mode), " cm**-1", &
                "position ", index_interval, "/", num_intervals
            do i = 1, num_atoms
                write(ofl_unit, '(2x, a2, 3(7x,f11.6))') &
                    element_name(atom_index(i)),  atom_coords(:, i) + vibr_coords(:, i, index_mode) * coeff * move_scale
            end do
        end do
        do index_interval = num_intervals - 1, - num_intervals, - 1
            coeff = real(index_interval, kind = 8) / real(num_intervals, kind = 8)
            write(ofl_unit, '(i0)') num_atoms
            write(ofl_unit, '(2x, a, f10.4, a, 1x, a, i3, a, i2)') "frequency = ", freq(index_mode), " cm**-1", &
                "position ", index_interval, "/", num_intervals
            do i = 1, num_atoms
                write(ofl_unit, '(2x, a2, 3(7x,f11.6))') &
                    element_name(atom_index(i)),  atom_coords(:, i) + vibr_coords(:, i, index_mode) * coeff * move_scale
            end do
        end do
        do index_interval = - num_intervals + 1, - 1
            coeff = real(index_interval, kind = 8) / real(num_intervals, kind = 8)
            write(ofl_unit, '(i0)') num_atoms
            write(ofl_unit, '(2x, a, f10.4, a, 1x, a, i3, a, i2)') "frequency = ", freq(index_mode), " cm**-1", &
                "position ", index_interval, "/", num_intervals
            do i = 1, num_atoms
                write(ofl_unit, '(2x, a2, 3(7x,f11.6))') &
                    element_name(atom_index(i)),  atom_coords(:, i) + vibr_coords(:, i, index_mode) * coeff * move_scale
            end do
        end do
        close(ofl_unit)
        write(*, '(a, a, a)') "Done. File """, trim(ofl_name), """ written."
        write(*, '()')
    end do

    ! release memory
    deallocate(atom_index, atom_coords, freq, vibr_coords)
    write(*, '(a)') "Exiting program."
    if (argc == 0) then
        write(*, '(a)') "Press <Enter> to exit."
        read(*, '(a)') line
    end if

    stop
end program main

logical function check_whether_linear(num_atoms, atom_index, atom_coords)
    implicit none
    integer, intent(in) :: num_atoms
    integer, parameter :: num_dims = 3
    integer, dimension(num_atoms), intent(in) :: atom_index
    double precision, dimension(num_dims, num_atoms), intent(in) :: atom_coords

    ! use atomic order instead of atomic mass for making the program easier
    ! mi is "moments of inertia"
    double precision, dimension(num_dims, num_dims) :: mi_tensor
    double precision, dimension(num_dims) :: mi_principle_axes
    integer :: i, info, lwork
    double precision, dimension(:), allocatable :: work
    double precision, parameter :: tol = 1.d-6

    mi_tensor = 0.d0
    do i = 1, num_atoms
        mi_tensor(1, 1) = mi_tensor(1, 1) + atom_index(i) * (atom_coords(2, i) ** 2 + atom_coords(3, i) ** 2)
        mi_tensor(1, 2) = mi_tensor(1, 2) - atom_index(i) * atom_coords(1, i) * atom_coords(2, i)
        mi_tensor(1, 3) = mi_tensor(1, 3) - atom_index(i) * atom_coords(1, i) * atom_coords(3, i)
        mi_tensor(2, 1) = mi_tensor(2, 1) - atom_index(i) * atom_coords(2, i) * atom_coords(1, i)
        mi_tensor(2, 2) = mi_tensor(2, 2) + atom_index(i) * (atom_coords(3, i) ** 2 + atom_coords(1, i) ** 2)
        mi_tensor(2, 3) = mi_tensor(2, 3) - atom_index(i) * atom_coords(2, i) * atom_coords(3, i)
        mi_tensor(3, 1) = mi_tensor(3, 1) - atom_index(i) * atom_coords(3, i) * atom_coords(1, i)
        mi_tensor(3, 2) = mi_tensor(3, 2) - atom_index(i) * atom_coords(3, i) * atom_coords(2, i)
        mi_tensor(3, 3) = mi_tensor(3, 3) + atom_index(i) * (atom_coords(1, i) ** 2 + atom_coords(2, i) ** 2)
    end do
    ! mi_tensor will be destroyed
    allocate(work(1))
    lwork = -1
    call dsyev('N', 'L', num_dims, mi_tensor, num_dims, mi_principle_axes, work, lwork, info)
    lwork = nint(work(1))
    deallocate(work)
    allocate(work(lwork))
    call dsyev('N', 'L', num_dims, mi_tensor, num_dims, mi_principle_axes, work, lwork, info)
    deallocate(work)
    ! mi_principle_axes must be positive defined
    if (mi_principle_axes(1) <= tol .and. mi_principle_axes(3) - mi_principle_axes(2) <= tol) then
        check_whether_linear = .true.
    else
        check_whether_linear = .false.
    end if

    return
end function check_whether_linear

