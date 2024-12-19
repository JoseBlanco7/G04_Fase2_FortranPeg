
module tokenizer
    implicit none

contains

    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end function to_lower

    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme

        if (cursor > len(input)) then
            allocate(character(len=3) :: lexeme)
            lexeme = "EOF"
            return
        end if

        
                if (input(cursor:cursor + 0) < "4" .or. input(cursor:cursor + 0) > "6") then
                    print*, "Error: El carácter no está en el rango permitido."
                    stop
                end if
                allocate(character(len=1) :: lexeme)
                lexeme = input(cursor:cursor + 0)
                cursor = cursor + 1
                return
                

        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        allocate(character(len=5) :: lexeme)
        lexeme = "ERROR"
        cursor = cursor + 1 ! Avanzar el cursor para evitar el bucle infinito
    end function nextSym

end module tokenizer
        