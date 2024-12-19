
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

        
    if (to_lower("prueba") == to_lower(input(cursor:cursor + 5))) then !Foo
        allocate(character(len=6) :: lexeme)
        lexeme = input(cursor:cursor + 5)
        cursor = cursor + 6
        return
    end if
        

    if ("123" == input(cursor:cursor + 2)) then !Foo
        allocate(character(len=3) :: lexeme)
        lexeme = input(cursor:cursor + 2)
        cursor = cursor + 3
        return
    end if
        

    if ("hola" == input(cursor:cursor + 3)) then !Foo
        allocate(character(len=4) :: lexeme)
        lexeme = input(cursor:cursor + 3)
        cursor = cursor + 4
        return
    end if
        

        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        allocate(character(len=5) :: lexeme)
        lexeme = "ERROR"
        cursor = cursor + 1 ! Avanzar el cursor para evitar el bucle infinito
    end function nextSym

end module tokenizer
        