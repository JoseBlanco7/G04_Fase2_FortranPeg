
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
    character(len=:), allocatable :: entrada_anterior
    integer :: i

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    
    if (input(cursor:cursor) >= '1' .and. input(cursor:cursor) <= '2') then
        allocate(character(len=1) :: lexeme)  ! Reservar espacio para el lexema
        lexeme = input(cursor:cursor)        ! Asignar el carácter al lexema
        lexeme = lexeme // " - " // "hola"
        cursor = cursor + 1                  ! Avanzar el cursor
        return
    end if
                

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        