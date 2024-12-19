
module tokenizer
    implicit none

contains
function nextSym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    
    if ("hola" == input(cursor:cursor + 3)) then !Foo
        allocate( character(len=4) :: lexeme)
        lexeme = input(cursor:cursor + 3)
        cursor = cursor + 4
        return
    end if
    

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        