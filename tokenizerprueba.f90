
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
    
        
    if ("prueba" == input(cursor:cursor + 5)) then 
        allocate(character(len=6) :: lexeme)
        allocate(character(len=6) :: entrada_anterior)
        lexeme = "prueba"
        entrada_anterior = lexeme
        cursor = cursor + 6 
        if (to_lower("xd") == to_lower(input(cursor:cursor + 1))) then 
            deallocate(lexeme)
            allocate(character(len=8) :: lexeme)
            lexeme = entrada_anterior // input(cursor:cursor + 1)
            deallocate(entrada_anterior)
            entrada_anterior = lexeme
            cursor = cursor + 2
            if ("hola" == input(cursor:cursor + 3)) then 
                deallocate(lexeme)
                allocate(character(len=12) :: lexeme)
                lexeme = entrada_anterior // input(cursor:cursor + 3)
                deallocate(entrada_anterior)
                entrada_anterior = lexeme
                cursor = cursor + 4
                return
                end if
    end if
    end if
    
        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        lexeme = "ERROR"
    end function nextSym
    end module tokenizer 
            