
module parser
implicit none

contains    

subroutine parse(input)
    character(len=:), intent(inout), allocatable :: input
    integer :: cursor
    character(len=:), allocatable :: lexeme  
    cursor = 1
    do
        lexeme = nextsym(input, cursor)
        if (lexeme == "EOF") then
            print *, lexeme
            exit   
        end if
        if (lexeme == "ERROR") then
            print *, lexeme
        else
            print *, lexeme
        end if
    end do
end subroutine parse


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
        allocate(character(len=3) :: lexeme)
        lexeme = "EOF"
        return
    end if

    
	if ("prueba" == input(cursor:cursor + 5)) then 
	    allocate(character(len=6) :: lexeme)
	    allocate(character(len=6) :: entrada_anterior)
	    lexeme = "prueba"
	    entrada_anterior = lexeme
	    cursor = cursor + 6 
    		if (input(cursor:cursor) >= '0' .and. input(cursor:cursor) <= '9') then
    	    	deallocate(lexeme)
    	    	allocate(character(len=1) :: lexeme)
    	    	lexeme = entrada_anterior // input(cursor:cursor)
    	    	lexeme = lexeme // " - " // "hola"
    	    	deallocate(entrada_anterior)
    	    	entrada_anterior = lexeme
    	    	cursor = cursor + 1
			return
		end if
	end if

	if (to_lower("xd") == to_lower(input(cursor:cursor + 1))) then 
	    allocate(character(len=2) :: lexeme)
	    allocate(character(len=2) :: entrada_anterior)
	    lexeme = input(cursor:cursor + 1)
	    entrada_anterior = lexeme
	    cursor = cursor + 2 
    		if (input(cursor:cursor) >= 'w' .and. input(cursor:cursor) <= 'z') then
    	    	deallocate(lexeme)
    	    	allocate(character(len=1) :: lexeme)
    	    	lexeme = entrada_anterior // input(cursor:cursor)
    	    	lexeme = lexeme // " - " // "hola"
    	    	deallocate(entrada_anterior)
    	    	entrada_anterior = lexeme
    	    	cursor = cursor + 1
			return
		end if
	end if

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    cursor = cursor + 1  ! Incrementa el cursor para evitar el bucle infinito
    lexeme = "ERROR"

end function nextSym
end module parser
        