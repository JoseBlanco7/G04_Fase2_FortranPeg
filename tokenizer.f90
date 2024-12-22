
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

    
	if ("xd" == input(cursor:cursor + 1)) then 
	    allocate(character(len=2) :: lexeme)
	    allocate(character(len=2) :: entrada_anterior)
	    lexeme = "xd"
	    entrada_anterior = lexeme
	    cursor = cursor + 2 
		if ("probando" == input(cursor:cursor + 7)) then 
	    	deallocate(lexeme)
	    	allocate(character(len=10) :: lexeme)
	    	lexeme = entrada_anterior // input(cursor:cursor + 7)
	    	deallocate(entrada_anterior)
	    	entrada_anterior = lexeme
	    	cursor = cursor + 8
			if (to_lower("compi") == to_lower(input(cursor:cursor + 4))) then 
	    		deallocate(lexeme)
	    		allocate(character(len=15) :: lexeme)
	    		lexeme = entrada_anterior // input(cursor:cursor + 4)
	    		lexeme = lexeme // " - " // "EstaEsUnaPrueba"
	    		deallocate(entrada_anterior)
	    		entrada_anterior = lexeme
	    		cursor = cursor + 5
				return
			end if
		end if
	end if

    if (to_lower("hola") == to_lower(input(cursor:cursor + 3))) then !Foo
        allocate(character(len=4) :: lexeme)
        lexeme = input(cursor:cursor + 3)
        lexeme = lexeme // " - " // "EstaEsUnaPrueba"
        cursor = cursor + 4
        return
    end if
            

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        