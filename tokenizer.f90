
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
    integer :: i

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    
    if ("fizz" == input(cursor:cursor + 3)) then !Foo
        allocate(character(len=4) :: lexeme)
        lexeme = input(cursor:cursor + 3)
        cursor = cursor + 4
        return
    end if
        

    if ("buzz" == input(cursor:cursor + 3)) then !Foo
        allocate(character(len=4) :: lexeme)
        lexeme = input(cursor:cursor + 3)
        cursor = cursor + 4
        return
    end if
        

    if ("hueco" == input(cursor:cursor + 4)) then !Foo
        allocate(character(len=5) :: lexeme)
        lexeme = input(cursor:cursor + 4)
        cursor = cursor + 5
        return
    end if
        

i = cursor


if (input(i:i) >= "0" .and. input(i:i) <= "9") then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if
    
    

    if (to_lower("es Gei") == to_lower(input(cursor:cursor + 5))) then !Foo
        allocate(character(len=6) :: lexeme)
        lexeme = input(cursor:cursor + 5)
        cursor = cursor + 6
        return
    end if
        

i = cursor


if (input(i:i) >= "A" .and. input(i:i) <= "z") then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if
    
    

    if ("joshua" == input(cursor:cursor + 5)) then !Foo
        allocate(character(len=6) :: lexeme)
        lexeme = input(cursor:cursor + 5)
        cursor = cursor + 6
        return
    end if
        

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        