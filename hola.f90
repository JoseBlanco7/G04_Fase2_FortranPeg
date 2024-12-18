program main

    implicit none

    ! Declaración de variables
    character(len=100) :: buffer  ! Cadena temporal de tamaño fijo
    character(len=:), allocatable :: entrada
    integer :: i

    print *, "Escribe algo: "
    read(*, '(A)') buffer  ! Leer la entrada en una cadena fija

    ! ! Asignar el tamaño exacto del contenido leído a la cadena allocatable
    entrada = trim(buffer)

    ! print *, "Hola ", entrada
    ! Recorrer la cadena y mostrar los caracteres uno a uno
    do i = 1, len(entrada)
        
        ! if ( entrada(i:i) >= '0' .and. entrada(i:i) <= '9' ) then
        !     print *, "Integer"
        ! elseif ( entrada(i:i) == "+" .or. entrada(i:i) == "*" .or. entrada(i:i) == "-" .or. entrada(i:i) == "/") then
        !     print *, "Operador"
        ! elseif ( entrada(i:i) == " " ) then
        !     print *, "Espacio"
        ! else
        !     print *, "Error Lexico"
        ! end if
    end do

end program main
