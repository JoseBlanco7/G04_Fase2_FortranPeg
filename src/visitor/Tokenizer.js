import Visitor from './Visitor.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
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

        ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        allocate(character(len=5) :: lexeme)
        lexeme = "ERROR"
        cursor = cursor + 1 ! Avanzar el cursor para evitar el bucle infinito
    end function nextSym

end module tokenizer
        `;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }

    visitOpciones(node) {
        // Generar código para todas las opciones
        return node.exprs.map(expr => expr.accept(this)).join('\n');
    }

    visitUnion(node) {
        // Generar código para todas las uniones
        return node.exprs.map(expr => expr.accept(this)).join('\n');
    }

    visitExpresion(node) {
        return node.expr.accept(this);
    }

    visitCorchetes(node) {
        console.log("Prueba");
        console.log(node.isCase);
        console.log(node);
    
        const rangeRegex = /^([a-zA-Z0-9])\-([a-zA-Z0-9])$/;
    
        const match = node.contenido.match(rangeRegex);
    
        if (match) {
            const startChar = match[1];
            const endChar = match[2];
    
            // Si es sensible a mayúsculas
            if (node.isCase !== 'i') {
                return `
                if (input(cursor:cursor + 0) < "${startChar}" .or. input(cursor:cursor + 0) > "${endChar}") then
                    print*, "Error: El carácter no está en el rango permitido."
                    stop
                end if
                allocate(character(len=1) :: lexeme)
                lexeme = input(cursor:cursor + 0)
                cursor = cursor + 1
                return
                `;
            } else {
                // Si es insensible a mayúsculas
                return `
                if (to_lower(input(cursor:cursor + 0)) < to_lower("${startChar}") .or. to_lower(input(cursor:cursor + 0)) > to_lower("${endChar}")) then
                    print*, "Error: El carácter no está en el rango permitido."
                    stop
                end if
                allocate(character(len=1) :: lexeme)
                lexeme = input(cursor:cursor + 0)
                cursor = cursor + 1
                return
                `;
            }
        }
    }
    
    
    
    


visitString(node) {
    console.log("Prueba");
    console.log(node.isCase);
    console.log(node);
    if (node.isCase == 'i') {
        console.log("Prueba2");
        // Comparación insensible a mayúsculas y minúsculas
        return `
    if (to_lower("${node.val}") == to_lower(input(cursor:cursor + ${node.val.length - 1}))) then !Foo
        allocate(character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if
        `;
    } else {
        // Comparación sensible a mayúsculas y minúsculas
        return `
    if ("${node.val}" == input(cursor:cursor + ${node.val.length - 1})) then !Foo
        allocate(character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if
        `;
    }
}

}
