import Visitor from './Visitor.js';
import { Rango } from './CST.js';

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
    integer :: i

    if (cursor > len(input)) then
        allocate( character(len=3) :: lexeme )
        lexeme = "EOF"
        return
    end if

    ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    lexeme = "ERROR"
end function nextSym
end module tokenizer 
        `;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        return node.exprs.map((node) => node.accept(this)).join('\n');
    }
    visitUnion(node) {
        return node.exprs.map((node) => node.accept(this)).join('\n');
    }
    visitExpresion(node) {
        return node.expr.accept(this);
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

generateCaracteres(chars) {
    if (chars.length === 0) return '';
    return `
if (findloc([${chars
    .map((char) => `"${char}"`)
    .join(', ')}], input(i:i), 1) > 0) then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if
    `;
}

visitClase(node) {
    let result =
     `
i = cursor
${this.generateCaracteres(
    node.chars.filter((node) => typeof node === 'string')
)}
${node.chars
    .filter((node) => node instanceof Rango)
    .map((range) => range.accept(this))
    .join('\n')}
    `;

    //console.log("*******************************");
    //console.log(result);
    return result;
}

visitRango(node) {
    
    console.log("----Trabajando con rangos");
    console.log(node.listOfRanges);

    /*
    do i = 1, len(str)
            
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if

    end do
    */
    /*
    do while (i <= n )
        suma = suma + i
        i = i + 1
        end do
*/
    let result = 
    `
if (input(i:i) >= 1 .and. input(i:i) <= 2) then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if
    `;
    //console.log("------------------------");
    //console.log(result);
    return result;
}
}


