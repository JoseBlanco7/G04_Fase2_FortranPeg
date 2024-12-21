import Visitor from './Visitor.js';
import { Rango,String } from './CST.js';

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
           let fortran = "";
        if (node.exprs.length == 1){
            console.log(node.exprs);
            if (node.exprs[0].expr.isCase == 'i') {
                // Comparación insensible a mayúsculas y minúsculas
        return `
        if (to_lower("${node.exprs[0].expr.val}") == to_lower(input(cursor:cursor + ${node.exprs[0].expr.val.length - 1}))) then !Foo
            allocate(character(len=${node.exprs[0].expr.val.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.exprs[0].expr.val.length - 1})
            cursor = cursor + ${node.exprs[0].expr.val.length}
            return
        end if
            `;
            }
        return `
        if ("${node.exprs[0].expr.val}" == input(cursor:cursor + ${node.exprs[0].expr.val.length - 1})) then !Foo
            allocate(character(len=${node.exprs[0].expr.val.length}) :: lexeme)
            lexeme = input(cursor:cursor + ${node.exprs[0].expr.val.length - 1})
            cursor = cursor + ${node.exprs[0].expr.val.length}
            return
        end if`
            
        }
        let repeticiones = 0;
        let total_nodos = node.exprs.length;
        let longitud = 0;
        let lexema_anterior = "";
        // console.log(node.exprs.length);
        node.exprs.forEach(element => {
            const tabuladores = "\t".repeat(repeticiones);
            if (element.expr instanceof String){
                // Case insensitive
                if (element.expr.isCase == 'i') {
                    console.log("entra")
                    if (repeticiones == 0){
                        fortran += `
    ${tabuladores}if (to_lower("${element.expr.val}") == to_lower(input(cursor:cursor + ${element.expr.val.length - 1}))) then 
        ${tabuladores}allocate(character(len=${element.expr.val.length}) :: lexeme)
        ${tabuladores}lexeme = "${element.expr.val}"
        ${tabuladores}cursor = cursor + ${element.expr.val.length} `
                    repeticiones++;
                    lexema_anterior = element.expr.val;
                    longitud = element.expr.val.length;
                    } else if(repeticiones == total_nodos - 1){
                        longitud += element.expr.val.length;
                        fortran += `
    ${tabuladores}if (to_lower("${element.expr.val}") == to_lower(input(cursor:cursor + ${element.expr.val.length - 1}))) then 
        ${tabuladores}deallocate(lexeme)
        ${tabuladores}allocate(character(len=${longitud}) :: lexeme)
        ${tabuladores}lexeme= "${lexema_anterior}" // "${element.expr.val}"
        ${tabuladores}cursor = cursor + ${element.expr.val.length}`
                        lexema_anterior += element.expr.val;
                    } else {
                        longitud += element.expr.val.length;
                        fortran += `
    ${tabuladores}if (to_lower("${element.expr.val}") == to_lower(input(cursor:cursor + ${element.expr.val.length - 1}))) then 
        ${tabuladores}deallocate(lexeme)
        ${tabuladores}allocate(character(len=${longitud}) :: lexeme)
        ${tabuladores}lexeme = "${lexema_anterior}" // "${element.expr.val}"
        ${tabuladores}cursor = cursor + ${element.expr.val.length}`
                        lexema_anterior += element.expr.val;
                repeticiones++;
                    } 
                } else {
                if (repeticiones == 0){
                    fortran += `
${tabuladores}if ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then 
    ${tabuladores}allocate(character(len=${element.expr.val.length}) :: lexeme)
    ${tabuladores}lexeme = "${element.expr.val}"
    ${tabuladores}cursor = cursor + ${element.expr.val.length} `
                repeticiones++;
                lexema_anterior = element.expr.val;
                longitud = element.expr.val.length;
                } else if(repeticiones == total_nodos - 1){
                    longitud += element.expr.val.length;
                    fortran += `
${tabuladores}if ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then 
    ${tabuladores}deallocate(lexeme)
    ${tabuladores}allocate(character(len=${longitud}) :: lexeme)
    ${tabuladores}lexeme= "${lexema_anterior}" // "${element.expr.val}"
    ${tabuladores}cursor = cursor + ${element.expr.val.length}`
                    lexema_anterior += element.expr.val;
                } else {
                    longitud += element.expr.val.length;
                    fortran += `
${tabuladores}if ("${element.expr.val}" == input(cursor:cursor + ${element.expr.val.length - 1})) then 
    ${tabuladores}deallocate(lexeme)
    ${tabuladores}allocate(character(len=${longitud}) :: lexeme)
    ${tabuladores}lexeme = "${lexema_anterior}" // "${element.expr.val}"
    ${tabuladores}cursor = cursor + ${element.expr.val.length}`
                    lexema_anterior += element.expr.val;
            repeticiones++;
                } 
            }}
        });
        for (let i = repeticiones; i > 0; i--) {
            let tabuladores = "\t".repeat(i);
            if (i == repeticiones){
                fortran += `
${tabuladores}\treturn
${tabuladores}end if`;
            }else{
            fortran += `
${tabuladores}end if`;
            }
        }
        let tabuladores = "\t".repeat(repeticiones);
                fortran += `
end if`;       
        console.log(fortran);
        return fortran;
    }
    visitExpresion(node) {
        return node.expr.accept(this);
    }   
    
    
    
    
    


visitString(node) {
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
    return `
i = cursor
${this.generateCaracteres(
    node.chars.filter((node) => typeof node === 'string')
)}
${node.chars
    .filter((node) => node instanceof Rango)
    .map((range) => range.accept(this))
    .join('\n')}
    `;
}

visitRango(node) {
    return `
if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if
    `;
}
}


