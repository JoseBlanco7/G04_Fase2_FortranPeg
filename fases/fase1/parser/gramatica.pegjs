{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'
}}

gramatica = _ producciones+ _ {

    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
}

producciones = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? { 
    ids.push(id) 
    return new n.Rule(id, expr, alias);
    }

opciones = head:union tail:(_ "/" _ @union)* {
    return new n.Choice([head, ...tail]);
}

union = head:expresion tail:(_ @expresion !(_ literales? _ "=") )* {
    return new n.Concatenation([head, ...tail]);
}

expresion  = etiqueta:(etiqueta/varios)? _ expr:expresiones _ mod:([?+*]/conteo)? {
    return new n.Expression(expr, mod, etiqueta);
}

etiqueta = pluck:("@")? _ id:identificador _ ":" varios:(varios)? {
    return new n.Label(id, pluck ? true : false, varios);
}

varios = ("!"/"$"/"@"/"&")

expresiones  =  id:identificador { 
    usos.push(id);
    return new n.Expression(id);
    }
                / lit:literales "i"? {
                    return new n.Expression(lit)
                }
                / "(" _ opciones:opciones _ ")" {
                    return new n.Expression(opciones)
                }
                / corchetes:corchetes "i"? {
                    return new n.Expression(corchetes)
                }
                / "." {
                    return new n.Expression(".")
                }
                / "!." {
                    return new n.Expression("!.")
                }

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo = "|" _ valor:(numero / id:identificador) _ "|" {
            return new n.Quantifier(valor);
}
        / "|" _ valor1:(numero / id:identificador)? _ modo:".." _ valor2:(numero / id2:identificador)? _ "|" {
            return new n.Quantifier(valor1,modo,valor2);
        }
        / "|" _ valor1:(numero / id:identificador)? _ modo:"," _ valor2:opciones _ "|" {
            return new n.Quantifier(valor1,modo,valor2);
        }
        / "|" _ valor1:(numero / id:identificador)? _ modo:".." _ valor2:(numero / id2:identificador)? _ modo2:"," _ valor3:opciones _ "|" {
            return new n.Quantifier(valor1,modo,valor2,modo2,valor3);
        }

// parteconteo = identificador
//             / [0-9]? _ ".." _ [0-9]?
// 			/ [0-9]

// delimitador =  "," _ expresion

// Regla principal que analiza corchetes con contenido
corchetes
    = "[" contenido:(rango / contenido)+ "]" {
        console.log(`Entrada válida: [${input}]`);
        return new n.Corchetes(contenido);
    }

// Regla para validar un rango como [A-Z]
rango
    = inicio:caracter "-" fin:caracter {
        if (inicio.charCodeAt(0) > fin.charCodeAt(0)) {
            throw new Error(`Rango inválido: [${inicio}-${fin}]`);

        }
        return `${inicio}-${fin}`;
    }

// Regla para caracteres individuales
caracter
    = [a-zA-Z0-9_ ] { return text()}

// Coincide con cualquier contenido que no incluya "]"
contenido
    = (corchete / texto)+

corchete
    = "[" contenido "]"

texto
    = [^\[\]]+

literales = '"' stringDobleComilla* '"'
            / "'" stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    

numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }


_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
