
// Auto-generated
import Node from './Node.js';

export class Producciones extends Node {
    constructor(id, expr, alias) {
        super();
        this.id = id;
		this.expr = expr;
		this.alias = alias;
    }

    accept(visitor) {
        return visitor.visitProducciones(this);
    }
}
    
export class Opciones extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitOpciones(this);
    }
}
    
export class Union extends Node {
    constructor(exprs) {
        super();
        this.exprs = exprs;
    }

    accept(visitor) {
        return visitor.visitUnion(this);
    }
}
    
export class Expresion extends Node {
    constructor(expr, label, qty) {
        super();
        this.expr = expr;
		this.label = label;
		this.qty = qty;
    }

    accept(visitor) {
        return visitor.visitExpresion(this);
    }
}
    
export class Corchetes extends Node {
    constructor(contenido, isCase) {
        super();
        this.contenido = contenido;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitCorchetes(this);
    }
}
    
export class String extends Node {
    constructor(val, isCase) {
        super();
        this.val = val;
		this.isCase = isCase;
    }

    accept(visitor) {
        return visitor.visitString(this);
    }
}
    