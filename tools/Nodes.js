const nodes = {
    Producciones: ['id', 'expr', 'alias'],
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresion: ['expr', 'label', 'qty'],
    Corchetes: ['contenido', 'isCase'],
    String: ['val', 'isCase'],
};

export default nodes;
