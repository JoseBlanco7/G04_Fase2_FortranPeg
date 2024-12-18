{{
  class AstTree {
   /* type; //node //sheet
    name;
    lexema;
    pos;
    init;
    end;
    rule;
    fortranCode;
    children;*/
    constructor(_type,name,lexema) {
          this.type 		= 	_type; //node //sheet
          this.name 		= 	name;
          if(this.type=="node")
          	this.rule = lexema;
          else
          	this.lexema 		= 	lexema;
          
          /*this.pos 			= 	0;
          this.init 		= 	0;
          this.end 			= 	0;
          this.rule 		= 	0;
          this.fortranCode 	= 	"";*/
          this.children = new Array();
    }
    addChild(child){
    	this.children.push(child);
    }
    addFortranCode(_code){
    	this.code= _code; 
    }
  }
}}
gramatica = wh1:_ produc:producciones+ wh2:_  { 
                                     let gram = new AstTree("node","gramatica","_ producciones+ _");
                                      gram.addChild(wh1);
                                      gram.addChild(produc);
                                      gram.addChild(wh2);
                                      console.log(gram);
                                      return gram;
                                      
                                  }

producciones = wh1:_ id:identificador wh2:_ lit:(literales)? wh3:_ "=" wh4:_ opc:opciones wh5:(_";")? { 
                                      let prod = new AstTree("node","producciones","_ identificador _ (literales)? _ = _ opciones (_;)?");
                                      prod.addChild(wh1);
                                      prod.addChild(id);
                                      prod.addChild(wh2);
                                      prod.addChild(lit);
                                      prod.addChild(wh3);
                                      prod.addChild(new AstTree("sheet","producciones","="));
                                      prod.addChild(wh4);
                                      prod.addChild(opc);
                                      prod.addChild(wh5);
                                      return prod;
                                  }
opciones = u:union u2:(_ "/" _ union)*{ 
                                      let opc = new AstTree("node","opciones","union (_ "/" _ union)*");
                                      opc.addChild(u);
                                      opc.addChild(u2);
                                      return opc;
                                  }

union = ex:expresion opt:(_ expresion !(_ literales? _ "=") )*{ 
                                      let uni = new AstTree("node","union","expresion (_ expresion !(_ literales? _ =) )*");
                                      uni.addChild(ex);
                                      uni.addChild(opt);
                                      return uni;
                                  }

expresion  = firstV:(etiqueta/varios)? wh1:_ ex:expresiones wh2:_ cnt:([?+*]/conteo)?{ 
                                      let expr = new AstTree("node","expresion","(etiqueta/varios)? _ expresiones _ ([?+*]/conteo)?");
                                      expr.addChild(firstV);
                                      expr.addChild(wh1);
                                      expr.addChild(ex);
                                      expr.addChild(wh2);
                                      if(typeof(cnt)=="string")
                                      	expr.addChild(new AstTree("sheet","expresion",cnt));
                                      else
                                      	expr.addChild(cnt);
                                      
                                      return expr;
                                  }

etiqueta = arr:("@")? wh1:_ id:identificador wh2:_ ":" vari:(varios)?{ 
                                      let eti = new AstTree("node","etiqueta","(@)? _ identificador _ : (varios)?");
                                      eti.addChild(new AstTree("node","etiqueta",arr));
                                      eti.addChild(wh1);
                                      eti.addChild(id);
                                      eti.addChild(wh2);
                                      eti.addChild(new AstTree("node","etiqueta",":"));
                                      eti.addChild(vari);
                                      return eti;
                                  }

varios = value:("!"/"$"/"@"/"&") { 
                                      let vari = new AstTree("node","varios","valor");
                                      vari.addChild(new AstTree("sheet","varios",value));
                                      return vari;
                                  }

expresiones  =  id:identificador  {  let exp = new AstTree("node","expresiones","identificador");
                                         exp.addChild(id);
                                         return exp;
                                  }
                / lits:literales caseS:"i"? {  let exp = new AstTree("node","expresiones","literales i?");
                                                   exp.addChild(lits);
                                                   exp.addChild(new AstTree("sheet","expresiones",caseS));
                                                   return exp;
                                            }
                / "(" wh1:_ opc:opciones wh2:_ ")"{  let exp = new AstTree("node","expresiones","( _ opciones _ )");
                                                   exp.addChild(new AstTree("sheet","expresiones","("));
                                                   exp.addChild(wh1);
                                                   exp.addChild(opc);
                                                   exp.addChild(wh2);
                                                   exp.addChild(new AstTree("sheet","expresiones",")"));
                                                   return exp;
                                            }
                / cor:corchetes caseS:"i"?{  let exp = new AstTree("node","expresiones"," corchetes i?");
                                                   exp.addChild(cor);
                                                   exp.addChild(new AstTree("sheet","expresiones",caseS));
                                                   return exp;
                                            }
                / "." {  let exp = new AstTree("node","expresiones"," .");
                                                   exp.addChild(new AstTree("sheet","expresiones","."));
                                                   return exp;
                                            }
                / "!."{  let exp = new AstTree("node","expresiones","!.");
                                                   exp.addChild(cor);
                                                   exp.addChild(new AstTree("sheet","expresiones","!."));
                                                   return exp;
                                            }

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo = "|" wh1:_ res:(numero / identificador) wh2:_ "|"  { let cht = new AstTree("node","conteo","| _ (num:numero / id:identificador) _ |" );
                                        cht.addChild(new AstTree("sheet","conteo","|"));
                                        cht.addChild(wh1);
                                        cht.addChild(res);
                                        cht.addChild(wh2);
                                        cht.addChild(new AstTree("sheet","conteo","|"));
                                        return cht;
        	   						  }
        / "|" wh1:_ res:(numero / identificador)? wh2:_ ".." wh3:_ res2:(numero / identificador)? wh4:_ "|"{ let cnto = new AstTree("node","conteo","| _ (num:numero / id:identificador)? _ .. _ (numero / id2:identificador)? _ |" );
                                        cnto.addChild(new AstTree("sheet","conteo","|"));
                                        cnto.addChild(wh1);
                                        cnto.addChild(res);
                                        cnto.addChild(wh2);
                                        cnto.addChild(new AstTree("sheet","conteo",".."));
                                        cnto.addChild(wh3);
                                        cnto.addChild(res2);
                                        cnto.addChild(wh4);
                                        cnto.addChild(new AstTree("sheet","conteo","|"));
                                        return cnto;
        	   						  }
        / "|"wh1:_ res:(numero / identificador)? wh2:_ "," wh3:_ opc:opciones wh4:_ "|"{ let cnto = new AstTree("node","conteo","|wh1:_ res:(numero / identificador)? wh2:_ , wh3:_ opc:opciones wh4:_ |" );
                                        cnto.addChild(new AstTree("sheet","conteo","|"));
                                        cnto.addChild(wh1);
                                        cnto.addChild(res);
                                        cnto.addChild(wh2);
                                        cnto.addChild(new AstTree("sheet","conteo",","));
                                        cnto.addChild(wh3);
                                        cnto.addChild(opc);
                                        cnto.addChild(wh4);
                                        cnto.addChild(new AstTree("sheet","conteo","|"));
                                        return cnto;
        	   						  }
        / "|" wh1:_ res:(numero / identificador)? wh2:_ ".." wh3:_ res2:(numero / identificador)? wh4:_ "," wh5:_ opc:opciones wh6:_ "|"{ let cnto = new AstTree("node","conteo","| _ (num:numero / id:identificador)? _ .. _ (numero / id2:identificador)? _ |" );
                                        cnto.addChild(new AstTree("sheet","conteo","|"));
                                        cnto.addChild(wh1);
                                        cnto.addChild(res);
                                        cnto.addChild(wh2);
                                        cnto.addChild(new AstTree("sheet","conteo",".."));
                                        cnto.addChild(wh3);
                                        cnto.addChild(res2);
                                        cnto.addChild(wh4);
                                        cnto.addChild(new AstTree("sheet","conteo",","));
                                        cnto.addChild(wh5);
                                        cnto.addChild(opc);
                                        cnto.addChild(wh6);
                                        cnto.addChild(new AstTree("sheet","conteo","|"));
                                        return cnto;
        	   						  }

// parteconteo = identificador
//             / [0-9]? _ ".." _ [0-9]?
// 			/ [0-9]

// delimitador =  "," _ expresion

// Regla principal que analiza corchetes con contenido
corchetes
    = "[" contenido:(rango / contenido)+ "]" { let cht = new AstTree("node","corchetes","[ contenido:(rango / contenido)+ ]" );
                                        cht.addChild(new AstTree("sheet","corchetes","["));
                                        cht.addChild(contenido);
                                        cht.addChild(new AstTree("sheet","corchetes","]"));
                                        return cht;
        	   						  }

// Regla para validar un rango como [A-Z]
rango
    = inicio:caracter "-" fin:caracter{ let rng = new AstTree("node","rango","inicio:caracter - fin:caracter");
                                        rng.addChild(inicio);
                                        rng.addChild(new AstTree("sheet","rango","-"));
                                        rng.addChild(fin);
                                        return rng;
        	   						  }

// Regla para caracteres individuales
caracter
    = [a-zA-Z0-9_ ] { let ch = new AstTree("node","caracter","[a-zA-Z0-9_ ]");
                 		ch.addChild(new AstTree("sheet","caracter",text()));
                 		return ch;
        	   		}

// Coincide con cualquier contenido que no incluya "]"
contenido
    = co:(corchete / texto)+ {  let cont = new AstTree("node","contenido","(corchete / texto)+");
                          		cont.addChild(co);
                           		return cont;
        	   			}

corchete
    = "[" cnt:contenido "]" { let cor = new AstTree("node","corchete","[ contenido ]");
                           cor.addChild(new AstTree("sheet","corchete","["));
                           cor.addChild(cnt);
                           cor.addChild(new AstTree("sheet","corchete","]"));
                           return cor;
        	   			}

texto
    = [^\[\]]+ { let txt = new AstTree("node","texto","[^\[\]]+");
                 txt.addChild(new AstTree("sheet","texto",txtC));
                 return txt;
        	   }

literales = '"' ssC:stringDobleComilla* '"'{ 
                                let lit = new AstTree("node","literales","\" stringDobleComilla* \"");
                                lit.addChild(new AstTree("sheet","literales","\""));
                                lit.addChild(new AstTree("sheet","literales",ssC));
                                lit.addChild(new AstTree("sheet","literales","\""));
                                return lit;
        					}
            / "'" ssC:stringSimpleComilla* "'" { 
                                let lit = new AstTree("node","literales","' stringSimpleComilla* '");
                                lit.addChild(new AstTree("sheet","literales","'"));
                                lit.addChild(new AstTree("sheet","literales",ssC));
                                lit.addChild(new AstTree("sheet","literales","'"));
                                return lit;
        					}

stringDobleComilla = !('"' / "\\" / finLinea) .{
											return text();
                                    }
                    / "\\" esc:escape{ 
                               return text();
        					}
                    / cnl:continuacionLinea{ 
                               return text();
        					}

stringSimpleComilla = !("'" / "\\" / finLinea) .{ 
                               return text();
        					}
                    / "\\" esc:escape { 
                                return text();
        					}
                    / cnl:continuacionLinea{ 
                                return text();
        					}

continuacionLinea = "\\" val:secuenciaFinLinea { 
                                let cl = new AstTree("node","continuacionLinea","\\ secuenciaFinLinea");
                                cl.addChild(new AstTree("sheet","continuacionLinea","\\"));
                                cl.addChild(val);
                                return cl;
        					}

finLinea = [\n\r\u2028\u2029]{ 
                                let fl = new AstTree("node","finlinea"," [\n\r\u2028\u2029]");
                                fl.addChild(new AstTree("sheet","finlinea",text()));
                                return fl;
        					}

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
        { 
            let esc = new AstTree("node","escape","'/ ''/ \\/ b/ f/ n/ r/ t/ v/ u");
            esc.addChild(new AstTree("sheet","escape",text()));
            return esc;
        }

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029" { 
	let sec = new AstTree("node","secuenciaFinLinea","\r\n / \n / \r / \u2028 / \u2029");
    sec.addChild(new AstTree("sheet","secuenciaFinLinea",text()));
    return sec;
}

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    

numero = [0-9]+ { 
	let num = new AstTree("node","numero","valor");
    num.addChild(new AstTree("sheet","numero",text()));
    return num;
}

identificador =[_a-z]i[_a-z0-9]i* { 
                                    let identi = new AstTree("node","identificador","[_a-z]i[_a-z0-9]i*");
                                    identi.addChild(new AstTree("sheet","identificador",text()));
                                    return identi;
								  }


_ = valFin:(Comentarios/[ \t\n\r])* {
                                            let undrscr = new AstTree("node","_","_");
											undrscr.addChild(new AstTree("sheet","_",valFin));
											return undrscr;	
									}


Comentarios = 
    "//" [^\n]*  { 	return text();
   				 }
    / "/*" (!"*/" .)* "*/" { return text();
   						  }
