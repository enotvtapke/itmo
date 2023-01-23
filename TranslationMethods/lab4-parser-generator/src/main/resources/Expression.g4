grammar Expression;

expr [acc=BigDecimal] returns [res=BigDecimal]
: t=term[acc] { val tVal = t.res!! } e=expr_[tVal] { expr.res = e.res }
;

expr_ [acc=BigDecimal] returns [res=BigDecimal]
: PLUS t=term[acc] { val tmp = context.add(acc, t.res!!) } e_=expr_[tmp] { expr_.res = e_.res }
| { expr_.res = acc }
;

term [acc=BigDecimal] returns [res=BigDecimal]
: number=NUMBER { term.res = number.text.toBigDecimal() }
| LBRACKET e=expr[acc] RBRACKET { term.res = e.res }
;

PLUS : '\\+';
NUMBER : '(\\+|-|)[0-9]+(.[0-9]+)?';
LBRACKET : '\\(';
RBRACKET : '\\)';
WS : '[ \t\r\n]+' -> skip;