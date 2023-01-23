grammar Expression1;

expr [acc=Int] returns [res=Int]
: t=term[acc] { val tVal = t.res!! } e=expr_[tVal] { expr.res = e.res }
;

expr_ [acc=Int] returns [res=Int]
: PLUS t=term[acc] { val tmp = context.add(acc, t.res!!) } e_=expr_[tmp] { expr_.res = e_.res }
| { expr_.res = acc }
;

term [acc=Int] returns [res=Int]
: f=fl[acc] { val fVal = f.res!! } t_=term_[fVal] { term.res = t_.res }
;

term_ [acc=Int] returns [res=Int]
: MULT f=fl[acc] { val tmp = acc * f.res!! } t_=term_[tmp] { term_.res = t_.res }
| { term_.res = acc }
;

fl [acc=Int] returns [res=Int]
: number=NUMBER { fl.res = number.text.toInt() }
| LBRACKET e=expr[acc] RBRACKET { fl.res = e.res }
;

PLUS : '\\+';
MULT : '\\*';
NUMBER : '[0-9]+';
LBRACKET : '\\(';
RBRACKET : '\\)';
WS : '[ \t\r\n]+' -> skip;