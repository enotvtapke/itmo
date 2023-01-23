grammar Python;

@parser::header {
    import java.util.*;
    import java.lang.*;
    import java.util.stream.Collectors;
}

@parser::members {
    public StringBuilder translated = new StringBuilder();
    Map<String, String> memory = new HashMap<String, String>();
}

file returns [String res]
: stats EOF
{
String varDecls = memory.entrySet().stream().map(it -> it.getValue() + " " + it.getKey() + ";\n").collect(Collectors.joining());
translated.append(String.format("""
#include <stdio.h>
#include <stdbool.h>
%s
int main() {
%s
    return 0;
}
""", varDecls, $stats.res));
}
;

stat returns [String res]
: ID '=' 'int' '(' 'input' '(' ')' ')'
{
$res = "scanf(\"%d\", &" + $ID.text + ");\n";
memory.put($ID.text, "int");
}
| 'print' '(' expr ')' { $res = "printf(\"%d\\n\", " + $expr.res + ");\n"; }
| 'if' expr ':' NEWLINE block { $res = "if (" + $expr.res + ")" + $block.res; }
| 'elif' expr ':' NEWLINE block { $res = "else if (" + $expr.res + ")" + $block.res; }
| 'else' ':' NEWLINE block { $res = "else" + $block.res; }
| 'while' expr ':' NEWLINE block { $res = "while (" + $expr.res + ")" + $block.res; }
| assign NEWLINE { $res = $assign.res + ";\n"; }
| expr NEWLINE { $res = $expr.res + ";\n"; }
| NEWLINE
;

stats returns [String res]
: (sts+=stat)*
{
$res = $sts.stream().filter(it -> it.res != null)
    .map(it -> it.res).collect(Collectors.joining());
}
;

block returns [String res]
:
| INDENT stats DEDENT { $res = "{\n    " + $stats.res.replace(";\n", ";\n    ") + "}\n"; }
;

assign returns [String res]
: ID '=' expr
{
$res = $ID.text + "=" + $expr.res;
memory.put($ID.text, $expr.type);
}
;

expr returns [String res, String type]
: a=expr '//' b=expr { $res = $a.res + "/" + $b.res; $type = "int"; }
| a=expr '*' b=expr { $res = $a.res + "*" + $b.res; $type = "int"; }
| a=expr op=('+'|'-') b=expr { $res = $a.res + $op.text + $b.res; $type = "int"; }
| 'not' expr { $res = "!" + $expr.res; $type = "bool"; }
| a=expr 'and' b=expr { $res = $a.res + "&&" + $b.res; $type = "bool"; }
| a=expr 'or' b=expr { $res = $a.res + "||" + $b.res; $type = "bool"; }
| a=expr op=('<'|'>'|'==') b=expr { $res = $a.res + $op.text + $b.res; $type = "bool"; }
| '(' expr ')' { $res = $expr.res; $type = $expr.type; }
| TRUE { $res = "true"; $type = "bool"; }
| FALSE { $res = "false"; $type = "bool"; }
| ID { $res = $ID.text; $type = memory.get($ID.text); }
| INT { $res = $INT.text; $type = "int"; }
;

TRUE : 'True' ;
FALSE : 'False' ;
INT : [0-9]+ ;
ID : [a-zA-Z_] [a-zA-Z_0-9]* ;
NEWLINE
    : '\r'? '\n'
    ;
INDENT : '{' ;
DEDENT : '}' ;
WS : [ \t\r\n]+ -> skip ;
